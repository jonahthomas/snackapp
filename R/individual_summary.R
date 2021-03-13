#' individual_summary
#'
#' @param folder_path A path to the folder which contains the SnackApp usage data. The file should only contain csv files.
#' Defaults to a folder in the current work directory named "data".
#' @param csv A logical variable that decides whether a csv output file is created.
#' @param r_object A logical variable that decides whether an R object is produced and saved in the global environment.
#' @param output_path A path to a folder where the csv summary file(s) will be written. Defaults to a folder in the current
#' work directory named "summary".
#'
#' @return A dataframe is generated to summarise each participants usage data.
#' @export
#'
#' @examples

individual_summary <- function(folder_path = file.path(getwd(), "data"), csv = FALSE, r_object = TRUE, output_path = file.path(getwd(), "summary")) {

  # take file path and extract file names within the folder

  folder_path %>%
    list.files() %>%
    .[stringr::str_detect(., "csv")] -> file_names

  # loop through each file name and read the csv in, then manipulate the data, assign this dataframe to df_list

  file_names %>%
    purrr::map(function(file_names) { # iterate through each file name
      utils::read.csv(file.path(folder_path, file_names), stringsAsFactors = FALSE) %>%
        dplyr::mutate(
          id = as.numeric(substr(file_names, 20, 21)), # use file name as a unique id column for each dataframe
          date = as.POSIXct(Date), # convert date from chr to POSIXct format
          year = as.numeric(format(date, "%Y")),
          month = as.numeric(format(date, "%m")), # create month and day column
          day = as.numeric(format(date, "%d"))
        ) %>%
        dplyr::relocate(id, date, month, day) %>%
        dplyr::select(-Date) -> data

      data <- as.data.frame(data)

      file_id <- data$id[1]

      data %>%
        dplyr::filter(Event == "app-state-changed") %>%
        dplyr::mutate(
          diff = lubridate::as.duration(date - dplyr::lag(date))
        ) %>%
        dplyr::filter(Metric == "background") %>%
        dplyr::group_by(id, year, month, day) %>%
        dplyr::summarise(
          total_time = sum(diff),
          average_time = mean(diff, na.rm = TRUE),
          std_dev_time = stats::sd(diff, na.rm = TRUE),
          minimum_time = min(diff, na.rm = TRUE),
          maximum_time = max(diff, na.rm = TRUE)
        ) -> state_change_summary

      # create app element flagging columns and calcualte time difference

      data <- data %>%
        dplyr::mutate(
          lag_date = dplyr::lag(date),
          my_stat = dplyr::if_else(stringr::str_detect(Metric, "my-stat"), 1, 0),
          my_stat = dplyr::lag(my_stat, 1),
          stat_diff = dplyr::if_else(my_stat == 1, as.numeric(date - lag_date), 0),
          resources = dplyr::if_else(stringr::str_detect(Metric, "resources") | stringr::str_detect(Metric, "your-health") | stringr::str_detect(Metric, "faq") | stringr::str_detect(Metric, "forum") & !stringr::str_detect(Metric, "fitbit"), 1, 0),
          resources = dplyr::lag(resources, 1),
          resource_diff = dplyr::if_else(resources == 1, as.numeric(date - lag_date), 0),
          my_goals = dplyr::if_else(stringr::str_detect(Metric, "my-goal"), 1, 0),
          my_goals = dplyr::lag(my_goals, 1),
          goal_diff = dplyr::if_else(my_goals == 1, as.numeric(date - lag_date), 0),
          notifications = dplyr::if_else(stringr::str_detect(Event, "notification"), 1, 0)
        )

      # summarise the usage of difference data for each individual app element

      data %>%
        dplyr::group_by(id, year, month, day) %>%
        dplyr::summarise(
          total_stat = sum(stat_diff, na.rm = TRUE),
          total_resource = sum(resource_diff, na.rm = TRUE),
          total_goal = sum(goal_diff, na.rm = TRUE),
          average_stat = mean(stat_diff, na.rm = TRUE),
          average_resource = mean(resource_diff, na.rm = TRUE),
          average_goal = mean(goal_diff, na.rm = TRUE),
          stdev_goal = stats::sd(stat_diff, na.rm = TRUE),
          stdev_resources = stats::sd(resource_diff, na.rm = TRUE),
          stdev_goal = stats::sd(goal_diff, na.rm = TRUE),
          min_stat = min(stat_diff[which(stat_diff > 0)]),
          min_resource = min(resource_diff[which(resource_diff > 0)]),
          min_goal = min(goal_diff[which(goal_diff > 0)]),
          max_stat = max(stat_diff, na.rm = TRUE),
          max_resource = max(resource_diff, na.rm = TRUE),
          max_goal = max(goal_diff, na.rm = TRUE),
          notificaition_count = sum(notifications)
        ) -> summary

      # due to minimum not including zero, coerces inf so convert these back to zeros

      is.na(summary) <- sapply(summary, is.infinite)
      summary[is.na(summary)] <- 0

      # bind dataframes from state change and summary using inner join

      summary <- dplyr::inner_join(state_change_summary, summary, keep = FALSE) %>%
        round(digits = 2) %>%
        dplyr::mutate(
          date = lubridate::make_date(year, month, day)
        ) %>%
        dplyr::relocate(id, date, year, month, day)

      # write the total summary data back to a csv

      if (csv == TRUE) {
        utils::write.csv(summary, file = file.path(output_path, "/", "individual_summary.csv"))
      }
      if (r_object == TRUE) {
        assign(paste(file_id, "individual_summary", sep = "_"), summary, envir = globalenv())
      }
    })
}
