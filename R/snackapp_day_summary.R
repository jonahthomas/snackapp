#' snackapp_day_summary
#'
#' @param folder_path
#' @param csv
#' @param r_object
#' @param output_path
#'
#' @return
#' @export
#'
#' @examples
snackapp_day_summary <- function(folder_path, csv = FALSE, r_object = TRUE, output_path = getwd()) {

  # take file path and extract file names within the folder

  folder_path %>%
    list.files() %>%
    .[str_detect(., "csv")] -> file_names

  # loop through each file name and read the csv in, then manipulate the data, assign this dataframe to df_list

  file_names %>%
    purrr::map(function(file_names) { # iterate through each file name
      read.csv(paste(folder_path, file_names, sep = "/"), stringsAsFactors = FALSE) %>%
        mutate(
          id = as.numeric(substr(file_names, 20, 21)), # use file name as a unique id column for each dataframe
          date = as.POSIXct(Date), # convert date from chr to POSIXct format
          year = as.numeric(format(date, "%Y")),
          month = as.numeric(format(date, "%m")), # create month and day column
          day = as.numeric(format(date, "%d"))
        ) %>%
        relocate(id, date, month, day) %>%
        select(-Date)
    }) -> df_list # Assign to a list

  # bind the rows of all the dataframes in the list into one dataframe

  bind_rows(df_list) -> data

  # calculate summary date for the total duration of app usage

  data %>%
    filter(Event == "app-state-changed") %>%
    mutate(
      diff = as.duration(date - lag(date))
    ) %>%
    filter(Metric == "background") %>%
    group_by(id, year, month, day) %>%
    summarise(
      total_time = sum(diff),
      average_time = mean(diff, na.rm = TRUE),
      std_dev_time = sd(diff, na.rm = TRUE),
      minimum_time = min(diff, na.rm = TRUE),
      maximum_time = max(diff, na.rm = TRUE)
    ) -> state_change_summary

  # create app element flagging columns and calcualte time difference

  data <- data %>%
    mutate(
      lag_date = lag(date),
      my_stat = if_else(str_detect(Metric, "my-stat"), 1, 0),
      my_stat = lag(my_stat, 1),
      stat_diff = if_else(my_stat == 1, as.numeric(date - lag_date), 0),
      resources = if_else(str_detect(Metric, "resources") | str_detect(Metric, "your-health") | str_detect(Metric, "faq") | str_detect(Metric, "forum") & !str_detect(Metric, "fitbit"), 1, 0),
      resources = lag(resources, 1),
      resource_diff = if_else(resources == 1, as.numeric(date - lag_date), 0),
      my_goals = if_else(str_detect(Metric, "my-goal"), 1, 0),
      my_goals = lag(my_goals, 1),
      goal_diff = if_else(my_goals == 1, as.numeric(date - lag_date), 0),
      notifications = if_else(str_detect(Event, "notification"), 1, 0)
    )

  # summarise the usage of difference data for each individual app element

  data %>%
    group_by(id, year, month, day) %>%
    summarise(
      total_stat = sum(stat_diff, na.rm = TRUE),
      total_resource = sum(resource_diff, na.rm = TRUE),
      total_goal = sum(goal_diff, na.rm = TRUE),
      average_stat = mean(stat_diff, na.rm = TRUE),
      average_resource = mean(resource_diff, na.rm = TRUE),
      average_goal = mean(goal_diff, na.rm = TRUE),
      stdev_goal = sd(stat_diff, na.rm = TRUE),
      stdev_resources = sd(resource_diff, na.rm = TRUE),
      stdev_goal = sd(goal_diff, na.rm = TRUE),
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

  summary <- inner_join(state_change_summary, summary, keep = FALSE) %>%
    round(digits = 2) %>%
    mutate(
      date = make_date(year, month, day)
    ) %>%
    relocate(id, date, year, month, day)

  # write the total summary data back to a csv

  if (csv == TRUE) {
    write.csv(summary, file = paste(output_path, "/", "summary.csv", sep = ""))
  }
  if (r_object == TRUE) {
    snackapp_analytics <<- summary
  }
}
