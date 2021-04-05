#' individual_summary
#'
#' Creating a separate dataframe for each participant for which data has been collected. A list of tibbles are generated while
#' separate .csv files can be produced and written to file.
#'
#' @param data The SnackApp usage data that is to be analysed. This parameter accepts the output of the load_data function.
#' It also accepts a dataframe with 6 columns: id, Date, Event, Metric, Goal Is Automatic Set and Goal Value Set.
#' @param csv A logical variable that decides whether a csv output file is created.
#' @param output_path A path to a folder where the csv summary file(s) will be written. Defaults to a folder in the current
#' work directory named "summary".
#'
#' @return A dataframe is generated to summarise each participants usage data.
#' @export
#'
#' @importFrom rlang .data

individual_summary <- function(data, csv = FALSE, output_path = file.path(getwd(), "summary")) {

  if(inherits(data, "data.frame") == FALSE){
    data <- data %>%
      dplyr::bind_rows(.id = "id")
  }

  # NEW CODE! Check that imported data has correct column names!

  if(!"id" %in% colnames(data)){
    stop("Column names of imported data are not compatable with this function.")
  }
  if(!"Date" %in% colnames(data)){
    stop("Column names of imported data are not compatable with this function.")
  }
  if(!"Event" %in% colnames(data)){
    stop("Column names of imported data are not compatable with this function.")
  }
  if(!"Metric" %in% colnames(data)){
    stop("Column names of imported data are not compatable with this function.")
  }
  if(!"Goal.Is.Automatic.Set" %in% colnames(data)){
    stop("Column names of imported data are not compatable with this function.")
  }
  if(!"Goal.Value.Set" %in% colnames(data)){
    stop("Column names of imported data are not compatable with this function.")
  }
  if(ncol(data) != 6){
    stop("Incorrect number of columns. Please ensure all csv files only have 5 columns.")
  }

  file_id <- unique(data$id)

  data <- data %>%
    dplyr::mutate(
      date = as.POSIXct(.data$Date), # convert date from chr to POSIXct format
      year = as.numeric(format(.data$date, "%Y")),
      month = as.numeric(format(.data$date, "%m")), # create month and day column
      day = as.numeric(format(.data$date, "%d"))
    ) %>%
    dplyr::relocate(.data$id, .data$date, .data$month, .data$day) %>%
    dplyr::select(-.data$Date)

  # calculate summary date for the total duration of app usage

  data %>%
    dplyr::filter(.data$Event == "app-state-changed") %>%
    dplyr::mutate(
      diff = lubridate::as.duration(.data$date - dplyr::lag(.data$date))
    ) %>%
    dplyr::filter(.data$Metric == "background") %>%
    dplyr::group_by(.data$id, .data$year, .data$month, .data$day) %>%
    dplyr::summarise(
      total_time = sum(.data$diff),
      average_time = mean(.data$diff, na.rm = TRUE),
      std_dev_time = stats::sd(.data$diff, na.rm = TRUE),
      minimum_time = min(.data$diff, na.rm = TRUE),
      maximum_time = max(.data$diff, na.rm = TRUE)
    ) -> state_change_summary

  # create app element flagging columns and calcualte time difference

  data <- data %>%
    dplyr::mutate(
      lag_date = dplyr::lag(.data$date),
      my_stat = dplyr::if_else(stringr::str_detect(.data$Metric, "my-stat"), 1, 0),
      my_stat = dplyr::lag(.data$my_stat, 1),
      stat_diff = dplyr::if_else(.data$my_stat == 1, as.numeric(.data$date - .data$lag_date), 0),
      resources = dplyr::if_else(stringr::str_detect(.data$Metric, "resources"), 1, 0),
      resources = dplyr::lag(.data$resources, 1),
      resource_diff = dplyr::if_else(.data$resources == 1, as.numeric(.data$date - .data$lag_date), 0),
      my_goals = dplyr::if_else(stringr::str_detect(.data$Metric, "my-goal"), 1, 0),
      my_goals = dplyr::lag(.data$my_goals, 1),
      goal_diff = dplyr::if_else(.data$my_goals == 1, as.numeric(.data$date - .data$lag_date), 0),
      notifications = dplyr::if_else(stringr::str_detect(.data$Event, "notification"), 1, 0)
    )

  data[, 11:17][data[, 11:17] == 0] <- NA

  # summarise the usage of difference data for each individual app element

  data %>%
    dplyr::group_by(.data$id, .data$year, .data$month, .data$day) %>%
    dplyr::summarise(
      total_stat = sum(.data$stat_diff, na.rm = TRUE),
      total_resource = sum(.data$resource_diff, na.rm = TRUE),
      total_goal = sum(.data$goal_diff, na.rm = TRUE),
      average_stat = mean(.data$stat_diff, na.rm = TRUE),
      average_resource = mean(.data$resource_diff, na.rm = TRUE),
      average_goal = mean(.data$goal_diff, na.rm = TRUE),
      stdev_stat = stats::sd(.data$stat_diff, na.rm = TRUE),
      stdev_resources = stats::sd(.data$resource_diff, na.rm = TRUE),
      stdev_goal = stats::sd(.data$goal_diff, na.rm = TRUE),
      min_stat = min(.data$stat_diff[which(.data$stat_diff > 0)]),
      min_resource = min(.data$resource_diff[which(.data$resource_diff > 0)]),
      min_goal = min(.data$goal_diff[which(.data$goal_diff > 0)]),
      max_stat = max(.data$stat_diff, na.rm = TRUE),
      max_resource = max(.data$resource_diff, na.rm = TRUE),
      max_goal = max(.data$goal_diff, na.rm = TRUE),
      notificaition_count = sum(.data$notifications)
    ) -> user_summary

  # due to minimum not including zero, coerces inf so convert these back to zeros

  is.na(user_summary) <- sapply(user_summary, is.infinite)
  user_summary[is.na(user_summary)] <- 0

  # bind dataframes from state change and summary using inner join

  summary <- dplyr::inner_join(state_change_summary, user_summary) %>%
    dplyr::mutate(
      date = lubridate::make_date(.data$year, .data$month, .data$day)
    ) %>%
    dplyr::relocate(.data$id, .data$date, .data$year, .data$month, .data$day)

  summary <- data.frame(lapply(summary,
                               function(x) if(is.numeric(x)) round(x, 2) else x))

  summary <- split(summary, f = summary$id)

  # write the total summary data back to a csv

  if (csv == TRUE) {

    for (i in seq_along(summary)) {
      utils::write.csv(summary[[i]], file.path(output_path, paste0(summary[[i]]$id[1], "_summary.csv")))
    }
  }

  return(summary)
}



