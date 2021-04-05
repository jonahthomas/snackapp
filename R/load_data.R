#' load_data
#'
#' @param folder_path The path to the folder where SnackApp usage data is located. This path defaults to a folder named "data" within the current work directory.
#'
#' @return A list of tibbles which can then be analysed using other SnackApp functions.
#' @export
#'
#' @examples
load_data <- function(folder_path = file.path(getwd(), "data")) {
  folder_path %>%
    list.files(pattern = "*.csv") -> file_names
  file.path(folder_path, file_names) %>%
    purrr::map(vroom::vroom) %>%
    stats::setNames(substr(file_names, 20, 21))
}
