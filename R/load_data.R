#' load_data
#'
#' @param folder_path
#'
#' @return
#' @export
#'
#' @examples
load_data <- function(folder_path = file.path(getwd(), "data")) {
  folder_path %>%
    list.files(pattern = "*.csv") -> file_names
  file.path(folder_path, file_names) %>%
    map(read.csv) %>%
    setNames(substr(file_names, 20, 21))
}
