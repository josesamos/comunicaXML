#' Check if a dataframe cell is empty
#'
#' This function checks whether a given cell from a dataframe is empty.
#' A cell is considered empty if it contains `NA` or an empty string (`""`).
#'
#' @param cell A single value from a dataframe.
#'
#' @return `TRUE` if the cell is empty (`NA` or `""`), otherwise `FALSE`.
#'
#' @keywords internal
is_cell_empty <- function(cell) {
  as.vector(sapply(cell, function(x) is.na(x) || x == ""))
}
