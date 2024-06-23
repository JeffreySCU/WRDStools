#' Capitalize
#'
#' @param x A tibble (or other structure) with columns
#'
#' @return The original tibble where all column names have been converted to uppercase
#' @export
#'
#' @examples capitalize(trucost)
capitalize <- function(x)
{
  base::colnames(x) <- base::toupper(colnames(x))
  return(x)
}
