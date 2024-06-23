#' Print Column
#'
#' @param x A tibble (or other structure) from which to read.
#' @param column The name of a column in structure x to be printed to the console.
#'
#' @return The function is void; it does not return anything, only prints to the console.
#' @export
#'
#' @examples print_column(individual_employment, ISIN)
print_column <- function(x, column)
{

  columnString <- as.character(x[[column]]) %>% paste(collapse = " ") %>%
    print()
}
