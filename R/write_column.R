#' Write Column
#'
#' @param x A tibble (or other structure) from which to read.
#' @param column The name of a column in structure x to be printed to a tempfile.
#'
#' @return A temp_file containing the column written as a string, delineated by spaces (such that it can be copied and pasted for use with querying in WRDS).
#' @export
#'
#' @examples write_column(individual_employment, ISIN)
write_column <- function(x, column)
{
  temp_file <- base::tempfile(fileext = ".csv") #Creates a temporary .csv file
  base::on.exit(unlink(temp_file), add = TRUE)  #Closes the .csv when the R markdown session ends

  columnString <- base::as.character(x[[column]]) %>% base::paste(collapse = " ") #Converts each element of column into a string, pastes them all together into "columnString" separated by a space.

  base::writeLines(columnString, temp_file)       #Writes columnString to the temp file

  base::paste("PRINT: ", columnString)     #Prints the columnString
  return(temp_file)
}
