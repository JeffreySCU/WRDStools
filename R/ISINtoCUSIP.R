#' ISIN to CUSIP
#'
#' @param x A tibble (or other structure) containing a column named "ISIN".
#'
#' @return The same tibble, but with its ISIN values converted to CUSIP values and the column name "ISIN" changed to "CUSIP"
#' @export
#'
#' @examples ISINtoCUSIP(individual_employment)
ISINtoCUSIP <- function(x)
{
  x <- x %>% dplyr::rename(CUSIP = ISIN) %>% dplyr::mutate(CUSIP = str_sub(CUSIP, start = 3, end = -2))
  return(x)
}
