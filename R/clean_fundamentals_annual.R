#' Clean Fundamentals Annual
#'
#' @param x Tibble (or other structure) containing data from the Fundamentals Annual dataset as offered by Compustat.
#'
#' @return Cleaned version of the original tibble with NA values, overlaps, etc. removed. Also capitalizes all column names and prints a summary of what was removed.
#' @export
#'
#' @examples
clean_fundamentals_annual <- function(x)
{
  #capitalize all
  x <- WRDStools::capitalize(x)

  #count original observations
  original_observations <- base::nrow(x)

  x <- x %>%

    #Filter any blank or NA GVKEYs or CUSIPs
    dplyr::filter(!is.na(GVKEY)) %>%
    dplyr::filter(GVKEY != "") %>%
    dplyr::filter(!is.na(CUSIP)) %>%
    dplyr::filter(CUSIP != "")

  #count filtered observations
  filtered_observations <- base::nrow(x)

  #Print a summary
  base::writeLines(
    c("CLEAN FUNDAMENTALS ANNUAL: ",
      "-------------------------------------------------------------------",
      paste0("Original Observations: ",original_observations),
      paste0("Observations with a GVKey and a CUSIP: ",filtered_observations),
      "")
  )

  return(x)
}
