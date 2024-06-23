#' Clean Trucost
#'
#' @param x A tibble (or other structure) with data from a query of the Environment dataset as provided by Trucost.
#'
#' @return Cleaned version of the original tibble with NA values, overlaps, etc. removed. Also capitalizes all column names and prints a summary of what was removed.
#' @export
#'
#' @examples cleaned_env <- env %>% clean_trucost()
clean_trucost <- function(x)
{
  #Capitalize all the columns
  x <- WRDStools::capitalize(x)

  #count original observations
  original_observations <- base::nrow(x)

  #If either ticker or gvkey is empty, that row will not be included
  x <- dplyr::filter(x, GVKEY != "") %>%
    dplyr::filter(!is.na(GVKEY))

  #Count filtered observations
  filtered_observations <- base::nrow(x)

  #Print a summary
  base::writeLines(
    c("CLEAN TRUCOST: ",
      "---------------------------------------------",
      base::paste0("Original Observations: ",original_observations),
      base::paste0("Observations with a GVKey: ",filtered_observations),
      "")
  )

  return(x)
}
