#' Clean Individual Employment
#'
#' @param x A tibble (or other structure) with data from a query of the Individual Profile Employment dataset as provided by BoardEx.
#'
#' @return Cleaned version of the original tibble with NA values, overlaps, etc. removed. Also capitalizes all column names and prints a summary of what was removed.
#' @export
#'
#' @examples individual_empl <- individual_empl %>% clean_individual_employment()
clean_individual_employment <- function(x)
{
  #First save the number of rows in the data
  original_observations <- base::nrow(x)

  #Now, begin the cleaning process
  x <- WRDStools::capitalize(x)
  tryCatch(
    {
      #Find remove all duplicated (everything + ISIN), then remove all duplicated (everything + CompanyID); effectively an "OR" statement
      to_return <- x %>%

        #Group where all observations have the same value for all of these fields
        dplyr::group_by(ISIN, ROLENAME, DATESTARTROLE, DATEENDROLE) %>%

        #Slice to take only the first occurance (essentially keeping only one unique value)
        dplyr::slice_max(order_by = row_number(), n = 1) %>%

        #Ungroup
        dplyr::ungroup()

      #Take those distinct
      to_return <- to_return %>%

        #Keep only distinct by companyID
        dplyr::group_by(COMPANYID, ROLENAME, DATESTARTROLE, DATEENDROLE) %>%
        dplyr::slice_max(order_by = row_number(), n = 1) %>%
        dplyr::ungroup()

      #Count the number of unique observations
      unique_observations <- base::nrow(to_return)

      #remove any observations without an ISIN
      to_return <- to_return %>%
        dplyr::filter(ISIN != "")

      #Count the number of no ISIN observations
      no_ISIN <- base::nrow(to_return)

      base::writeLines(
        c("CLEAN INDIVIDUAL EMPLOYMENT: ",
          "-------------------------------------------------------------------",
          paste0("Original Observations: ",original_observations),
          paste0("Unique Observations: ",unique_observations),
          paste0("Unique Observations with an ISIN: ",no_ISIN),
        "")
      )
      return(to_return)
    },
    error = function(e)
    {
      message(
        "This function is only applicable to querys of the Individual Profile dataset in the BoardEx library, please ensure the tibble dataframe or tibble entered satisfies this requirement."
      )
    }
  )
}
