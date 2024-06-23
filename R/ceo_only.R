#' CEO only
#'
#' @param x A tibble (or other structure) that contains a column "ROLENAME"
#'
#' @return The structure filtered to remove any observations in which "ROLENAME" is not some form of CEO. Also prints a summary of how many observations were removed.
#' @export
#'
#' @examples ceo_only(individual_employment)
ceo_only <- function(x)
{

  #Count original rows
  original_observations <- base::nrow(x)

  #Uses the GREPL to check if the row contains "CEO" and does not contain "Division" or "Regional"
  if(!("ROLENAME" %in% colnames(x)))
  {
    stop("Error: No ROLENAME column could be found for ceo_only().")
  }

  #Filter out any role without CEO or with any qualifier (like regional) that implies they are not the chief-executive.
  to_return <- x %>%
    dplyr::filter(grepl("CEO", .data$ROLENAME)) %>%
    dplyr::filter(!grepl("Division", .data$ROLENAME)) %>%
    dplyr::filter(!grepl("Regional", .data$ROLENAME)) %>%
    dplyr::filter(!grepl("Co-CEO", .data$ROLENAME))

  #Count rows after
  filtered_observations <- base::nrow(to_return)

  #Print a summary
  base::writeLines(
    c("CEO ONLY: ",
      "-------------------------------------------------------------------",
      paste0("Original Observations: ",original_observations),
      paste0("CEOs Only: ",filtered_observations),
      ""
    )
  )
  return(to_return)
}
