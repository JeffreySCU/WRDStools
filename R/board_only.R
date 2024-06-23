#' Board only
#'
#' @param x A tibble (or other structure) with column "BRDPOSITION", a query field within BoardEx's "Individual Employment".
#'
#' @return Returns a filtered version of the original tibble which only contains observations for board members. Also prints a summary of how many observations were removed.
#' @export
#'
#' @examples board_only(individual_employment)
board_only <- function(x)
{
  #Count original_observations
  original_observations <- base::nrow(x)

  #filter by BrdPosition
  filtered <- x %>% dplyr::filter(BRDPOSITION != "No")

  #count board members
  board_observations <- base::nrow(x)

  #Print a summary
  base::writeLines(
    c("BOARD ONLY: ",
      "-------------------------------------------------------------------",
      paste0("Original Observations: ",original_observations),
      paste0("Board Observations: ",board_observations),
    "")
  )

  return(filtered)
}
