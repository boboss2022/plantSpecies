#' Match Plant Species
#'
#' This function matches plant species names between a provided data frame and
#' an internal reference data set (`d1`).
#'
#' @param user_data A data frame containing plant species. It must have a column `物种`.
#' @return A data frame containing matched species.
#' @export
#' @examples
#' user_data <- data.frame(物种 = c("Species1", "Species2"))
#' matched_species <- match_species(user_data)
#' print(matched_species)
match_species <- function(user_data) {
  if (!"物种" %in% colnames(user_data)) {
    stop("The input data frame must have a column named '物种'.")
  }

  # Load internal data
  data("d1", package = "SpeciesMatcher")

  # Match species
  matched_data <- dplyr::inner_join(user_data, d1, by = c("物种" = "species_c"))

  return(matched_data)
}
