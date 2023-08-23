#' Title Get current list of risk layers hard-coded into prioritization model
#'
#' @return A vector of risk layer names
#' @export
#'
#' @examples current_risk_layers_available()
current_risk_layers_available <- function() {
  tibble(layer_name = c("prox_to_settlements",""),
         layer_detail = "Distance in kilometers from each geographic unit (e.g. lakes) to the closest BC city in the bcmaps package's bc_cities() function.")
}
