#' Title Download / calculate risk layers from BC Data Catalogue
#'
#' @param geog_units Geographic units for which to calculate risk layers.
#' @param layer Risk layer; one of 'prox_to_settlements'
#'
#' @return Various datasets that can be plugged into R6 priorization models as risk layers.
#' @export
#'
#' @examples \dontrun{}
get_bcdc_risk_layer = function(geog_units, geog_id_col, layer = c('prox_to_settlements')){
  if(layer == 'prox_to_settlements'){

    dat = bcmaps::bc_cities() |>
      sf::st_transform(dat, crs = sf::st_crs(geog_units))

    distance_matrix = sf::st_distance(dat, geog_units) |>
      tidyr::as_tibble() |>
      sapply(as.numeric) |>
      tidyr::as_tibble()

    output = distance_matrix |>
      dplyr::reframe(dplyr::across(dplyr::everything(), \(x) min(round(x/1000,3)))) |>
      purrr::set_names(geog_units[[geog_id_col]]) |>
      tidyr::pivot_longer(dplyr::everything(), names_to = geog_id_col, values_to = 'prox_to_settlements_raw')

    output
  }
}
