# ==============================

# Potentially multi-purpose data processing function. Currently just works for spatial files.

#' Title Read in and process additional spatial layers that represent either a risk increase
#' or a risk decrease to produce an output for left_joining with geog_units
#'
#' @param input_name name of this potential risk factor for prioritization model
#' @param input_weight weight assigned to this potential risk factor (can be positive or negative)
#' @param n_bins number of bins into which to categorize this input
#' @param geog_units the sf object (i.e. a spatial table) for which we are
#' performing the prioritization model
#' @param geog_id_col the column of the sf object that identifies each row uniquely
#' @param data_folder folder on local machine wherein the risk factor
#' spatial files can be found
#' @param quiet should the function be quiet instead of chatty?
#'
#' @return internal function used in prioritization model to read in and process
#' additional risk factor layers.
#'
#' @examples
process_input_data = function(input_name, input_weight, n_bins, geog_units, geog_id_col, data_folder, quiet){

  if(input_name == 'prox_to_settlements'){
    output = process_for_prox_to_settlements(input_name, input_weight, n_bins,
                                             geog_units, geog_id_col, data_folder, quiet)
  }
  if(input_name == 'rec_facilities'){
    output = process_for_rec_facilities(input_name, input_weight, n_bins,
                                             geog_units, geog_id_col, data_folder, quiet)
  }
  output
}

# ==============================
# Proximity to settlements

process_for_prox_to_settlements = function(input_name, input_weight, n_bins, geog_units, geog_id_col, data_folder, quiet){
  if(!quiet) cat("\nBC settlement geopackage file read in...")
  dat = read_sf(paste0(data_folder,'/bc_settlements.gpkg'))

  distance_matrix = sf::st_distance(dat, geog_units) |>
    as_tibble() |>
    sapply(as.numeric) |>
    as_tibble()

  if(!quiet) cat("\nDistance matrix calculated...")

  output = distance_matrix |>
    reframe(across(everything(), \(x) min(round(x/1000,3)))) |>
    set_names(geog_units[[geog_id_col]]) |>
    pivot_longer(everything(), names_to = geog_id_col, values_to = 'prox_to_settlements_raw')

  output = output |>
    mutate(prox_to_settlements_bin = as.numeric(cut(1/prox_to_settlements_raw, n_bins)))
}

# ==============================
# Proximity to settlements

process_for_rec_facilities = function(input_name, input_weight, n_bins, geog_units, geog_id_col, data_folder, quiet){
  if(!quiet) cat("\nRecreational facilities geopackage file read in...")
  dat = read_sf(paste0(data_folder,'/rec_facilities.gpkg'))

  output = dat |>
    sf::st_join(geog_units, st_intersects) |>
    sf::st_drop_geometry() |>
    dplyr::count(!!sym(geog_id_col), name = 'rec_facilities_raw')

  output = output |>
    mutate(rec_facilities_bin = as.numeric(cut(rec_facilities_raw, n_bins)))
}

# all_layers = bcdc_list()
# all_layers[str_detect(all_layers,'recreation')]

