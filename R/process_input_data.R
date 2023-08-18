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
process_input_data = function(input_name, input_weight, input_type, n_bins, geog_units, geog_id_col, data_folder, quiet){

  if(input_type == 'preset'){
    if(input_name == 'prox_to_settlements'){
      output = process_for_prox_to_settlements(input_name, input_weight, n_bins,
                                               geog_units, geog_id_col, data_folder, quiet)
    }
    if(input_name == 'rec_facilities'){
      output = process_for_spatial_layers(input_name, input_weight, n_bins,
                                          geog_units, geog_id_col, data_folder, quiet)
    }
  } else {
    output = process_for_user_data(input_name, input_weight, n_bins,
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
# Process for spatial layers

process_for_spatial_layers = function(input_name, input_weight, n_bins, geog_units, geog_id_col, data_folder, quiet){
  if(!quiet) cat("\nRecreational facilities geopackage file read in...")

  if(file.exists(paste0(data_folder,'/',input_name,'.gpkg'))){
    dat = read_sf(paste0(data_folder,'/',input_name,'.gpkg')) |> sf::st_transform(crs = 4326)
  }
  if(file.exists(paste0(data_folder,'/',input_name,'.shp'))){
    dat = read_sf(paste0(data_folder,'/',input_name,'.shp')) |> sf::st_transform(crs = 4326)
  }

  output = dat |>
    sf::st_join(geog_units, st_intersects) |>
    sf::st_drop_geometry() |>
    dplyr::count(!!sym(geog_id_col),
                 name = paste0(stringr::str_remove_all(input_name,'\\.[a-zA-Z]+$'),'_raw')) |>
    filter(!is.na(!!sym(geog_id_col)))

  output = output |>
    mutate(!!sym(paste0(stringr::str_remove_all(input_name,'\\.[a-zA-Z]+$'),'_bin')) := as.numeric(cut(!!sym(paste0(stringr::str_remove_all(input_name,'\\.[a-zA-Z]+$'),'_raw')), n_bins)))
}

# ===================================
# User Data Processing Logic

process_for_user_data = function(input_name, input_weight, n_bins, geog_units, geog_id_col, data_folder, quiet){
  # Is the user file format one of the acceptable options?

  # if(!stringr::str_detect(input_name,'(\\.shp|\\.gpkg|\\.csv|\\.xlsx)$')){
  #   # If not, give error.
  #   stop(paste0("Error: ",input_name," must be one of .xlsx, .csv, .shp or .gpkg"))
  # }
  # If yes, read in data
  if(paste0(input_name,".shp") %in% list.files(data_folder) | paste0(input_name,".gpkg") %in% list.files(data_folder)){
    # stringr::str_detect(input_name,'(\\.shp|\\.gpkg)$')){
    # Spatial input from user
    output = process_for_spatial_layers(input_name, input_weight, n_bins,
                                        geog_units, geog_id_col, data_folder, quiet)
  } else {
    if(paste0(input_name,".csv") %in% list.files(data_folder)){
      # .CSV file from user
      user_dat = read.csv(paste0(data_folder,'/',input_name,'.csv'))
    }
    if(paste0(input_name,".xlsx") %in% list.files(data_folder)){
      user_dat = openxlsx::read.xlsx(paste0(data_folder,'/',input_name,'.xlsx'))
    }
    # Summarise numeric column(s) by the geog_unit grouping column.
    output = user_dat |>
      group_by(!!sym(geog_id_col)) |>
      summarise(across(where(is.numeric), \(x) sum(x, na.rm=T)))

    columns_to_bin = names(output[,-1])

    for(column_name in columns_to_bin){
      output = output |>
        dplyr::mutate(!!sym(paste0(column_name,'_raw')) := as.numeric(!!sym(column_name))) |>
        dplyr::select(-column_name) |>
        mutate(!!sym(paste0(column_name,'_bin')) := as.numeric(cut(!!sym(paste0(column_name,'_raw')),n_bins)))
    }
    output
  }
}
