# ==============================

# Intuit the data and output folder(s), if none are specified.

check_data_and_output_folders = function(data_folder, output_folder, quiet){
  if(is.null(data_folder)){
    if(!dir.exists(paste0(getwd(),'/data'))){
      dir.create(paste0(getwd(),'/data'))
    }
    data_folder = paste0(getwd(),'/data')
  }

  if(is.null(output_folder)){
    if(!dir.exists(paste0(getwd(),'/output'))){
      dir.create(paste0(getwd(),'/output'))
    }
    output_folder = paste0(getwd(),'/output')
  }
  if(!quiet){
    cat(paste0("\nData folder intuited as: ",data_folder))
    cat(paste0("\nOutput folder intuited as: ",output_folder))
  }
  return(list(data_folder,output_folder))
}

# Intuit the likely species name, if not supplied.
#' Title
#'
#' @param species_name Name of species for which the model is run
#' @param species_occurrence_data Species occurrence dataset for species of interest
#'
#' @return
#' @export
#'
#' @examples
intuit_species_name = function(species_name,species_occurrence_data,quiet){
  if(is.null(species_name)){
    cat("\nIntuiting likely species name from data...")
    if('Species' %in% names(species_occurrence_data)){
      species_name = unique(species_occurrence_data$Species)[1]
    } else {
      species_name = 'Unknown_species'
    }
    if(quiet == F){
      cat(paste0("selected '",species_name,"'"))
    }
  }

  species_name
}
# ==============================

# summarize species occurrence data points to chosen geographic units.
sum_spatial_data_to_geog_units = function(dat,
                                     geog_units,
                                     geog_id_col,
                                     quiet){
  if(!quiet){cat("\nSummarizing species occurrence data to geographic units...")}
  dat |>
    sf::st_join(
      geog_units,
      st_intersects
    ) |>
    st_drop_geometry() |>
    group_by(!!sym(geog_id_col)) |>
    summarise(num_occ = n())
}

# ==============================

# check if given additional input dataset is in data folder. If not, download.

check_data_folder_for_input = function(input_name,
                                       data_folder){

  data_file_name = case_when(
    input_name == 'prox_to_settlements' ~ 'bc_settlements.gpkg'
  )

  cat(paste0("\nChecking data folder for requisite spatial files to calculate ",input_name))

  if(!file.exists(paste0(data_folder,'/',data_file_name))){

    cat(paste0("\n",data_file_name," not found in ",data_folder,"; attempting to download"))

    download_additional_input(data_file_name, data_folder, quiet = F)
  } else {
    cat(paste0("\n",data_file_name," present in data folder"))
  }
}

# ==============================

# Multi-purpose download function for possible additional input layers.
download_additional_input = function(data_file_name, data_folder, quiet = F){

  # # Do we have access / writing privileges to the supplied write_folder?
  # test() stop("Not able to save spatial layers to the supplied write folder!")

  if(data_file_name == "maj_roads.gpkg"){
    maj_roads = download_maj_roads()
    write_sf(maj_roads, data_folder)
  }

  if(data_file_name == "bc_settlements.gpkg"){
    settlements = bcmaps::bc_cities() |> sf::st_transform(crs = 4326)
    write_sf(settlements, paste0(data_folder,"/",data_file_name))
  }
}


# ==============================

# Potentially multi-purpose data processing function. Currently just works for spatial files.

#' Title
#'
#' @param input_name
#' @param input_weight
#' @param n_bins
#' @param geog_units
#' @param geog_id_col
#' @param data_folder
#' @param quiet
#'
#' @return
#' @export
#'
#' @examples
process_input_data = function(input_name, input_weight, n_bins, geog_units, geog_id_col, data_folder, quiet){

  if(input_name == 'prox_to_settlements'){
    cat("\nBC settlement geopackage file read in...")
    dat = read_sf(paste0(data_folder,'/bc_settlements.gpkg'))

    distance_matrix = sf::st_distance(dat, geog_units) |>
      as_tibble() |>
      sapply(as.numeric) |>
      as_tibble()

    if(!quiet) cat("\nDistance matrix calculated...")

    output = distance_matrix |>
      reframe(across(everything(), \(x) min(round(x/1000),2))) |>
      set_names(geog_units[[geog_id_col]]) |>
      pivot_longer(everything(), names_to = geog_id_col, values_to = 'prox_to_settlements_raw')

    output = output |>
      mutate(prox_to_settlements_bin = as.numeric(cut(1/prox_to_settlements_raw, n_bins)))

    output
  }
}

# ==============================

# Potentially multi-purpose function for adding a given input to our geographic units.

join_input_to_geog_units = function(geog_units, input){
  # Is the data spatial? Use a tryCatch to search for a geometry column to see.
  if(length(
      tryCatch(expr = sf::st_geometry(test),
           error = function(e){})
  ) > 0){
    input_data_type = 'spatial'
  } else {
    input_data_type = 'tabular'
  }

  if(input_data_type == 'spatial'){
    geog_units = sf::st_join(geog_units, input, st_intersects)
  }
  if(input_data_type == 'tabular'){
    geog_units = dplyr::left_join(geog_units, input, st_intersects)
  }
  geog_units
}
