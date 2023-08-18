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
                                       input_type,
                                       data_folder,
                                       quiet){

  data_file_name = case_when(
    input_name == 'prox_to_settlements' ~ 'bc_settlements.gpkg',
    input_name == 'rec_facilities' ~ 'rec_facilities.gpkg',
     T ~ input_name
  )

  if(!quiet) cat(paste0("\nChecking data folder for requisite spatial files to calculate ",input_name))

  if(!input_name %in% stringr::str_remove_all(list.files(data_folder),'\\..*$')){
    # !file.exists(paste0(data_folder,'/',data_file_name))){

    if(input_type == 'preset'){
      if(!quiet) cat(paste0("\n",input_name," not found in ",data_folder,"; attempting to download"))
      download_additional_input(input_name, data_folder, quiet = F)
    } else {
      if(!quiet) cat(paste0("\n",input_name," not found in ",data_folder,"; please place file in this folder, then re-run prioritization model"))
      stop()
    }
  } else {
    if(!quiet) cat(paste0("\n",input_name," present in data folder"))
  }
}

# ==============================

# Multi-purpose download function for possible additional input layers.
download_additional_input = function(input_name, data_folder, quiet = F){

  # # Do we have access / writing privileges to the supplied write_folder?
  # test() stop("Not able to save spatial layers to the supplied write folder!")

  if(input_name == "maj_roads"){
    maj_roads = download_maj_roads()
    write_sf(maj_roads, data_folder)
  }

  if(input_name == "bc_settlements"){
    settlements = bcmaps::bc_cities() |> sf::st_transform(crs = 4326)
    write_sf(settlements, paste0(data_folder,"/",input_name,".gpkg"))
  }

  if(input_name == "rec_facilities"){
    rec_facilities = bcdata::bcdc_query_geodata('recreational-features-inventory') |>
      bcdata::filter(bcdata::INTERSECTS(geog_units)) |>
      bcdata::collect() |>
      sf::st_transform(crs = 4326)
    write_sf(rec_facilities, paste0(data_folder,"/",data_file_name,".gpkg"))
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
