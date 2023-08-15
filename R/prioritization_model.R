# Brain map for generalized prioritization model function.

#' Title
#'
#' @param species_occurrence_data A spatial table (sf object) of species occurrence data
#' @param geog_units A spatial table (sf object) of polygons for which to estimate risk values
#' @param geog_id_col One or more columns that specify the unique identity of each polygon
#' @param risk_factors One or more variables pertaining to geographic units that could influence the risk estimation
#' @param risk_factor_weights A vector of weights for the selected risk factors; number of weights must be equal to number of risk factors
#' @param data_folder Folder on local machine for risk factor dataset(s)
#' @param quiet Whether or not this function will return copious feedback at each step
#' @param n_bins Number of bins to categorize risk factors into; defaults to 3
#' @param output_folder Folder on local machine in which to put results
#' @param set_wd_dir set working directory for function; potentially temporary
#' @param options list of additional options, including the species name (otherwise intuited), and whether to make a choropleth and/or sf multipolygon object output
#'
#' @return A table of estimated risk values for the geographic units supplied; additional possible outputs include one or more choropleth images and the corresponding polygon dataset
#' @export
#'
#' @examples
prioritization_model = function(
                         geog_units,
                         geog_id_col = NULL,
                         species_occurrence_data,
                         risk_factors = c(NA),
                         risk_factor_weights = c(NA),
                         n_bins = 3,
                         data_folder = NULL,
                         output_folder = NULL,
                         quiet = F,
                         set_wd_dir = NULL,
                         options = list(
                           species_name = NULL,
                           make_choropleth = FALSE,
                           save_spatial_table = FALSE
                         )
                         ){

  # Adjust working directory, if necessary.
  if(!is.null(set_wd_dir)) {
    setwd(set_wd_dir)
    cat(paste0("\nWorking directory adjusted to: ",getwd()))
  }

  # # Make sure the first two layers are spatial.
  # test()

  # Convert CRS of sp_occ_dat and geog_units to EPSG WGS84
  species_occurrence_data = sf::st_transform(species_occurrence_data, 4326)
  geog_units = sf::st_transform(geog_units, 4326)

  # Check that some ID column is supplied or identifiable from the geographic unit dataset.
  if(is.null(geog_id_col)){
    cat("\nNo column was provided as the ID column of the geographic units. Intuiting column now...")
    if(sum(str_detect(names(geog_units), '(DISTRICT_NAME|REGION_NAME|GNIS_NA)')) == 0) {
      stop("No suitable ID column for geographic units could be automatically detected; please supply a column name.")
    } else {
      geog_id_col = names(geog_units)[str_detect(names(geog_units),'(DISTRICT_NAME|REGION_NAME|GNIS_NA)')][1]
    cat(paste0("\nLikely column that will be used from here onwards: ",geog_id_col))
    }
  }

  # # Check that we have the same number of elements in
  # # the risk_factors vector and the vector of weights.
  # test() stop("The number of additional inputs and associated weights is not equal.")

  # # Check that species and spatial data overlap.
  # test()

  # # Check that the additional layers are either spatial or can
  # # be joined on to our sf object by one or more column(s).
  # # * If spatial, check CRS.
  # test()

  # Find the likely species name based on input data.
  species_name = intuit_species_name(options$species_name, species_occurrence_data, quiet)

  # Find the likely data and output folders, if not supplied.
  the_folders = check_data_and_output_folders(data_folder, output_folder, quiet)
  data_folder = the_folders[[1]]
  output_folder = the_folders[[2]]

  # Summarize species occurrence data to spatial object.
  species_occurrence_data = sum_spatial_data_to_geog_units(dat = species_occurrence_data,
                                                      geog_units,
                                                      geog_id_col,
                                                      quiet)

  # Add species occurrence summary numbers to geographic units.
  geog_units = geog_units |>
    left_join(species_occurrence_data, by = geog_id_col)

  # Replace NA for species occurrence with 0.
  geog_units = geog_units |>
    mutate(num_occ = tidyr::replace_na(num_occ, 0))

  # # Load / calculate / join additional inputs to geog_units
  # risk_factors_list = list()

  for(i in 1:length(risk_factors)){

    input = risk_factors[i]
    input_weight = risk_factor_weights[i]

    if(!quiet) cat(paste0("\nAssessing input ",input,"..."))

    # Do we have the input already on our local machine? If not, download it.
    check_data_folder_for_input(input, data_folder)

    # Join input to geographic units
    data_to_join = process_input_data(input, input_weight, n_bins, geog_units, geog_id_col, data_folder, quiet)

    # If the data_to_join has duplicate rows for the geographic units, flag this
    # and give the user a warning.
    if(nrow(data_to_join) > nrow(geog_units)){
      warning("Warning: ",input," has more rows than the geographic units, indicating duplication in the input processing step. Probably worth reviewing that before continuing!")
    }
    # Left-join the processed data to geog_units.
    geog_units = left_join(geog_units, data_to_join, by = geog_id_col)
    if(!quiet) cat(paste0("\n",input," successfully joined to geog_units"))
  }

  # Calculate the overall risk estimate from our bins!
  geog_units = geog_units |> mutate(summed_risk_factor_bins = rowSums(across(ends_with("_bin"))))

  # Make a choropleth?
  if(options$make_choropleth){

    the_choropleth = make_choropleth(geog_units)

    time_suffix = str_remove(str_replace_all(Sys.time(),':',"-"),"-[0-9]+\\.[0-9]*$")

    choropleth_filename = paste0(output_folder,'/choropleth_',time_suffix,'.png')

    save_choropleth(the_choropleth, choropleth_filename)
  }

  # Output a spatial object with risk values.
  if(options$save_spatial_table){
    write_sf(geog_units, paste0(output_folder,'/',likely_sp_name,'prioritization_model_output.gpkg'))
  }

  # Drop geometry column so that our output is a simple table.
  geog_units = geog_units |>
    st_drop_geometry()

  geog_units
}
