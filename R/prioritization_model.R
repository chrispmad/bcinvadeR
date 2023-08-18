#' Title Run a prioritization model for a species of interest
#'
#' @param geog_units A spatial table (sf object) of polygons for which to estimate risk values
#' @param geog_id_col One or more columns that specify the unique identity of each polygon
#' @param risk_factors One or more variables pertaining to geographic units that could
#' influence the risk estimation; options include 'prox_to_settlements', 'rec_facilities'
#' @param risk_factor_weights A vector of weights for the selected risk factors; number of weights must be equal to number of risk factors
#' @param data_folder Folder on local machine for risk factor dataset(s)
#' @param quiet Whether or not this function will return copious feedback at each step
#' @param n_bins Number of bins to categorize risk factors into; defaults to 3
#' @param output_folder Folder on local machine in which to put results
#' @param species_name Species of interest's common name; will be intuited from data if not provided.
#' @param plot_types One or more plots to be generated, including 'static', 'leaflet', and 'plotly'
#' @param save_spatial_table TRUE or FALSE; save results table with geometries to output folder?
#' @param apply_second_bin_to_summed_bins TRUE or FALSE; bin the sum of risk factor bins? This gives us a final risk estimate column with values of 1, 2 or 3.
#' @param set_wd_dir set working directory for function; potentially temporary
#' @param ggplot_types A vector of plot types to make ggplots for; currently only bar_graph is implemented
#'
#' @return A table of estimated overall risk to ecosystem function based on the risk factors supplied; additional possible outputs include one or more choropleth images and the corresponding polygon dataset
#' @export
#'
#' @examples
prioritization_model = function(
                         geog_units,
                         geog_id_col = NULL,
                         risk_factors = c(NA),
                         risk_factor_weights = c(NA),
                         n_bins = 3,
                         data_folder = NULL,
                         output_folder = NULL,
                         set_wd_dir = NULL,
                         species_name = NULL,
                         plot_types = c("none"),
                         save_spatial_table = FALSE,
                         apply_second_bin_to_summed_bins = TRUE,
                         ggplot_types = 'bar_graph',
                         quiet = F){

  # Adjust working directory, if necessary.
  if(!is.null(set_wd_dir)) {
    setwd(set_wd_dir)
    cat(paste0("\nWorking directory adjusted to: ",getwd()))
  }

  # # Make sure the first two layers are spatial.
  # test()

  # Vector of preset risk layers
  preset_risk_layers = c("prox_to_settlements","rec_facilities")

  # If the user included a filetype suffix in risk factors, remove now.
  risk_factors = stringr::str_remove_all(risk_factors, '\\..*$')

  # Convert CRS of sp_occ_dat and geog_units to EPSG WGS84
  species_occurrence_data = sf::st_transform(species_occurrence_data, 4326)
  geog_units = sf::st_transform(geog_units, 4326)

  # Check that some ID column is supplied or identifiable from the geographic unit dataset.
  if(is.null(geog_id_col)){
    if(!quiet) cat("\nNo column was provided as the ID column of the geographic units. Intuiting column now...")
    if(sum(str_detect(names(geog_units), '(DISTRICT_NAME|REGION_NAME|GNIS_NA)')) == 0) {
      stop("No suitable ID column for geographic units could be automatically detected; please supply a column name.")
    } else {
      geog_id_col = names(geog_units)[str_detect(names(geog_units),'(DISTRICT_NAME|REGION_NAME|GNIS_NA)')][1]
    if(!quiet) cat(paste0("\nLikely column that will be used from here onwards: ",geog_id_col))
    }
  }

  # Check that we have the same number of elements in
  # the risk_factors vector and the vector of weights.
  if(length(risk_factors) != length(risk_factor_weights)){
    stop("The number of additional inputs and associated weights is not equal.")
  }

  # # Check that species and spatial data overlap.
  # test()

  # # Check that the additional layers are either spatial or can
  # # be joined on to our sf object by one or more column(s).
  # # * If spatial, check CRS.
  # test()

  # Find the likely species name based on input data.
  species_name = intuit_species_name(species_name, species_occurrence_data, quiet)

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
    mutate(num_occ = tidyr::replace_na(num_occ, 0)) |>
    # Add bin for num_occ
    mutate(num_occ_bin = as.numeric(cut(num_occ, n_bins)))

  # Load, process, and join additional inputs to geog_units
  for(i in 1:length(risk_factors)){

    input = risk_factors[i]
    input_weight = risk_factor_weights[i]

    if(!quiet) cat(paste0("\nAssessing input ",input,"..."))

    if(input %in% all_of(preset_risk_layers)){
      input_type = 'preset'
    } else {
      input_type = 'user_supplied'
    }

    # Do we have the input already on our local machine? If not, download it.
    check_data_folder_for_input(input, input_type, data_folder, quiet)

    # Prepare input data to be joined to geographic units
    data_to_join = process_input_data(input, input_weight, input_type, n_bins, geog_units, geog_id_col, data_folder, quiet)

    # Add weighted version of input risk factor bin.
    columns_to_add_weighted_bin = names(data_to_join[,stringr::str_detect(names(data_to_join),'_bin$')])

    for(column in columns_to_add_weighted_bin){
      data_to_join[[paste0(column,"_weighted")]] = data_to_join[[column]] * input_weight
    }

    # If the data_to_join has duplicate rows for the geographic units, flag this
    # and give the user a warning.
    if(nrow(data_to_join) > nrow(geog_units)){
      warning("Warning: ",input," has more rows than the geographic units, indicating duplication in the input processing step. Probably worth reviewing that before continuing!")
    }
    # Left-join the processed data to geog_units.

    geog_units = left_join(geog_units, data_to_join, by = geog_id_col) |>
      # If the data to join in this loop doesn't include a row for each
      # geog_unit, replace NA from left_join with 0.
      mutate(across(names(data_to_join[,-1]), \(x) replace_na(x, 0)))
    if(!quiet) cat(paste0("\n",input," successfully joined to geog_units"))
  }

  # Calculate the overall risk estimate from our bins!
  geog_units = geog_units |>
    #Apply weights to bins temporarily, so they affect the summed_risk_factor_bins
    mutate(summed_risk_factor_bins = rowSums(across(ends_with("_bin_weighted"))))

  # # If apply_second_bin_to_summed_bins is TRUE in options, add that final column.
  # if(apply_second_bin_to_summed_bins) {
    geog_units = geog_units |> mutate(risk_estimate_bin = as.numeric(cut(summed_risk_factor_bins, n_bins)))
  # }

  # Make plots? If yes, could be one or more of static, plotly, or leaflet.
  if(plot_types[1] != 'none'){

    time_suffix = str_replace(str_remove(str_replace_all(Sys.time(),':',"-"),"-[0-9]+\\.[0-9]*$"),' ','_')

    if('static' %in% plot_types){
      # If no folder for static plots inside output folder, create it.
      if(!dir.exists(paste0(output_folder,'/static_plots/'))) dir.create(paste0(output_folder,'/static_plots/'))
      make_ggplots(geog_units, geog_id_col, time_suffix, output_folder, ggplot_types)
    }
    if('plotly' %in% plot_types){
      make_plotly(geog_units, geog_id_col, time_suffix, output_folder, binned_variables)
    }
    if('leaflet' %in% plot_types){
      make_leaflet(geog_units, geog_id_col, time_suffix, output_folder, n_bins)
    }

  }

  # Output a spatial object with risk values.
  if(save_spatial_table){
    write_sf(geog_units, paste0(output_folder,'/',species_name,'_prioritization_model_output.gpkg'))
  }

  # Drop geometry column so that our output is a simple table.
  geog_units = geog_units |>
    st_drop_geometry()

  geog_units
}
