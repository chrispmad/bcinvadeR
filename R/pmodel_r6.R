#' Title Set up a new priorization model.
#'
#' @param occurrence_data Species occurrence data in a table with a geometry column (e.g. an sf object)
#' @param likely_sp_name (Optional)
#' @param geog_units Geographic units for analysis (e.g. 20 lakes)
#' @param risk_layers Risk layers to consider, e.g. water chemistry, angler pressure, etc.
#' @param risk_estimates Calculated risk estimates
#'
#' @return An R6 prioritization model model; use methods add(), run() and print() for more info.
#' @export
#'
#' @examples /dontrun{
#' No examples yet.
#' }

pmodel = R6::R6Class(
  'pmodel_r6',
  list(
    occurrence_data = NULL,
    geog_units = NULL,
    geog_id_col = NULL,
    risk_layers = list(),
    risk_weights = NULL,
    risk_estimates = NULL,

#' Title Print details of prioritization model
#'
#' @return Details of prioritization model
#'
#' @examples \dontrun{}
    print = function(){
      cat("\n********************************************************")
      cat("\nThis object represents an initialized prioritization model.")
      if(is.null(self$occurrence_data) & is.null(self$geog_units) & length(self$risk_layers) == 0){
        cat("\nTo begin, please use 'my_model_name'$add() function to add occurrence data, \ngeographic units, risk layers and \nrisk weights, then use 'my_model_name'$run() to estimate \nrisk of invasive species spread.")
      }
      if(!is.null(self$occurrence_data)){
        cat(paste0("\nPrioritization Framework consists of ",nrow(self$occurrence_data),
                   " rows of occurrence data."))
      }
      if(!is.null(self$geog_units)){
        cat(paste0("\n\nThe geographic units being considered consist of ",nrow(self$geog_units),
                   " separate features. Project system: ",unname(unlist(sf::st_crs(self$geog_units[1,])[1]))
                   ))
      }
      if(length(self$risk_layers) > 0){
        if(length(self$risk_layers) != length(self$risk_weights)){
          cat(paste0("\n\nRisk layers and weights not equal in length (",
                     length(self$risk_layers)," vs ",length(self$risk_weights),")"))
        } else {
        cat(paste0("\n\nRisk layers (N = ",length(self$risk_layers),") consist of ",
                   paste0(names(self$risk_layers), collapse = ', ')))
        }
      }
      if(is.null(self$risk_estimates)){
        cat(paste0("\n\nRisk estimates have not yet been calculated. Use the 'run()' function to calculate them."))
      }
    },

#' Title Add layer to prioritization model.
#'
#' @param role One of 'occurrence', 'geog_units', 'geog_id_col', 'risk', or 'weights'
#' @param x The layer to add (can be with or without geometry column)
#'
#' @return Adds a layer to R6 prioritization model
#'
#' @examples \dontrun{}
    add = function(role, x) {
      if(!role %in% c('occurrence','geog_units','geog_id_col','risk','weights')){
        stop("\nPlease choose one of 'occurrence', 'geog_units', 'geog_id_col', 'risk', or 'weights' for the 'role'")
      }
      if(role == 'occurrence'){
        if(!is.data.frame(x)) stop("Please input occurrence data in a tabular format.")
        if(sum(sf::st_is_empty(x)) == nrow(x)) stop("Tabular data must have a recognized geometry column with at least one row containing valid coordinates (e.g. an sf table)")
        self$occurrence_data = x
        invisible(self)
      }
      if(role == 'geog_units'){
        self$geog_units = x
        invisible(self)
      }
      if(role == 'geog_id_col'){
        if(!is.vector(x)) stop("Please input a single character string for the geog_id_col")
        if(!is.character(x)) stop("Please input a single character string for the geog_id_col")
        self$geog_id_col = x
      }
      if(role == 'risk'){
        cur_r_l = length(self$risk_layers)
        self$risk_layers[[cur_r_l + 1]] <- x
        if('geometry' %in% names(x)){
          names(self$risk_layers)[[cur_r_l + 1]] <- names(x)[names(x) != 'geometry'][1]
        } else {
        names(self$risk_layers)[[cur_r_l + 1]] <- stringr::str_remove_all(names(x)[2],'_raw')
        invisible(self)
        }
      }
      if(role == 'weights'){
        self$risk_weights = x
        invisible(self)
      }
    },

#' Title Run the prioritization model
#'
#' @param n_bins Number of bins to group risk layers and final risk estimates
#' @param quiet Should this function return copious feedback during the run?
#'
#' @return The prioritization model will have a new slot populated: geog_units_
#'
#' @examples \dontrun{}
    run = function(n_bins = 3, quiet = F) {

      if(is.null(self$occurrence_data) | is.null(self$geog_units) | length(self$risk_layers) == 0) stop("Model is missing one or more of occurrence data, geographic units, or risk layers. \nPlease ensure these are present, then try again.")
      # Check that some ID column is supplied or identifiable from the geographic unit dataset.
      if(is.null(self$geog_id_col)){
        if(!quiet) cat("\nNo column was provided as the ID column of the geographic units. Intuiting column now...")
        if(sum(stringr::str_detect(names(self$geog_units), '(DISTRICT_NAME|REGION_NAME|GNIS_NA)')) == 0) {
          stop("No suitable ID column for geographic units could be automatically detected; please supply a column name.")
        } else {
          self$geog_id_col = names(self$geog_units)[stringr::str_detect(names(self$geog_units),'(DISTRICT_NAME|REGION_NAME|GNIS_NA)')][1]
          if(!quiet) cat(paste0("\nLikely column that will be used from here onwards: ",self$geog_id_col))
        }
      }

      # # Find the likely species name based on input data.
      # species_name = intuit_species_name(species_name, species_occurrence_data, quiet)

      # Summarize species occurrence data to spatial object.
      species_occurrence_data = sum_spatial_data_to_geog_units(self$occurrence_data,
                                                               self$geog_units,
                                                               self$geog_id_col,
                                                               sum_name = 'num_occ')

      # Add species occurrence summary numbers to geographic units.
      self$risk_estimates = self$geog_units |>
        dplyr::left_join(species_occurrence_data, by = self$geog_id_col)

      # Replace NA for species occurrence with 0.
      self$risk_estimates = self$risk_estimates |>
        dplyr::mutate(num_occ = tidyr::replace_na(num_occ, 0)) |>
        # Add bin for num_occ
        dplyr::mutate(num_occ_bin = as.numeric(cut(num_occ, n_bins))) |>
        dplyr::select(self$geog_id_col, num_occ, num_occ_bin)

      # Load, process, and join risk factor inputs to geog_units
      for(i in 1:length(self$risk_layers)){

        # Prepare input data to be joined to geographic units
        if('geom' %in% names(self$risk_layers[[i]]) | 'geometry' %in% names(self$risk_layers[[i]])){
        data_to_join = sum_spatial_data_to_geog_units(self$risk_layers[[i]],
                                                      self$geog_units,
                                                      self$geog_id_col,
                                                      sum_name = names(self$risk_layers[i]),
                                                      quiet)
        } else {
          data_to_join = self$risk_layers[[i]]
        }

        name_for_bin_col = names(self$risk_layers)[[i]]

        data_to_join[paste0(name_for_bin_col,'_bin')] = as.numeric(cut(data_to_join |> dplyr::select(-self$geog_id_col) |> dplyr::pull(),n_bins))

        data_to_join[paste0(name_for_bin_col,'_weighted')] = (data_to_join |>
                                                                dplyr::select(paste0(name_for_bin_col,'_bin')) |>
                                                                dplyr::pull()) * self$risk_weights[i]

        self$risk_estimates = dplyr::left_join(self$risk_estimates, data_to_join, by = self$geog_id_col) |>
          dplyr::mutate(across(names(data_to_join[,-1]), \(x) tidyr::replace_na(x, 0)))
      }

      # Calculate the overall risk estimate from our bins!
      self$risk_estimates = self$risk_estimates |>
        #Apply weights to bins temporarily, so they affect the summed_risk_factor_bins
        dplyr::mutate(summed_risk_factor_bins = rowSums(dplyr::across(dplyr::ends_with("_bin_weighted"))))

      self$risk_estimates = self$risk_estimates |> dplyr::mutate(risk_estimate_bin = as.numeric(cut(summed_risk_factor_bins, n_bins)))

      # self$risk_estimates = dplyr::select(self$geog_id_col,
      #                                       geometry,
      #                                       dplyr::everything())

      return(self$risk_estimates)
    }
  )
)
