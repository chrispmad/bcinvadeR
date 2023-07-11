#' Grab Terrestrial BC Occurrence Data
#'
#' @param common_names A vector of common names for one or more species of interest.
#' @param scientific_name A vector of scientific names for one or more species of interest.
#' @param excel_path Optional; path to your excel file (must include columns Date, Species, Scientific, Location, Latitude and Longitude)
#' @param sheet_name Optional; if you read in your own excel file, what is the excel sheet name?
#' @param excel_species_var Optional; if you read in your own excel file, what is the name of the column listing common names?
#' @param output_crs Coordinate Reference System (i.e. projection system); defaults to 4326 (WGS 84), another common option for BC is 3005.
#' @param quiet Boolean to determine amount of feedback given by function
#' @param ... Additional arguments
#'
#' @return Terrestrial occurrence data in British Columbia; optional to add in one's own excel file from local machine.
#' @export
#'
#' @examples
#' # Search for a single species (perhaps with various possible spellings)
#' liz_occ = grab_terr_occ_data(common_names = c("wall lizard","common wall lizard"),
#' scientific_name = 'Podarcis muralis')
#'
#' # Search for multiple species in one go! Here we look for wall lizards and Eastern grey squirrels.
#' invasives = grab_terr_occ_data(common_names = c("common wall lizard"),
#' scientific_name = c('Sciurus carolinensis'))
#'
grab_terr_occ_data = function(common_names = NULL,
                              scientific_name = NULL,
                              excel_path = 'J/2 SCIENCE - Invasives/SPECIES/5_Incidental Observations/Master Incidence Report Records.xlsx',
                              sheet_name = 'Aquatic Reports',
                              excel_species_var = NULL,
                              output_crs = 4326,
                              ...){
  # Must specify common name or scientific name, as character string
  if(is.null(common_names)) stop("Enter the species' common name")
  if(!is.character(common_names)) stop("Species name must be a character string")


  # expand common names to all kinds of CaPiTaLiZaTiOn.
  common_names = c(stringr::str_to_lower(common_names),
                   stringr::str_to_sentence(common_names),
                   stringr::str_to_title(common_names),
                   stringr::str_to_upper(common_names),
                   paste0(common_names,' '))

  search_results = list()

  ## BCG Warehouse Data
  bcg_records = tryCatch(
    expr = bcdata::bcdc_query_geodata('https://catalogue.data.gov.bc.ca/dataset/7d5a14c4-3b6e-4c15-980b-68ee68796dbe') |>
      dplyr::filter(SPECIES_ENGLISH_NAME %in% common_names) |>
      bcdata::collect() |>
      sf::st_transform(crs = output_crs) |>
      dplyr::select(Date = OBSERVATION_DATE, Species = SPECIES_ENGLISH_NAME,
                    Scientific = SCIENTIFIC_NAME, Location = OBSERVATION_LOCATION) |>
      dplyr::mutate(DataSource = 'SPI Wildlife layer') |>
      dplyr::mutate(Date = as.character(Date)) |>
      dplyr::select(DataSource, dplyr::everything()),
    error = function(e) NULL
  )

  search_results = append(search_results, list(bcg_records))

  if(!is.null(scientific_name)){
    bcg_records_scientific = tryCatch(
      expr = bcdata::bcdc_query_geodata('https://catalogue.data.gov.bc.ca/dataset/7d5a14c4-3b6e-4c15-980b-68ee68796dbe') |>
        dplyr::filter(SCIENTIFIC_NAME == scientific_name) |>
        bcdata::collect() |>
        sf::st_transform(crs = output_crs) |>
        dplyr::select(Date = OBSERVATION_DATE, Species = SPECIES_ENGLISH_NAME,
                      Scientific = SCIENTIFIC_NAME, Location = OBSERVATION_LOCATION) |>
        dplyr::mutate(DataSource = 'SPI Wildlife layer') |>
        dplyr::mutate(Date = as.character(Date)) |>
        dplyr::select(DataSource, dplyr::everything()),
      error = function(e) NULL
    )
    search_results = append(search_results, list(bcg_records_scientific))
  }

  ## Incidental occurrence reports (from the I: drive)
  if(!is.null(excel_path) & !is.null(sheet_name) & !is.null(excel_species_var)){
    inc = tryCatch(
      expr = {
        excel_dat = readxl::read_excel(path = excel_path,
                                       sheet = sheet_name) |>
          dplyr::filter(!!rlang::sym(excel_species_var) %in% dplyr::all_of(common_names))

        initial_nrow_inc = nrow(excel_dat)

        excel_dat = excel_dat |>
          dplyr::mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude)) |>
          dplyr::mutate(Date = as.character(Date)) |>
          dplyr::rename(Species = excel_species_var) |>
          dplyr::select(Species,Scientific,Date,Location,Latitude,Longitude) |>
          dplyr::mutate(DataSource = 'Incidental Observation') |>
          dplyr::select(DataSource, dplyr::everything()) |>
          dplyr::filter(!is.na(Latitude),!is.na(Longitude)) |>
          sf::st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) |>
          sf::st_transform(crs = output_crs)

        post_latlon_filter_nrow_inc = nrow(excel_dat)

        if(initial_nrow_inc != post_latlon_filter_nrow_inc){
          warning(paste0("Note: ",
                         initial_nrow_inc-post_latlon_filter_nrow_inc,
                         " rows dropped from Master incidental sheet due to non-numeric lat/long data"))
        }

        excel_dat
      },
      error = function(e) NULL
    )
    search_results = append(search_results, list(inc))
  }

  ## Combine datasets
  dataset = search_results |>
    dplyr::bind_rows()

  if(nrow(dataset) == 0) stop("No records found for this species name")

  # Clean up Species name a bit.
  dataset$Species = stringr::str_squish(stringr::str_to_title(dataset$Species))

  if(quiet == 'false'){
    cat(paste0(nrow(dataset), " rows prior to dropping duplicates"))
  }
  # Make sure rows are unique
  dataset = dataset |>
    dplyr::distinct()

  if(quiet == 'false'){
    cat(paste0(nrow(dataset), " rows after dropping duplicates"))
  }
  return(dataset)
}
