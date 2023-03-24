grab_terr_occ_data = function(common_names = NULL,
                              scientific_name = NULL,
                              excel_path = NULL,
                              sheet_name = NULL,
                              as_sf = FALSE,
                              output_crs = 4326,
                              ...){

  # Must specify common name or scientific name, as character string
  if(is.null(common_names)) stop("Enter the species' common name")
  if(!is.character(common_names)) stop("Species name must be a character string")
  # Must use a valid CRS code.
  if(as_sf == TRUE){
    if(!is.numeric(output_crs)) stop("Output CRS code must be numeric")
  }

  # expand common names to all kinds of CaPiTaLiZaTiOn.
  common_names = c(stringr::str_to_lower(common_names),
                   stringr::str_to_sentence(common_names),
                   stringr::str_to_title(common_names),
                   stringr::str_to_upper(common_names),
                   paste0(common_names,' '))

  ## BCG Warehouse Data (Aquatics from the WHSE layer, Terrestrial from the SPI layer)
  ## search with common names first.
  bcg_records = tryCatch(
    expr = bcdata::bcdc_query_geodata('https://catalogue.data.gov.bc.ca/dataset/7d5a14c4-3b6e-4c15-980b-68ee68796dbe') |>
      dplyr::filter(SPECIES_ENGLISH_NAME %in% dplyr::all_of(common_names)) |>
      bcdata::collect() |>
      sf::st_transform(crs = output_crs) |>
      dplyr::select(Date = OBSERVATION_DATE, Species = SPECIES_ENGLISH_NAME,
                    Scientific = SCIENTIFIC_NAME, Location = OBSERVATION_LOCATION) |>
      dplyr::mutate(DataSource = 'SPI Wildlife layer') |>
      dplyr::mutate(Date = as.character(Date)) |>
      dplyr::select(DataSource, dplyr::everything()),
    error = function(e) "No records found"
  )

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
      error = function(e) "No records found"
    )

    # If we got results on both searches (common and scientific names)
    if(!is.vector(bcg_records) & !is.vector(bcg_records_scientific)){
      bcg_records = bcg_records |>
        dplyr::bind_rows(bcg_records_scientific) |>
        dplyr::distinct()
    } # If we only got results from the scientific name query
    if(is.vector(bcg_records) & !is.vector(bcg_records_scientific)){
      bcg_records = bcg_records_scientific
    }
  }

  ## Incidental occurrence reports (from the I: drive)
  if(!is.null(excel_path) & !is.null(sheet_name)){
    inc = readxl::read_excel(path = excel_path,
                             sheet = sheet_name) |>
      dplyr::filter(Species %in% dplyr::all_of(common_names))
  } else {
    inc = tibble(a = 0)[0,]
  }

  # If the name didn't return any results, stop and inform user here.
  if(nrow(inc) == 0 & is.vector(bcg_records)) stop("No records found for this species name")

  initial_nrow_inc = nrow(inc)

  inc = inc |>
    dplyr::mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude)) |>
    dplyr::mutate(Date = as.character(Date)) |>
    dplyr::select(Species,Scientific,Date,Location,Latitude,Longitude) |>
    dplyr::mutate(DataSource = 'Incidental Observation') |>
    dplyr::select(DataSource, dplyr::everything())

  post_latlon_filter_nrow_inc = nrow(inc)

  # If we lost any rows in the master incidental occurrence sheet because of crummy lat/long data,
  # notify.
  if(initial_nrow_inc != post_latlon_filter_nrow_inc){
    warning(paste0("Note: ",
                   initial_nrow_inc-post_latlon_filter_nrow_inc,
                   " rows dropped from Master incidental sheet due to non-numeric lat/long data"))
  }

  ## Combine datasets
  if(as_sf == TRUE & !is.vector(bcg_records)){
    inc = inc |>
      dplyr::filter(!is.na(Latitude),!is.na(Longitude)) |>
      sf::st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) |>
      sf::st_transform(crs = output_crs)
  }
  if(as_sf == FALSE & !is.vector(bcg_records)){
    bcg_records = bcg_records |>
      dplyr::bind_cols(sf::st_coordinates(bcg_records)) |>
      sf::st_drop_geometry() |>
      dplyr::rename("Longitude" = X, "Latitude" = Y)
  }

  # If BCG warehouse had no records, but incidental report tracker sheet did...
  if(is.vector(bcg_records)){
    dat = inc
  }
  # If both BCG warehouse and incident sheet had results
  if(!is.vector(bcg_records)){
    dat = bcg_records |>
      dplyr::bind_rows(inc) |>
      dplyr::mutate(Location = stringr::str_to_title(Location)) |>
      dplyr::mutate(Location = replace(Location, Location == 'NA' | Location == 'Na' | Location == '', NA))
  }
  # If only BCG warehouse had results
  if(nrow(inc) == 0 & !is.vector(bcg_records)){
    dat = bcg_records
  }

  # Clean up Species name a bit.
  dat = dat |>
    dplyr::mutate(Species = stringr::str_to_title(Species))

  # Make sure rows are unique
  dat = dat |>
    dplyr::distinct()
  return(dat)
}
