grab_aq_occ_data = function(common_names = NULL,
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

    bcg_records = tryCatch(
      expr = bcdata::bcdc_query_geodata('known-bc-fish-observations-and-bc-fish-distributions') |>
        dplyr::filter(SPECIES_NAME %in% dplyr::all_of(common_names)) |>
        bcdata::collect() |>
        sf::st_transform(crs = output_crs) |>
        dplyr::select(Date = OBSERVATION_DATE, Species = SPECIES_NAME, Location = GAZETTED_NAME) |>
        dplyr::mutate(DataSource = 'BCG fish layer') |>
        dplyr::mutate(Date = as.character(Date)) |>
        dplyr::select(DataSource, dplyr::everything()),
      error = function(e) "No records found"
    )

    # Look in the old AIS layer
    old_ais = tryCatch(
      expr = bcdata::bcdc_query_geodata('aquatic-invasive-species-of-british-columbia') |>
        dplyr::filter(ENGLISH_NAME %in% dplyr::all_of(common_names)) |>
        bcdata::collect() |>
        sf::st_transform(crs = output_crs) |>
        dplyr::mutate(Species = stringr::str_to_title(ENGLISH_NAME)) |>
        dplyr::mutate(Location = ifelse(is.na(BCGNIS_NAME),LOCATION_INFORMATION,stringr::str_to_title(BCGNIS_NAME))) |>
        dplyr::select(Species, Date = COLLECTION_DATE, Location) |>
        dplyr::mutate(DataSource = 'Old BCG AIS layer') |>
        dplyr::select(DataSource, dplyr::everything()),
      error = function(e) "No records found"
    )

    # Combine the BCG fish layer with the old AIS aquatic layer
    if(!is.vector(bcg_records) & !is.vector(old_ais)){
      bcg_records = dplyr::bind_rows(bcg_records, old_ais)
    }
    if(is.vector(bcg_records) & !is.vector(old_ais)){
      bcg_records = old_ais
    }

    inc = readxl::read_excel(path = excel_path,
                     sheet = sheet_name) |>
      dplyr::filter(Species %in% dplyr::all_of(common_names))

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

    # Make sure rows are unique (could be duplication coming
    # from web-hosted old aquatics layer and the aquatics incidence
    # report sheet)
    dat = dat |>
      dplyr::distinct()

    return(dat)
}
