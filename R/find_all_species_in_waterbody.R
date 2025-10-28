#' Title Find all species occurrence records in a waterbody
#'
#' @param wb Waterbody in which to search
#' @param in_shiny Is this being run in a 'shiny' environment?
#' @param sources Which data sources should we look in? One or more of 'SPI','Old Aquatic','Incident Reports', and 'iNaturalist'
#' @param exclude Which kinds of organisms should we exclude? Default is 'Fungi' and 'Plantae'
#' @param excel_path Path (including excel file name) to master incident tracking sheet
#' @param sheet_name Name of aquatic sheet in excel file
#' @param excel_species_var Name of column with species common name in excel sheet
#' @param output_crs Output coordinate reference system (e.g. WGS 84, i.e. lat/long)
#' @param quiet Should this function run in a verbose fashion?
#' @param ...
#'
#' @return A table of species occurrence data
#' @export
#'
#' @examples \dontrun{}
find_all_species_in_waterbody = function(wb,
                                         in_shiny = F,
                                         sources = c("FDIS","Old Aquatic","Incident Reports","iNaturalist"),
                                         taxa_to_include = c('Actinopterygii','Mollusca','Fish'),
                                         excel_path = '5_Incidental Observations/Master Incidence Report Records.xlsx',
                                         sheet_name = 'Aquatic Reports',
                                         excel_species_var = 'Submitted_Common_Name',
                                         output_crs = 4326,
                                         quiet = T,
                                         ...){

  # Initial CRS of wb.
  init_crs = stringr::str_extract(unlist(sf::st_crs(wb))[1],'^[A-Z0-9]+')

  # If not NAD83 (the CRS of the BC Data Catalogue), reproject.
  if(init_crs != 'NAD83'){
    wb = sf::st_transform(wb, 3005)
  }

  search_results = list()

  if(quiet == F){
    cat("Looking for records in the Known BC Fish Observations and BC Fish Distributions layer on BC Warehouse\n")
  }

  if(in_shiny) shiny::incProgress(amount = 1/5, message = 'Searching FDIS dataset')

  if('FDIS' %in% sources){
    ## BCG Warehouse Data
    bcg_records = tryCatch(
      expr = bcdata::bcdc_query_geodata('aca81811-4b08-4382-9af7-204e0b9d2448') |>
        bcdata::filter(bcdata::INTERSECTS(wb)) |>
        bcdata::collect() |>
        sf::st_transform(crs = output_crs) |>
        dplyr::select(Date = 'OBSERVATION_DATE', Species = 'SPECIES_NAME', Location = 'GAZETTED_NAME') |>
        dplyr::mutate(DataSource = 'BCG fish layer') |>
        dplyr::mutate(Date = as.character(Date)) |>
        dplyr::select(DataSource, dplyr::everything()),
      error = function(e) NULL
    )

    if(quiet == F){
      if(!is.null(bcg_records)){
        cat(paste0(nrow(bcg_records)," records...\n"))
      } else {
        cat("No records here!\n")
      }
    }

    search_results = append(search_results, list(bcg_records))
  }

  if(in_shiny) shiny::incProgress(amount = 1/5, message = 'Searching old Aquatic Invasives layer')

  if('Old Aquatic' %in% sources){
    if(quiet == F){
      cat("Looking for records in the (deprecated) Aquatic Invasive Species of British Columbia layer on BC Warehouse...\n")
    }

    # Look in the old AIS layer
    old_ais = tryCatch(
      expr = suppressWarnings(
        bcdata::bcdc_query_geodata('d9613096-b2fe-43b4-9be1-d82a3b805082') |>
          bcdata::filter(bcdata::INTERSECTS(wb)) |>
          bcdata::collect() |>
          sf::st_transform(crs = output_crs) |>
          dplyr::mutate(Species = stringr::str_to_title(ENGLISH_NAME)) |>
          dplyr::mutate(Location = ifelse(is.na(BCGNIS_NAME),LOCATION_INFORMATION,stringr::str_to_title(BCGNIS_NAME))) |>
          dplyr::filter(TAXONOMIC_GROUP %in% dplyr::any_of(taxa_to_include)) |>
          dplyr::select(Species, Date = COLLECTION_DATE, Location) |>
          dplyr::mutate(Date = as.character(Date)) |>
          dplyr::mutate(DataSource = 'Old BCG AIS layer') |>
          dplyr::select(DataSource, dplyr::everything())
      ),
      error = function(e) NULL
    )

    if(quiet == F){
      if(!is.null(old_ais)){
        cat(paste0(nrow(old_ais)," records...\n"))
      } else {
        cat("No records here!\n")
      }
    }

    search_results = append(search_results, list(old_ais))
  }

  if(in_shiny) shiny::incProgress(amount = 1/5, message = 'Searching incident report excel file')

  if('Incident Reports' %in% sources){
    if(!is.null(excel_path) & !is.null(sheet_name) & !is.null(excel_species_var)){

      if(quiet == F){
        cat("Looking for records in the Master Incidence Report Records excel file...\n")
      }

      if(stringr::str_detect(excel_path,"^5_Incidental Observations/")){
        # This is likely our folder; prepend the full LAN filepath.
        excel_path = paste0("\\\\SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/SPECIES/", excel_path)
      }

      tryCatch(
        expr = {
          excel_dat = readxl::read_excel(path = excel_path,
                                         sheet = sheet_name) |>
            dplyr::rename(Species = excel_species_var) |>
            dplyr::select(Species,Submitted_Scientific_Name,Date,Location,Latitude,Longitude)

          initial_nrow_inc = nrow(excel_dat)

          # Filter out rows with no lat/long
          inc = suppressWarnings(
            excel_dat |>
              dplyr::mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude)) |>
              dplyr::mutate(Date = as.character(Date)) |>
              dplyr::mutate(stringr::str_extract(Date, '[0-9]{4}-[0-9]{2}-[0-9]{2}')) |>
              dplyr::select(Species,Submitted_Scientific_Name,Date,Location,Latitude,Longitude) |>
              dplyr::mutate(DataSource = 'Incidental Observation') |>
              dplyr::select(DataSource, dplyr::everything()) |>
              dplyr::filter(!is.na(Latitude),!is.na(Longitude))
          )

          if(quiet == F){
            if(nrow(inc) == 0 & initial_nrow_inc > 0){
              cat("The excel record(s) were filtered out due to lacking latitude and longitude coordinates!\n")
              if(is.null(old_ais) & is.null(bcg_records)){
                stop(paste0("No records found for ",common_names[3]))
              }
            }
          }

          inc = inc |>
            dplyr::select(-Submitted_Scientific_Name) |>
            sf::st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) |>
            sf::st_transform(3005) |>
            dplyr::filter(sf::st_intersects(geometry, wb, sparse = F)) |>
            sf::st_transform(crs = output_crs)

          post_latlon_filter_nrow_inc = nrow(inc)

          # If we lost any rows in the master incidental occurrence sheet because of crummy lat/long data,
          # notify.
          if(quiet == F){
            if(initial_nrow_inc != post_latlon_filter_nrow_inc){
              cat(paste0("Note: ",
                         initial_nrow_inc-post_latlon_filter_nrow_inc,
                         " row(s) dropped from Master incidental sheet due to non-numeric coordinate data\n"))
            }
          }

          excel_dat = inc
        },
        error = function(e) NULL
      )

      if(quiet == F){
        if(!is.null(inc)){
          cat(paste0(nrow(excel_dat)," records...\n"))
        } else {
          cat("No records here!\n")
        }
      }

      search_results = append(search_results, list(excel_dat))
    }
  }

  if(in_shiny) shiny::incProgress(amount = 1/5, message = 'Searching iNaturalist')

  if('iNaturalist' %in% sources){

    if(quiet == F){
      cat("Looking for records on iNaturalist for just BC...\n")
    }

    bounds_wgs_84 = sf::st_transform(wb, 4326) |>
      sf::st_bbox(wb) |>
      as.matrix() |>
      as.data.frame() |>
      dplyr::slice(2,1,4,3)

    # inat = tryCatch(
    #   expr = suppressWarnings(
    #     # Cycle through taxa to search, binding rows.
    #     rinat_data = purrr::map(taxa_to_include){
    #       tryCatch(
    #         expr = rinat::get_inat_obs(
    #           #place_id = '7085',
    #           # bounds = bounds_wgs_84$V1,
    #           query = paste0(wb$Waterbody),
    #           quality = 'research',
    #           maxresults = 10000
    #         ),
    #         error = function(e) NULL
    #       )}
    #      |> dplyr::bind_rows()
    #     # rinat::get_inat_obs(
    #     #   #place_id = '7085',
    #     #   # bounds = bounds_wgs_84$V1,
    #     #   query = paste0(wb$Waterbody),
    #     #   quality = 'research',
    #     #   maxresults = 10000
    #     # )
    #     rinat_data |>
    #       dplyr::summarise(DataSource = 'iNaturalist',
    #                        Date = stringr::str_extract(observed_on_string, '[0-9]{4}-[0-9]{2}-[0-9]{2}'),
    #                        Species = common_name,
    #                        iNat_user = user_login,
    #                        iNat_report_id = id,
    #                        Location = place_guess,
    #                        iconic_taxon_name,
    #                        latitude,
    #                        longitude) |>
    #       sf::st_as_sf(coords = c('longitude','latitude'),
    #                    crs = 4326)
    #   ),
    #   error = function(e) NULL
    # )

    # if('Fungi' %in% exclude){
    #   inat = inat |>
    #     dplyr::filter(iconic_taxon_name != 'Fungi')
    # }
    # if('Plantae' %in% exclude){
    #   inat = inat |>
    #     dplyr::filter(iconic_taxon_name != 'Plantae')
    # }

    # Updated this section to loop through the taxa we are searching for,
    # which really speeds up the query.
    inat = tryCatch(
      expr = suppressWarnings(
        # Cycle through taxa to search, binding rows.
        purrr::map(taxa_to_include, ~ {
          tryCatch(
            expr = tidyr::as_tibble(
              rinat::get_inat_obs(
                #place_id = '7085',
                # bounds = bounds_wgs_84$V1,
                taxon_name = .x,
                query = paste0(wb$GNIS_NAME_1),
                quality = 'research',
                maxresults = 10000
              )),
            error = function(e) NULL
          )})
        |>
          dplyr::bind_rows() |>
          dplyr::summarise(DataSource = 'iNaturalist',
                           Date = stringr::str_extract(observed_on_string, '[0-9]{4}-[0-9]{2}-[0-9]{2}'),
                           Species = common_name,
                           iNat_user = user_login,
                           iNat_report_id = id,
                           Location = place_guess,
                           iconic_taxon_name,
                           latitude,
                           longitude) |>
          sf::st_as_sf(coords = c('longitude','latitude'),
                       crs = 4326)
      ),
      error = function(e) NULL
    )

    if(quiet == F){
      if(!is.null(inat)){
        # Filter taxa
        inat = inat |>
          dplyr::filter(iconic_taxon_name %in% dplyr::any_of(taxa_to_include)) |>
          dplyr::select(-iconic_taxon_name)

        # Filter for our wb
        inat = inat |> sf::st_filter(wb)
        cat(paste0(nrow(inat)," records...\n"))
      } else {
        cat("No records here! \nNote that this may because there were more than 10,000 records for the waterbody name provided.\nIf you suspect there should be records, \ntry using {rinat} to supplement results from this function.")
      }
    }

    search_results = append(search_results, list(inat))
  }
  ## Combine datasets

  if(in_shiny) shiny::incProgress(amount = 1/5, message = 'Combining results...')

  dataset = search_results |>
    dplyr::bind_rows()

  # Ensure we have filtered the dataset with the wb polygon.
  # browser()
  dataset = sf::st_filter(dataset, sf::st_transform(wb,4326))

  if(nrow(dataset) == 0) stop("No records found for this species name")

  # Clean up Species name a bit.
  dataset$Species = stringr::str_squish(stringr::str_to_title(dataset$Species))

  if(quiet == FALSE){
    cat(paste0(nrow(dataset), " rows prior to dropping duplicates\n"))
  }
  # Make sure rows are unique
  dataset = dataset |>
    dplyr::filter(!duplicated(paste0(Date,Species,Location,geometry)))

  if(quiet == FALSE){
    cat(paste0(nrow(dataset), " rows after dropping duplicates\n"))
  }

  #if(!quiet) beepr::beep(5)

  return(dataset)
}
