#' Grab Aquatic BC Occurrence Data
#'
#' @param common_names A vector of common names for one or more species of interest.
#' @param sources Which layers to search for occurrence data; one or more of 'FDIS','Old Aquatic','Incident Reports', and 'iNaturalist'
#' @param excel_path Optional; path to your excel file (must include columns Date, Species, Scientific, Location, Latitude and Longitude)
#' @param sheet_name Optional; if you read in your own excel file, what is the excel sheet name?
#' @param excel_species_var Optional; if you read in your own excel file, what is the name of the column listing common names?
#' @param confirmed_only Should we only pull incident reports that have been confirmed?
#' @param output_crs Coordinate Reference System (i.e. projection system); defaults to 4326 (WGS 84), another common option for BC is 3005.
#' @param quiet Boolean to determine amount of feedback given by function
#' @param ... Additional arguments
#' @param in_shiny Is this function being run in shiny? If so, give incremental progress updates.
#' @param remove_no_coord_rows Should rows from our excel tracking sheet be dropped if they are lacking lat/lon coordinates?
#'
#' @return Aquatic occurrence data in British Columbia; optional to add in one's own excel file from local machine.
#' @export
#'
#' @examples
#' # Search for a single species (perhaps with various possible spellings)
#' pike_occ = grab_aq_occ_data(common_names = c("pike","northern pike"))
#'
#' # Search for multiple species in one go!
#' invasive_fish = grab_aq_occ_data(common_names = c("black crappie","bullhead",
#' "black bullhead","brown bullhead","yellow bullhead"))
grab_aq_occ_data = function(common_names = NULL,
                            sources = c("FDIS","Old Aquatic","Incident Reports","iNaturalist"),
                            excel_path = '5_Incidental Observations/Master Incidence Report Records.xlsx',
                            sheet_name = 'Aquatic Reports',
                            excel_species_var = 'Submitted_Common_Name',
                            confirmed_only = TRUE,
                            output_crs = 4326,
                            quiet = F,
                            in_shiny = F,
                            remove_no_coord_rows = F,
                            ...){
  # Must specify common name or scientific name, as character string
  if(is.null(common_names)) stop("Enter the species' common name")
  if(!is.character(common_names)) stop("Species name must be a character string")

  # Expand search when necessary to catch alternate common names.
  og_common_name = common_names
  if(stringr::str_detect(common_names,'umpkinseed$')) common_names = c(common_names,'Pumpkinseed Sunfish')

  # expand common names to all kinds of CaPiTaLiZaTiOn.
  common_names = c(stringr::str_to_lower(common_names),
                   stringr::str_to_sentence(common_names),
                   stringr::str_to_title(common_names),
                   stringr::str_to_upper(common_names),
                   paste0(common_names,' '))

  search_results = list()

  if(quiet == F){
    cat("Looking for records in the Known BC Fish Observations and BC Fish Distributions layer on BC Warehouse...\n")
  }

  if(in_shiny) shiny::incProgress(amount = 1/5, message = 'Searching FDIS dataset')

  common_names_title = common_names

  if('FDIS' %in% sources){

    # cql_query = paste0("SPECIES_NAME like ",common_names,"%")

    cql_query = stringr::str_squish(paste0('SPECIES_NAME LIKE ',paste0("'",common_names_title,"'", collapse = ' or SPECIES_NAME LIKE ')))

    ## BCG Warehouse Data
    bcg_records = tryCatch(
      expr = bcdata::bcdc_query_geodata('aca81811-4b08-4382-9af7-204e0b9d2448') |>
        # bcdata::filter(SPECIES_NAME %in% common_names) |>
        bcdata::filter(bcdata:::CQL(cql_query)) |>
        bcdata::filter(POINT_TYPE_CODE == 'Observation') |>
        # bcdata::filter(bcdata:::CQL("SPECIES_NAME LIKE '% shad' OR SPECIES_NAME LIKE '% shad %'")) |>
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
    # cql_query = paste0("ENGLISH_NAME LIKE '",common_names_title,"'  or ENGLISH_NAME LIKE '% ",common_names_title,"' or ENGLISH_NAME LIKE '% ",common_names_title," %'")
    cql_query = stringr::str_squish(paste0('ENGLISH_NAME LIKE ',paste0("'",common_names_title,"'", collapse = ' or ENGLISH_NAME LIKE ')))

    old_ais = tryCatch(
      expr = suppressWarnings(
        bcdata::bcdc_query_geodata('d9613096-b2fe-43b4-9be1-d82a3b805082') |>
          # bcdata::filter(ENGLISH_NAME %in% common_names) |>
          bcdata::filter(bcdata:::CQL(cql_query)) |>
          bcdata::collect() |>
          sf::st_transform(crs = output_crs) |>
          dplyr::mutate(Species = stringr::str_to_title(ENGLISH_NAME)) |>
          dplyr::mutate(Location = ifelse(is.na(BCGNIS_NAME),LOCATION_INFORMATION,stringr::str_to_title(BCGNIS_NAME))) |>
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
        excel_path = paste0("\\\\SFP.IDIR.BCGOV/S140/S40203/RSD_ FISH & AQUATIC HABITAT BRANCH/General/2 SCIENCE - Invasives/SPECIES/", excel_path)
      }

      tryCatch(
        expr = {
          excel_dat = readxl::read_excel(path = excel_path,
                                         sheet = sheet_name) |>
            dplyr::rename(Species = excel_species_var) |>
            dplyr::mutate(Species = stringr::str_to_title(Species)) |>
            dplyr::filter(stringr::str_detect(Species,paste0("(",paste0(common_names,collapse = '|'),")"))) |>
            dplyr::select(Species,Submitted_Scientific_Name,Date,Location,Latitude,Longitude,ID_Confirmation)

          initial_nrow_inc = nrow(excel_dat)

          # Filter out rows with no lat/long
          inc = suppressWarnings(
            excel_dat |>
              dplyr::mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude)) |>
              dplyr::mutate(Date = as.character(Date)) |>
              dplyr::mutate(stringr::str_extract(Date, '[0-9]{4}-[0-9]{2}-[0-9]{2}')) |>
              dplyr::select(Species,Submitted_Scientific_Name,Date,Location,Latitude,Longitude,ID_Confirmation) |>
              dplyr::mutate(DataSource = 'Incidental Observation') |>
              dplyr::select(DataSource, dplyr::everything()) #|>
              # dplyr::filter(!is.na(Latitude),!is.na(Longitude))
          )

          if(remove_no_coord_rows){
            inc = inc |>
              dplyr::filter(!is.na(Latitude),!is.na(Longitude))
          } else {

            if(nrow(inc[is.na(inc$Latitude),]) > 0){
              cat(paste0(nrow(inc[is.na(inc$Latitude),])," records lacked coordinate data; artificially set their coordinates to 0,0\n"))
            }

            inc = inc |>
              dplyr::mutate(Latitude = tidyr::replace_na(Latitude, 0),
                            Longitude = tidyr::replace_na(Longitude, 0))
          }

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

      # Remove unconfirmed reports!
      if(confirmed_only){
        if(quiet == F) cat(paste0("\nRemoving unconfirmed reports (",nrow(excel_dat[excel_dat$ID_Confirmation != 'Confirmed',])," reports)...\n"))
        excel_dat = excel_dat[excel_dat$ID_Confirmation == 'Confirmed',]
      }

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

    inat = tryCatch(
        expr = suppressWarnings(
          rinat::get_inat_obs(query = common_names[3],
                        place_id = '7085',
                        quality = 'research',
                        maxresults = 10000) |>
      dplyr::filter(common_name %in% common_names) |>
      dplyr::summarise(DataSource = 'iNaturalist',
                    Date = stringr::str_extract(observed_on_string, '[0-9]{4}-[0-9]{2}-[0-9]{2}'),
                    Species = common_names[3],
                    iNat_user = user_login,
                    iNat_report_id = id,
                    Location = place_guess,
                    latitude,
                    longitude) |>
      sf::st_as_sf(coords = c('longitude','latitude'),
                   crs = 4326)
      ),
      error = function(e) NULL
    )

    if(quiet == F){
      if(!is.null(inat)){
        cat(paste0(nrow(inat)," records...\n"))
        if(nrow(inat) == 0){
          cat("Please note that the common name must match iNaturalist's spelling conventions exactly...\n")
        }
      } else {
        cat("No records here!\n")
      }
    }

    search_results = append(search_results, list(inat))
  }
  ## Combine datasets

  if(in_shiny) shiny::incProgress(amount = 1/5, message = 'Combining results...')

  dataset = search_results |>
    dplyr::bind_rows()

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

  # Check that we aren't amalgamating species that we shouldn't! E.g. Northern
  # Pike and Northern Pikeminnow.
  if(stringr::str_to_lower(og_common_name) == 'northern pike') dataset = dataset[dataset$Species != "Northern Pikeminnow",]

  # Ensure we are returning the originally searched common name.
  dataset = dataset |>
    dplyr::mutate(Species = stringr::str_to_title(og_common_name))

  return(dataset)
}
