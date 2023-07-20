#' Title
#'
#' @param dat Occurrence data that is either an sf object or has columns labelled Latitude and Longitude.
#' @param species_column Name of column that lists the Species' name.
#' @param location_variable Name of column that lists a short description of the occurrence location.
#' @param popup_or_label Choice of either popups or labels for leaflet map - defaults to labels.
#' @param choose_baselayer Optional; Select one of 'CartoDB', 'Streets' or 'Terrain.'
#' @param species_circles_palette Colour palette for occurrence points (e.g. 'Dark2', 'Spectral', etc.) - defaults to 'Dark2'.
#' @param bg_regions Optional; choose either natural resource polygons, either 'regions' or 'districts'.
#' @param label_bg_regions Optional; Add names to background regions, either T or F.
#' @param bg_region_palette Optional; Colour palette of background regions (e.g. 'Dark2', 'Spectral', etc.)
#' @param set_leaf_view Optional; Set custom view for map - requires concatenated vector of latitude, longitude, and zoom factor.
#' @param take_snapshot Optional; take snapshot of leaflet map and save to local machine, either T or F.
#' @param folder_for_snapshot Optional; set folder for snapshot of leaflet map.
#' @param snapshot_name Optional; Set name for snapshot .PNG file.
#' @param ... Additional arguments
#'
#' @return A leaflet map of BC showing species occurrence points, optionally adding background regions and taking a snapshot .PNG file.
#' @export
#'
#' @examples
#' # using the toy dataset 'lizards', make a map!
#'
#' my_map = occdat_leafmap(dat = lizards, location_variable = 'Location', bg_regions = 'regions')
#' my_map
#'
#' # Take a snapshot of the map and save it to disk.
#'
#' map_with_png = occdat_leafmap(dat = goldfish, location_variable = 'Location',
#' bg_regions = 'districts', bg_region_palette = 'Spectral',
#' take_snapshot = TRUE, folder_for_snapshot = './map_pngs/',
#' snapshot_name = 'Goldfish in British Columbia')
#'
#' map_with_png
#'
occdat_leafmap = function(dat,
                          species_column = 'Common_Name',
                          location_variable = NULL,
                          popup_or_label = 'label',
                          choose_baselayer = NULL,
                          species_circles_palette = 'Dark2',
                          bg_regions = NULL,
                          label_bg_regions = F,
                          bg_region_palette = 'RdBu',
                          set_leaf_view = NULL,
                          take_snapshot = FALSE,
                          folder_for_snapshot = NULL,
                          snapshot_name = NULL,
                          ...){

  # If the user has chosen to add background regions.
  if(!is.null(bg_regions) | label_bg_regions == T){

    if(!bg_regions %in% c("regions","districts")) stop("Please specify one of 'regions' or 'districts' for bg_regions")

    # If user has chosen regions...
    if(bg_regions == 'regions'){
      regs = bcmaps::nr_regions() |>
        dplyr::select(regname = REGION_NAME) |>
        sf::st_transform(crs = 4326)
    }
    # If user has chosen districts...
    if(bg_regions == 'districts'){
      regs = bcmaps::nr_districts() |>
        dplyr::select(regname = DISTRICT_NAME) |>
        sf::st_transform(crs = 4326)
    }

    dat = dat |>
      sf::st_join(regs, sf::st_intersects)

    regs = regs |>
      dplyr::left_join(dat |> sf::st_drop_geometry() |> dplyr::count(regname, name = "number_records"))

    number_regs_pal = leaflet::colorNumeric(
      palette = bg_region_palette,
      reverse = T,
      domain = regs$number_records
    )

    if(label_bg_regions == T){
      reg_label_coords = regs$geometry |>
        purrr::map( ~ {
          sf::st_bbox(.x) |>
            sf::st_as_sfc() |>
            sf::st_centroid() |>
            sf::st_coordinates() |>
            as.data.frame() |>
            setNames(c("Long","Lat"))
        }) |>
        dplyr::bind_rows()

      region_labels = regs |>
        sf::st_centroid() |>
        dplyr::mutate(regname = stringr::str_remove(regname, " Natural Resource [Region,District]")) |>
        dplyr::mutate(regname = stringr::str_replace_all(regname,"[ ,-]","<br>"))
    }
  }

  species_pal = leaflet::colorFactor(
    palette = species_circles_palette,
    domain = dat[[species_column]]
  )

  # Start map
  if(is.null(choose_baselayer)){
    l = leaflet::leaflet() |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB,group = "CartoDB") |>
      leaflet::addTiles(group = 'Streets') |>
      leaflet::addProviderTiles(leaflet::providers$Stamen.Terrain, group = "Terrain") |>
      leaflet::addLayersControl(baseGroups = c("CartoDB","Streets","Terrain"),
                                options = leaflet::layersControlOptions(collapsed = F),
                                position = 'bottomright')
  }

  if(!is.null(choose_baselayer)){
    if(choose_baselayer == 'CartoDB'){
      l = l |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB,group = "CartoDB") |>
        leaflet::addLayersControl(basegroups = "CartoDB",
                                  options = leaflet::layersControlOptions(collapsed = F),
                                  position = 'bottomright')
    }
    if(choose_baselayer == 'Streets'){
      l = l |>
        leaflet::addTiles(group = 'Streets') |>
        leaflet::addLayersControl(basegroups = "Streets",
                                  options = leaflet::layersControlOptions(collapsed = F),
                                  position = 'bottomright')
    }
    if(choose_baselayer == 'Terrain'){
      l = l |>
        leaflet::addProviderTiles(leaflet::providers$Stamen.Terrain, group = "Terrain") |>
        leaflet::addLayersControl(basegroups = "Terrain",
                                  options = leaflet::layersControlOptions(collapsed = F),
                                  position = 'bottomright')
    }
  }

  # Add a minimap in the top right of the leaflet map.
  l = l |>
    leaflet::addMiniMap(position = 'topright',
                        zoomLevelOffset = -4)

  # If user specified a certain view:
  if(!is.null(set_leaf_view)){
    if(length(set_leaf_view) != 3){
      stop("Please enter three pieces of information for set_leaf_view: lat, long, and zoom")
    }
    if(!is.numeric(set_leaf_view)){
      stop("All of lat, long and zoom in set_leaf_view must be numeric")
    }
    l = l |>
      leaflet::setView(lat = set_leaf_view[1], lng = set_leaf_view[2], zoom = set_leaf_view[3])
  }

  if(!is.null(bg_regions)){
    l = l |>
      leaflet::addPolygons(
        weight = 1,
        color = 'black',
        fillOpacity = 0.5,
        fillColor = ~number_regs_pal(number_records),
        data = regs
      )
  }

  if(label_bg_regions == T){
    # Names of regions in Grey behind.
    l = l |>
      leaflet::addLabelOnlyMarkers(
        label = ~lapply(regname, htmltools::HTML),
        labelOptions = leaflet::labelOptions(noHide = T,
                                    opacity = 0.4,
                                    textOnly = T,
                                    textsize = '16px',
                                    offset = c(20,30)
        ),
        data = region_labels
      ) |>
      leaflet::addLabelOnlyMarkers(
        label = ~paste0(number_records),
        labelOptions = leaflet::labelOptions(noHide = T,
                                    offset = c(40,50),
                                    direction = 'right'),
        data = region_labels
      )
  }

  # Number of species in each region.
  if(!is.null(location_variable)){

    # Replace NA in location variable.
    dat[which(is.na(dat[[location_variable]]) | dat[[location_variable]] == " "),][[location_variable]] = 'unspecified location'
    # Make labels.
    dat = dat |>
      dplyr::mutate(popup_label = paste(.data[[species_column]], "at", .data[[location_variable]]))
  }
  if(is.null(location_variable)){
    dat = dat |>
      dplyr::mutate(popup_label = paste0("Date: ",Date,"<br>",
                                         "Species: ",.data[[species_column]],"<br>",
                                         "Location: ",ifelse(is.na(Location),'Not specified',Location)))
  }

  if(popup_or_label == 'label'){
    l = l |>
      leaflet::addCircleMarkers(
        color = species_pal(dat[[species_column]]),
        fillColor = species_pal(dat[[species_column]]),
        weight = 0,
        fillOpacity = 1,
        label = ~lapply(dat$popup_label, htmltools::HTML),
        data = dat
      )
  }
  if(popup_or_label == 'popup'){
    l = l |>
      leaflet::addCircleMarkers(
        color = species_pal(dat[[species_column]]),
        fillColor = species_pal(dat[[species_column]]),
        weight = 0,
        fillOpacity = 1,
        popup = ~popup_label,
        data = dat
      )
  }
  l = l |>
    leaflet::addLegend(pal = species_pal,
              values = ~Species,
              position = 'bottomleft',
              opacity = 1,
              data = dat)

  if(label_bg_regions == T){
    l = l |>
      leaflet::addLegend(
        title = 'Number in Region',
        pal = number_regs_pal,
        values = regs$number_records,
        position = 'bottomleft'
      )
  }
  l = l |>
    leaflet::addScaleBar(position = 'bottomright')

  if(take_snapshot == T){

    # Set output folder
    if(is.null(folder_for_snapshot))  output_folder = getwd()
    if(!is.null(folder_for_snapshot)) output_folder = folder_for_snapshot
    if(grepl(output_folder,'.{1}$') != "/") output_folder = paste0(output_folder,'/')

    if(is.null(snapshot_name)){
      mapview::mapshot(l,
                       file = paste0(output_folder, 'Occurrence Map ',Sys.Date(),'.PNG'),
                       remove_controls = c("zoomControl"))
    }
    if(!is.null(snapshot_name)){

      mapview::mapshot(l,
                       file = paste0(output_folder, snapshot_name, ' ', Sys.Date(),'.PNG'),
                       remove_controls = c("zoomControl"))
    }
  }
  #Print out leaflet map
  l
}
