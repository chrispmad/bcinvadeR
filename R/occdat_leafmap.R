occdat_leafmap = function(dat,
                          species_column = 'Species',
                          location_variable = NULL,
                          popup_or_label = 'label',
                          choose_baselayer = NULL,
                          species_circles_palette = 'Dark2',
                          background_regions = NULL,
                          label_background_regions = F,
                          background_region_palette = 'RdBu',
                          set_leaf_view = NULL,
                          take_snapshot = FALSE,
                          folder_for_snapshot = NULL,
                          snapshot_name = NULL,
                          ...){
  # browser()
  # If the user has chosen to add background regions.
  if(!is.null(background_regions)){

    if(!background_regions %in% c("regions","districts")) stop("Please specify one of 'regions' or 'districts' for background_regions")

    if(background_regions == 'regions'){
      regs = bcmaps::nr_regions() |>
        dplyr::select(regname = REGION_NAME) |>
        sf::st_transform(crs = 4326)
    }
    if(background_regions == 'districts'){
      regs = bcmaps::nr_districts() |>
        dplyr::select(regname = DISTRICT_NAME) |>
        sf::st_transform(crs = 4326)
    }

    dat = dat |>
      sf::st_join(regs, sf::st_intersects)

    regs = regs |>
      dplyr::left_join(dat |> sf::st_drop_geometry() |> dplyr::count(regname, name = "number_records"))

    number_regs_pal = leaflet::colorNumeric(
      palette = background_region_palette,
      reverse = T,
      domain = regs$number_records
    )

    if(label_background_regions == T){
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

  if(!is.null(background_regions)){
    l = l |>
      leaflet::addPolygons(
        weight = 1,
        color = 'black',
        fillOpacity = 0.5,
        fillColor = ~number_regs_pal(number_records),
        data = regs
      )
  }

  if(label_background_regions == T){
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
      dplyr::mutate(popup_label = .data[[species_column]])
  }

  if(popup_or_label == 'label'){
    l = l |>
      leaflet::addCircleMarkers(
        color = species_pal(dat[[species_column]]),
        fillColor = species_pal(dat[[species_column]]),
        weight = 0,
        fillOpacity = 1,
        label = ~popup_label,
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

  if(label_background_regions == T){
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
