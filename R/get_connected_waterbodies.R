# This function searches x kilometers around waterbody y and finds
# all waterbodies that intersect.

#' Title
#'
#' @param waterbody_name Name of the waterbody on which to base a connected network of waterbodies
#' @param waterbody_polygon (Optional) Polygon of your chosen waterbody; include if you have one!
#' @param waterbody_coordinates (Required unless polygon included) vector of WGS 84 coordinates, in format: c(longitude, latitude)
#' @param waterbody_type For now, only lakes are implemented
#' @param search_radius How far from the waterbody to search (in kilometers)
#' @param quiet Should this function return copious updates as it runs?
#'
#' @return A spatial table (sf object) of all lakes, streams and rivers that intersect with your chosen waterbody
#' @export
#'
#' @examples
#' get_connected_waterbodies('Stave Lake',
#' waterbody_coordinates = c(-122.2837065448555,49.37115621966073),
#' quiet = F)
get_connected_waterbodies = function(
    waterbody_name = NULL,
    waterbody_polygon = NULL,
    waterbody_coordinates = NULL,
    waterbody_type = 'lake',
    search_radius = 10,
    quiet = T){

  if(is.null(waterbody_name) & is.null(waterbody_polygon)) stop("Please enter a waterbody name or polygon")

  if(!is.null(waterbody_polygon)){
    wb = waterbody_polygon
    likely_name_column = names(wb |> dplyr::select(dplyr::where(~any(grepl('Lake', .)))))[1]
    wb = wb |>
      dplyr::rename(name := likely_name_column)
  } else {
    if(!is.null(waterbody_name)){
      if(is.null(waterbody_coordinates)) stop("Due to shared waterbody names across BC, we need coordinates to specify which waterbody to focus on. \nPlease supply coordinates in this format: c(-120, 49)")

      waterbody_coordinates = bcinvadeR::clean_coords(waterbody_coordinates)

      # wb_ln = waterbody_coordinates[1]
      # wb_lt = waterbody_coordinates[2]
      wb_ln = waterbody_coordinates$lon
      wb_lt = waterbody_coordinates$lat


      # It would be nice to clean supplied coordinates here.
      # E.g. wb_ln = clean_coords(wb_ln)
      point_for_search = sf::st_as_sf(data.frame(ln = wb_ln, lt = wb_lt),
                                      coords = c("ln","lt"),
                                      crs = 4326) |>
        sf::st_transform(3005)

      wb = bcdata::bcdc_query_geodata('cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6') |>
        bcdata::filter(bcdata::INTERSECTS(point_for_search)) |>
        bcdata::collect()

      if(nrow(wb) == 0) stop("Unfortunately, no lake was found with that name and coordinate combination...exiting.")
    }
  }
  if(!quiet) cat(paste0("\nGathering nearby lakes...",Sys.time(),"\n"))
  nearby_lakes = bcdata::bcdc_query_geodata('cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6') |>
    bcdata::filter(bcdata::DWITHIN(wb, distance = search_radius, units = 'kilometers')) |>
    bcdata::collect() |>
    dplyr::mutate(polytype = 'lakes') |>
    dplyr::select(WATERBODY_POLY_ID,WATERSHED_GROUP_ID,GNIS_NAME = GNIS_NAME_1,polytype)
  if(!quiet) cat(paste0("\nFinished at ",Sys.time(),"\n"))

  if(!quiet) cat("\nGathering nearby streams/rivers...\n")
  nearby_streams = bcdata::bcdc_query_geodata('freshwater-atlas-stream-network') |>
    bcdata::filter(bcdata::DWITHIN(wb, distance = search_radius, units = 'kilometers')) |>
    bcdata::collect() |>
    dplyr::mutate(polytype = 'stream') |>
    dplyr::select(LINEAR_FEATURE_ID,WATERSHED_GROUP_ID,GNIS_NAME,polytype)
  if(!quiet) cat(paste0("\nFinished at ",Sys.time(),"\n"))

  # Buffer streams by 5 meters.
  nearby_streams = sf::st_buffer(nearby_streams, 5)

  if(!quiet) cat("\nCombining lakes/rivers/streams, finding connected networks...\n")

  # Combine lakes with streams.
  wbs = nearby_lakes |>
    dplyr::select(name = GNIS_NAME, polytype) |>
    dplyr::bind_rows(nearby_streams |>
                       dplyr::select(name = GNIS_NAME, polytype)
    )
  # Find network.

  parts = sf::st_cast(wbs |> dplyr::summarise(),"POLYGON")

  clust = unlist(sf::st_intersects(wbs, parts))

  wbs$graph_id = clust

  if(!quiet) cat(paste0("\nFinished finding networks - ",Sys.time(),"\n"))

  target_graph = wbs |> dplyr::filter(name == waterbody_name) |> dplyr::pull(graph_id)

  wbs = wbs |>
    dplyr::filter(graph_id == target_graph)

  wbs
}