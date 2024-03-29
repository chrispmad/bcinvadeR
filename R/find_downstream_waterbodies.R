# library(tidyverse)
# library(ggthemes)
# library(bcdata)
# library(sf)
# library(leaflet)

# focus_wb = 'Echo Lake'
# focus_wb_coords = c(-125.41086, 49.98650)

#' Title Find the downstream waterbodies
#'
#' @param waterbody_name Name of waterbody
#' @param waterbody_coordinates Coordinates within waterbody (long, lat)
#' @param dist_from_wb_to_search Radius to search for streams/lakes, in kilometers
#' @param in_shiny Is this function being run in shiny? If so, give incremental progress updates.
#' @param the_session The shiny session
#'
#' @return sf object spatial table of downstream lakes
#' @export
#'
#' @examples \dontrun
find_downstream_waterbodies = function(
    waterbody_name = NULL,
    waterbody_coordinates = NULL,
    dist_from_wb_to_search = 10, # This number is in kilometers.
    in_shiny = F,
    the_session = session
){

  # # # # # # # # # # # # # # # # #
  # Getting Polygon to Search From#
  # # # # # # # # # # # # # # # # #

  # # Make sure lake name is in title format.
  # if(!is.null(waterbody_name)){
  # waterbody_name = stringr::str_to_title(waterbody_name)
  # }
  #
  # # No coordinates?
  # if(is.null(waterbody_coordinates)){
  #   # But there is a name?
  #   if(!is.null(waterbody_name)){
  #     # Look for the lake by name.
  #     lake_find_attempt = bcdata::bcdc_query_geodata('cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6') |>
  #       bcdata::filter(GNIS_NAME_1 == waterbody_name) |>
  #       bcdata::collect()
  #     # Is there only a single lake in BC with that name?
  #     if(nrow(lake_find_attempt) == 1){
  #       # If so, use that!
  #       focus_wb_poly = lake_find_attempt
  #     } else {
  #       focus_wb_poly = lake_find_attempt |>
  #         dplyr::mutate(Area = 1e-6 * as.numeric(sf::st_area(geometry))) |>
  #         dplyr::arrange(desc(AREA)) |> dplyr::slice(1)
  #       cat(paste0("We searched for the lake with only a name, and ", nrow(lake_find_attempt), " lakes were returned."))
  #       cat("Using the largest lake from that list...")
  #     }
  #   }
  # } else {
  #
  #   point_for_int = sf::st_as_sf(data.frame(lng = waterbody_coordinates[1],
  #                                           lat = waterbody_coordinates[2]),
  #                                coords = c("lng","lat"),
  #                                crs = 4326) |>
  #     sf::st_transform(crs = 3005)
  #
  #   focus_wb_poly = bcdata::bcdc_query_geodata('cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6') |>
  #     bcdata::filter(bcdata::INTERSECTS(point_for_int)) |>
  #     bcdata::collect()
  # }

  focus_wb_poly = get_waterbody_polygon(waterbody_name,
                        waterbody_coordinates)

  function_logic = function(){

  if(in_shiny) shiny::incProgress(amount = 1/5, message = 'waterbody polygon downloaded.')

  centroid_points = as.data.frame(sf::st_centroid(focus_wb_poly) |> sf::st_transform(crs = 4326) |> sf::st_coordinates())

  # If no waterbody name was fed in to function, grab it from the waterbody polygon we downloaded.
  if(is.null(waterbody_name)) waterbody_name = focus_wb_poly$GNIS_NAME_1

  wb_data = get_relevant_data(focus_wb_poly, dist_from_wb_to_search)

  if(in_shiny) shiny::incProgress(amount = 1/5, message = 'downloaded lake/stream/flow data.')

  lakes = wb_data$lakes
  streams = wb_data$streams
  flow_markers = wb_data$flow_markers

  # Extend each stream direction point by x m in the direction of flow it indicates.
  flow_line_length = 50
  flow_lines = extend_flow_markers(flow_markers, flow_line_length)

  if(in_shiny) shiny::incProgress(amount = 1/5, message = 'flow lines extended...')

  downstream_stream_network = find_downstream_graph(waterbody_name,
                                                    focus_wb_poly,
                                                    lakes,
                                                    streams,
                                                    flow_lines)

  if(in_shiny) shiny::incProgress(amount = 1/5, message = 'network composition complete!')

  downstream_lakes = lakes |>
    filter(st_intersects(geometry, downstream_stream_network, sparse = F))

  if(in_shiny) shiny::incProgress(amount = 1/4, message = 'lakes in network chosen.')

  return(downstream_lakes)
  }

  if(in_shiny){
    wbs = shiny::withProgress(
      message = 'Finding Connected Waterbodies',
      session = the_session,
      expr = function_logic()
    )
  } else {
    wbs = function_logic()
  }
  wbs
}
#
#   downstream_lakes |>
#     dplyr::mutate(Area = 1e-6 * as.numeric(sf::st_area(geometry))) |>
#     dplyr::mutate(`Center Point (NAD 83)` = sf::st_centroid(geometry)) |>
#     sf::st_transform(4326) |>
#     dplyr::mutate(`Center Point (WGS 84)` = sf::st_centroid(geometry)) |>
#     sf::st_drop_geometry() |>
#     dplyr::select(Name = name,
#                   Area,
#                   `Center Point (NAD 83)`,
#                   `Center Point (WGS 84)`) |>
#     dplyr::arrange(desc(Area))


