#' Title Get the polygon for a waterbody
#'
#' @param focus_wb_name Name of waterbody
#' @param focus_wb_coordinates Coordinates (long, lat) of waterbody
#'
#' @return A single-rowed spatial table (sf object)
#' @export
#'
#' @examples /dontrun{}
get_waterbody_polygon = function(focus_wb_name = NULL,
                                 focus_wb_coordinates = NULL){

  if(is.null(focus_wb_name) & is.null(focus_wb_coordinates)) stop("Please enter waterbody name or, preferably, coordinates.")
  if(!is.null(focus_wb_coordinates) & length(focus_wb_coordinates) != 2) stop("Coordinates must be in the format: c(longitude, latitude); remember the minus sign for longitude!")
  # Make sure lake name is in title format.
  if(!is.null(focus_wb_name)){
    focus_wb_name = stringr::str_to_title(focus_wb_name)
  }

  # No coordinates?
  if(is.null(focus_wb_coordinates)){
    # But there is a name?
    if(!is.null(focus_wb_name)){
      # Look for the lake by name.
      lake_find_attempt = bcdata::bcdc_query_geodata('cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6') |>
        bcdata::filter(GNIS_NAME_1 == focus_wb_name) |>
        bcdata::collect()
      # Is there only a single lake in BC with that name?
      if(nrow(lake_find_attempt) == 1){
        # If so, use that!
        focus_wb_poly = lake_find_attempt
      } else {
        focus_wb_poly = lake_find_attempt |>
          dplyr::mutate(Area = 1e-6 * as.numeric(sf::st_area(geometry))) |>
          dplyr::arrange(desc(AREA)) |> dplyr::slice(1)
        cat(paste0("We searched for the lake with only a name, and ", nrow(lake_find_attempt), " lakes were returned."))
        cat("Using the largest lake from that list...")
      }
    }
  } else {

    point_for_int = sf::st_as_sf(data.frame(lng = focus_wb_coordinates[1],
                                            lat = focus_wb_coordinates[2]),
                                 coords = c("lng","lat"),
                                 crs = 4326) |>
      sf::st_transform(crs = 3005)

    focus_wb_poly = bcdata::bcdc_query_geodata('cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6') |>
      bcdata::filter(bcdata::INTERSECTS(point_for_int)) |>
      bcdata::collect()
  }
  focus_wb_poly
}
