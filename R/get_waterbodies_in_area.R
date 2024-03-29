#' Title Download lake waterbody polygons from the BC Data Catalogue
#'
#' @param area_type Natural Resource area type; one of 'regions' or 'districts'
#' @param specific_areas The name(s) of one or more polygons of interest from the Natural Resource area selected
#' @param size_threshold Minimum area in square kilometers of lakes to include in download
#' @param output_crs Spatial projection system to use for lake sf object; defaults to WGS 84 (EPSG:4326)
#'
#' @return An sf object (spatial table) of lakes in the selected polygon(s) within the chosen Natural Resource area
#' @export
#'
#' @examples if(interactive()){
#' get_waterbodies_in_area(area_type = 'regions',
#' specific_areas = c("Thompson-Okanagan","West Coast"),
#' size_threshold = 100000)
#' }
get_waterbodies_in_area <- function(area_type = c("regions","districts"),
                                    specific_areas = c(NULL),
                                    size_threshold = 100,
                                    output_crs = 4326) {
  if(is.null(specific_areas)){
    stop("No specific area has been selected - please select one to avoid downloading all lakes in the province!")
  }
  if(length(area_type) > 1){
    stop("Please select one of 'regions' or 'districts'")
  }

  area_shapefile = get_natural_resource_area_shapefile(area_type = area_type,
                                      specific_areas = specific_areas)

  browser()
  # permanent ID cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6 is for 'freshwater-atlas-lakes'
    bcdata::bcdc_query_geodata('freshwater-atlas-lakes') |>
      bcdata::filter(FEATURE_AREA_SQM >= size_threshold*1000000) |>
      bcdata::filter(bcdata::INTERSECTS(area_shapefile)) |>
      bcdata::collect() |>
      sf::st_transform(crs = output_crs)
}
