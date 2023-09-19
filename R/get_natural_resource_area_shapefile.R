# Utils for this function
#' Title Download the polygon(s) for a natural resource region or district, by name
#'
#' @param area_type The natural resource are type; one of 'region' or 'district'
#' @param specific_areas Either 'all' or a concatenated vector of region or district names as strings.
#'
#' @return A spatial table (sf object) of the region(s) or district(s) specified.
#' @export
#'
#' @examples if(interactive()){
#' get_natural_resource_area_shapefile('regions', c('Thompson-Okanagan','South Coast'))
#' }
get_natural_resource_area_shapefile = function(area_type = area_type,
                                               specific_areas = 'all'){

  if(!'all' %in% specific_areas){
  # Convert vector of region or district names to a regular expression to be used in a {stringr} search.
  names_for_search = paste0("(",stringr::str_flatten(specific_areas, collapse = "|"),")")
  }

  initial_area = switch(area_type,
                        regions = bcmaps::nr_regions(),
                        districts = bcmaps::nr_districts())

  if(area_type == 'regions'){
    if('all' %in% specific_areas) area_filtered = initial_area
    if(!'all' %in% specific_areas){
      area_filtered = initial_area |>
        dplyr::filter(stringr::str_detect(REGION_NAME, names_for_search))
    }
  }
  if(area_type == 'districts'){
    if('all' %in% specific_areas) area_filtered = initial_area
    if(!'all' %in% specific_areas){
      area_filtered = initial_area |>
        dplyr::filter(stringr::str_detect(REGION_NAME, names_for_search))
    }
  }
  area_filtered
}
