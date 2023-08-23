#' Title Add a column for area to a table with waterbody (lake or river) names
#'
#' @param dat A table that includes a column of waterbody names
#' @param waterbody_polygons sf table of waterbody polygons (perhaps filtered for unique names of your dat!)
#' @param units Units for area measurement; one of 'sq_meters', 'sq_km', or 'acres'
#' @param digits How many digits to include in area measurement
#'
#' @return Your table with a new column 'wb_area'
#' @export
#'
#' @examples
add_waterbody_area = function(dat, waterbody_polygons, units = c("sq_meters","sq_km","acres"), digits = 2){

  if(length(units) > 1) stop("Please choose one unit for area from options 'sq_meters','sq_km', or 'acres'")
  # Check there's a column named 'row_index' in dat.
  if(!'row_index' %in% names(dat)){
    dat = dat |> mutate(row_index = dplyr::row_number())
  }

  unique_wb_names = unique(dat$waterbody)

  dat$wb_area = 0

  for(row in unique_wb_names){
    print(row)

    dat_by_name = dat |> filter(waterbody == row)

    # Do name match with polygons.
    matched_polys = waterbody_polygons |> filter(waterbody == row)

    if(nrow(matched_polys) == 1){
      # Single name match. Nice! Grab the lake area, set to proper units, and add
      # to data.
      if(units == 'sq_meters') dat[dat$waterbody == row,]$wb_area <- round(matched_polys$area_sq_m, digits)
      if(units == 'sq_kilometers') dat[dat$waterbody == row,]$wb_area <- round(matched_polys$area_sq_m/1000000, digits)
      if(units == 'acres') dat[dat$waterbody == row,]$wb_area <- round(matched_polys$area_sq_m * 0.000247105, digits)

    } else {

      # Spatialize the lab data with this loop's waterbody name.
      dat_by_name = sf::st_as_sf(dat_by_name, coords = c('lon','lat'), crs = 4326)

      for(i in 1:nrow(dat_by_name)){
        print(paste0('lab samples ',i,' for ', row))

        row_index_for_ith_loop = dat_by_name[i,]$row_index

        area_to_add = matched_polys |>
          dplyr::mutate(distance_to_sample = as.numeric(sf::st_distance(dat_by_name[i,]))) |>
          dplyr::arrange(distance_to_sample) |>
          dplyr::slice(1) |>
          dplyr::pull(area_sq_m)

        dat[dat$row_index == dat_by_name[i,]$row_index,]$wb_area <- area_to_add

        if(units == 'sq_meters') dat[dat$row_index == dat_by_name[i,]$row_index,]$wb_area <- round(area_to_add, digits)
        if(units == 'sq_kilometers') dat[dat$row_index == dat_by_name[i,]$row_index,]$wb_area <- round(area_to_add/1000000, digits)
        if(units == 'acres') dat[dat$row_index == dat_by_name[i,]$row_index,]$wb_area <- round(area_to_add * 0.000247105, digits)
      }
    }
  }
  dat
}
