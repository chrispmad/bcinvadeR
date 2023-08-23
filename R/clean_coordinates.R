
#' Title Clean latitude and longitude decimal degree coordinate data.
#'
#' @param dat A dataset with columns containing latitude and longitude in decimal degree format.
#'
#' @return A dataset with common typos in latitude / longitude columns corrected.
#' @export
#'
#' @examples dat = data.frame(Latitude = c('48.19N','38.92','-112.10'), Longitude = c('110.392W','-123.11','49.32'))
#' clean_coordinates(dat)
#'
clean_coordinates = function(dat){
  # Look at the first row of the data. Which row LIKELY has lat / long coordinates?
  likely_lat = names(which(lapply(dat[1,], \(x) stringr::str_detect(x, '[0-9]{2}\\.[0-9]+')) == TRUE))[1]
  likely_lon = names(which(lapply(dat[1,], \(x) stringr::str_detect(x, '[0-9]{3}\\.[0-9]+')) == TRUE))[1]

  if(is.na(likely_lat) | is.na(likely_lon)) stop("Latitude or longitude columns could not be identified! Are you sure you have any?")

  dat = dat |>
    dplyr::mutate(lat = !!rlang::sym(likely_lat),
           lon = !!rlang::sym(likely_lon)) |>
    dplyr::mutate(lat = stringr::str_remove_all(lat, '^(\\.)?'),
           lon = stringr::str_remove_all(lon, '^(\\.)?')) |>
    dplyr::mutate(lat = stringr::str_remove_all(lat, '[A-Za-z]+')) |>
    dplyr::mutate(lon = stringr::str_remove_all(lon, '[A-Za-z]+')) |>
    dplyr::mutate(lat = as.numeric(lat),
           lon = as.numeric(lon))

  # Sometimes lat and long are typed into the wrong columns; if so, switch here.
  dat = dat |>
    dplyr::mutate(
      lat_replace = dplyr::case_when(
        lat < 0 & length(stringr::str_extract(lat,'[0-9]+(?=\\.)')) > 2 ~ lon,
        T ~ lat))

  dat = dat |>
    dplyr::mutate(lon_replace = dplyr::case_when(
        lon > 0 & nchar(stringr::str_extract(lon,'[0-9]+(?=\\.)')) == 2 ~ lat,
        lon > 0 & nchar(stringr::str_extract(lon,'[0-9]+(?=\\.)')) == 3 ~ -1 * lon,
        T ~ lon)) |>
    dplyr::mutate(lat = lat_replace,
           lon = lon_replace) |>
    dplyr::select(-lat_replace, -lon_replace)

  dat |>
    dplyr::mutate(lon = ifelse(lon > 0, lon * -1, lon))
}
