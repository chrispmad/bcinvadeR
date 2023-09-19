#' Title Intuit the likely column containing longitude
#'
#' @param dat tabular data with (one) column of longitude data
#'
#' @return Returns the name of the (first) column likely containing longitude
#'
#' @examples dat = data.frame(lat = c(-29.112,-40.32,-41.32),
#' longitude = c(-120.8, -110.21, -90))
#' intuit_lng_col(dat)
intuit_lng_col = function(dat, hemisphere = 'W'){

  if(!is.data.frame(dat)) stop("This function works on tabular datasets of 2+ columns; please input such a dataset.")
  if(ncol(dat) < 2) stop("Your dataset has fewer than two columns; please check input data.")

  if(hemisphere == 'W'){
    lng_col_name = names(which(lapply(dat[1,], \(x) stringr::str_detect(x, '^-[0-9]+\\.[0-9]+')) == TRUE))[1]
  }

  if(hemisphere == 'E'){
    lng_col_name = names(which(lapply(dat[1,], \(x) stringr::str_detect(x, '^[0-9]+\\.[0-9]+')) == TRUE))[1]
  }

  if(is.na(lng_col_name)) stop("No longitude column could be deduced... apologies!")

  lng_col_name
}
