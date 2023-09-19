#' Title Intuit the likely column containing latitude
#'
#' @param dat tabular data with (one) column of latitude data
#'
#' @return Returns the name of the (first) column likely containing latitude
#'
#' @examples dat = data.frame(LaTiTuDe = c(49.12,31.22,-120.8))
#' intuit_lat_col(dat)
intuit_lat_col = function(dat){

  if(!is.data.frame(dat)) stop("This function works on tabular datasets of 2+ columns; please input such a dataset.")
  if(ncol(dat) < 2) stop("Your dataset has fewer than two columns; please check input data.")

  lat_col_name = names(which(lapply(dat[1,], \(x) stringr::str_detect(x, '^[0-9]{2}\\.[0-9]+')) == TRUE))[1]

  if(is.na(lat_col_name)) stop("No latitude column could be deduced... apologies!")
  lat_col_name
}
