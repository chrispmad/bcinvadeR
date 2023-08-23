#' Title Clean the waterbody name column of a table
#'
#' @param dat A dataset with waterbody names that need cleaning.
#' @param waterbody_column The column name (in quotations) that encodes the waterbody names.
#'
#' @return A dataset with common typos / different naming conventions in waterbody names corrected.
#' @export
#'
#' @examples dat = data.frame(watname = c("Lac La Hache","Shuswap Lake","Koocanusa Lake"))
#' clean_waterbody_names(dat)
clean_waterbody_names = function(dat, waterbody_column){
  # Some corrections to waterbody names.
  dat = dat |>
    dplyr::rename(waterbody = !!sym(waterbody_column)) |>
    dplyr::mutate(waterbody = stringr::str_squish(waterbody)) |>
    dplyr::mutate(waterbody = stringr::str_to_title(waterbody)) |>
    dplyr::mutate(waterbody = dplyr::case_when(
      waterbody == 'Lac La Hache' ~ 'Lac la Hache',
      waterbody == 'Okangan Lake' ~ 'Okanagan Lake',
      waterbody == 'Arrow Lake, Upper' ~ 'Upper Arrow Lake',
      waterbody == 'Arrow Lake, Lower' ~ 'Lower Arrow Lake',
      waterbody == 'Kootenay River (Nelson)' ~ 'Kootenay River',
      waterbody == 'Lower Fraser River' ~ 'Fraser River',
      waterbody == 'Upper Kettle River' ~ 'Kettle River',
      waterbody == 'Lower Kettle River' ~ 'Kettle River',
      waterbody == 'Alouette' ~ 'Alouette Lake',
      waterbody == 'Wahleach' ~ 'Wahleach Lake',
      waterbody == 'Koocanusa Lake' ~ 'Lake Koocanusa',
      waterbody == 'St Mary Lake' ~ 'St. Mary Lake',
      waterbody == 'Norbury Lake' ~ 'Norbury Lakes',
      waterbody == 'Lake Winderemere' ~ 'Windermere Lake',
      waterbody == 'Jimsmith Lake' ~ 'Jim Smith Lake',
      T ~ waterbody
    ))
  # Did we change the waterbody column name? If so, rename to original column name.
  if(waterbody_column != 'waterbody'){
    dat = dat |>
      dplyr::mutate(!!sym(waterbody_column) := waterbody) |>
      dplyr::select(-waterbody)
  }
  dat
}
