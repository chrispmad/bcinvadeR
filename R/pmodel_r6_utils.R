# summarize species occurrence data points to chosen geographic units.
sum_spatial_data_to_geog_units = function(dat,
                                          geog_units,
                                          geog_id_col,
                                          sum_name,
                                          quiet = T){
  if(!quiet){cat("\nSummarizing spatial layer to geographic units...")}
  dat |>
    sf::st_join(
      geog_units,
      sf::st_intersects
    ) |>
    sf::st_drop_geometry() |>
    dplyr::group_by(!!rlang::sym(geog_id_col)) |>
    dplyr::summarise(!!rlang::sym(sum_name) := dplyr::n())
}
