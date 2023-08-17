# Functions to make plots for prioritization model

make_ggplots = function(geog_units, geog_id_col, time_suffix, output_folder){

  binned_variables = names(geog_units |> sf::st_drop_geometry() |> dplyr::select(ends_with('bin')))

  make_static_ggplot_maps(geog_units, geog_id_col, time_suffix, output_folder, binned_variables)

  make_static_ggplot_boxplots(geog_units, geog_id_col, time_suffix, output_folder, binned_variables)
}

make_plotly = function(geog_units, geog_id_col, time_suffix, output_folder){

}

make_leaflet = function(geog_units, geog_id_col, time_suffix, output_folder, binned_variables, n_bins){

  binned_variables = names(geog_units |> sf::st_drop_geometry() |> dplyr::select(ends_with('bin')))

  my_leaf = leaflet::leaflet() |>
    leaflet::addTiles(group = 'OpenStreetMaps') |>
    leaflet::addProviderTiles(provider = leaflet::providers$CartoDB, group = 'CartoDB') |>
    leaflet::addLayersControl(baseGroups = c("OpenStreetMaps","CartoDB"),
                              overlayGroups = names(binned_variables),
                              options = leaflet::layersControlOptions(collapsed = F)) |>
    leaflet.extras::addSearchFeatures(
      targetGroups = c('risk_estimate_bin'),
      options = leaflet.extras::searchFeaturesOptions(zoom = 4, openPopup=TRUE)
    )

  my_pal = leaflet::colorFactor(palette = 'RdYlGn',
                                levels = c(1:n_bins),
                                reverse = T)

  # Add variables to map, one by one.
  add_layer_to_leaflet = function(leaflet_map, this_var){
    leaflet_map = leaflet_map |>
      leaflet::addPolygons(
        weight = 1,
        color = 'black',
        fillColor = my_pal(geog_units[[this_var]]),
        fillOpacity = 0.80,
        label = paste0(geog_units[[geog_id_col]],": ",geog_units[[this_var]]),
        group = this_var,
        data = geog_units |> dplyr::select(!!sym(geog_id_col),this_var) |> sf::st_transform(crs = 4326)
      )
  }

  for(the_name in names(binned_variables)){
    my_leaf = add_layer_to_leaflet(my_leaf, the_name)
  }

  my_leaf

  saveWidget(my_leaf, "output/my_leaf_test.html", selfcontained = F, libdir = "lib")
}
# library(plotly)
# library(htmlwidgets)
#
# test = plot_ly(iris) |>
#   plotly::add_trace(x = ~Species, y = ~Petal.Length, type = 'box')
#
# test
#
# test = leaflet() |>
#   addTiles() |>
#   addPolygons(data = geog_units |> st_transform(crs = 4326))
#
# test
#
# saveWidget(test, "output/test.html", selfcontained = F, libdir = "lib")



make_static_ggplot_maps = function(geog_units, geog_id_col, time_suffix, output_folder, binned_variables){
  for(i in 1:length(binned_variables)){

    this_var = binned_variables[i]

    this_dat = geog_units |>
      dplyr::select(geog_id_col, this_var) |>
      dplyr::mutate(!!sym(this_var) := factor(!!sym(this_var), levels = c(3,2,1)))

    browser()
    static_plot = ggplot() +
      geom_sf(data = this_dat, aes(fill = !!sym(this_var)))

    plot_filename = paste0(output_folder,'/static_plots/',this_var,'_static.png')

    ggsave(plot_filename, static_plot, width = 6, height = 4)
  }
}

make_static_ggplot_boxplots = function(geog_units, geog_id_col, time_suffix, output_folder, binned_variables){

  boxplot_dat = geog_units |>
    sf::st_drop_geometry() |>
    dplyr::select(geog_id_col, ends_with('raw')) |>
    pivot_longer(cols = -geog_id_col, names_to = "vars", values_to = "values")

  boxplot_dat = boxplot_dat |>
    mutate(outliers = values > 5*mean(values))

  boxplots = boxplot_dat |>
    ggplot() +
    geom_boxplot(aes(x = vars, y = values)) +
    geom_point(aes(x = vars, y = values),
               col = 'red',
               data = boxplot_dat[boxplot_dat$outliers == T,]) +
    ggrepel::geom_label_repel(aes(x = vars, y = values, label = geog_id_col),
               data = boxplot_dat[boxplot_dat$outliers == T,])

    plot_filename = paste0(output_folder,'/static_plots/boxplots.png')

    ggsave(plot_filename, boxplots, width = 6, height = 5)
}
