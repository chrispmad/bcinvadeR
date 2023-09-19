# Functions to make plots for prioritization model

make_ggplots = function(geog_units, geog_id_col, time_suffix, output_folder, ggplot_types){

  binned_variables = names(geog_units |> sf::st_drop_geometry() |> dplyr::select(ends_with('bin')))

  make_static_ggplot_maps(geog_units, geog_id_col, time_suffix, output_folder, binned_variables)

  make_static_ggplot_plots(geog_units, geog_id_col, time_suffix, output_folder, binned_variables, ggplot_types)
}

make_plotly = function(geog_units, geog_id_col, time_suffix, output_folder){

}

make_leaflet = function(geog_units, geog_id_col, time_suffix, output_folder, n_bins){

  binned_variables = names(geog_units |> sf::st_drop_geometry() |> dplyr::select(ends_with('bin')))

  my_pal = leaflet::colorFactor(palette = 'RdYlGn',
                                levels = c(0:n_bins),
                                reverse = T)

  my_leaf = leaflet::leaflet() |>
    leaflet::addTiles(group = 'OpenStreetMaps') |>
    leaflet::addProviderTiles(provider = leaflet::providers$CartoDB, group = 'CartoDB') |>
    leaflet::addLayersControl(baseGroups = c("CartoDB","OpenStreetMaps"),
                              overlayGroups = binned_variables,
                              options = leaflet::layersControlOptions(collapsed = F)) |>
    leaflet.extras::addSearchFeatures(
      targetGroups = c('risk_estimate_bin'),
      options = leaflet.extras::searchFeaturesOptions(zoom = 4, openPopup=TRUE)
    )  |>
    leaflet::addLegend(pal = my_pal, values = c(0:3))


  # Add variables to map, one by one.
  add_layer_to_leaflet = function(leaflet_map, this_var){

    if(this_var == 'num_occ_bin'){

      dat = geog_units |>
        dplyr::select(!!sym(geog_id_col),this_var) |>
        sf::st_transform(crs = 4326)

      the_label = paste0(geog_units[[geog_id_col]],": bin ",geog_units[[this_var]],
                         " (raw number: ",geog_units[[stringr::str_remove(this_var,'_bin')]],")")
    }

    if(this_var == 'risk_estimate_bin'){

      dat = geog_units |>
        dplyr::select(!!sym(geog_id_col),this_var) |>
        sf::st_transform(crs = 4326)

      the_label = paste0(geog_units[[geog_id_col]],": ",geog_units[[this_var]])
    }

    if(!this_var %in% c("num_occ_bin","risk_estimate_bin")){

      dat = geog_units |>
        dplyr::select(!!sym(geog_id_col),this_var,!!sym(stringr::str_replace(this_var,'_bin','_raw'))) |>
        sf::st_transform(crs = 4326)

      the_label = paste0(geog_units[[geog_id_col]],": bin ",geog_units[[this_var]],
                         " (raw number: ",geog_units[[stringr::str_replace(this_var,'_bin','_raw')]],")")
    }

    leaflet_map = leaflet_map |>
      leaflet::addPolygons(
        weight = 1,
        color = 'black',
        fillColor = my_pal(geog_units[[this_var]]),
        fillOpacity = 0.80,
        label = the_label,
        group = this_var,
        data = dat
      )
  }

  for(the_name in binned_variables){
    my_leaf = add_layer_to_leaflet(my_leaf, the_name)

    # Hide all layers on launch except the risk_estimate_bin!
    if(the_name != 'risk_estimate_bin'){
      my_leaf = my_leaf |>
        leaflet::hideGroup(group = the_name)
    }
  }

  my_leaf

  htmlwidgets::saveWidget(my_leaf, "output/Interactive_leaflet_map.html", selfcontained = F, libdir = "lib")
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

    if(this_var == 'angler_locations') browser()
    this_dat = geog_units |>
      dplyr::select(geog_id_col, this_var) |>
      dplyr::mutate(!!rlang::sym(this_var) := factor(!!rlang::sym(this_var), levels = c(3,2,1)))

    static_plot = ggplot2::ggplot() +
      ggplot2::geom_sf(data = this_dat, ggplot2::aes(fill = !!rlang::sym(this_var)))

    plot_filename = paste0(output_folder,'/static_plots/',this_var,'_static.png')

    ggplot2::ggsave(plot_filename, static_plot, width = 6, height = 4)
  }
}

make_static_ggplot_plots = function(geog_units, geog_id_col, time_suffix, output_folder, binned_variables, ggplot_types){

  if(ggplot_types == 'bar_graph'){
  boxplot_dat = geog_units |>
    sf::st_drop_geometry() |>
    dplyr::select(geog_id_col, dplyr::ends_with('raw')) |>
    tidyr::pivot_longer(cols = -geog_id_col, names_to = "vars", values_to = "values")

  boxplot_dat = boxplot_dat |>
    dplyr::mutate(outliers = values > 5*mean(values))

  boxplots = boxplot_dat |>
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = !!rlang::sym(geog_id_col), y = values, fill = vars)) +
    ggplot2::facet_wrap( ~ vars, scales = 'free') +
    ggplot2::labs(y = 'Values', x = 'Geographic Units') +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
          legend.position = 'none')
  # boxplots = boxplot_dat |>
  #   ggplot() +
  #   geom_boxplot(aes(x = vars, y = values)) +
  #   geom_point(aes(x = vars, y = values),
  #              col = 'red',
  #              data = boxplot_dat[boxplot_dat$outliers == T,]) +
  #   ggrepel::geom_label_repel(aes(x = vars, y = values, label = geog_id_col),
  #              data = boxplot_dat[boxplot_dat$outliers == T,]) +
  #   facet_wrap( ~ vars)

    plot_filename = paste0(output_folder,'/static_plots/bar_graphs.png')

    ggplot2::ggsave(plot_filename, boxplots, width = 6, height = 5)
  }
}
