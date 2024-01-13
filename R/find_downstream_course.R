#' Use the BC Freshwater Atlas to trace course downstream from chosen waterbody polygon
#'
#' @param wb_polygon Chosen waterbody polygon;
#' @param wb_is_lake Is the chosen waterbody polygon a lake?
#' @param wb_separate Would you like the resulting series of linestrings to be joined into one multilinestring?
#' @param ggplot_fig Should a ggplot figure be made an appended to output list?
#'
#' @return An sf vector table of downstream waterways; optional ggplot figure.
#' @export
#'
#' @examples \dontrun {
#' shus = bcdata::bcdc_query_geodata('freshwater-atlas-lakes') |>
#' bcdata::filter(GNIS_NAME_1 == 'Shuswap Lake') |>
#' bcdata::collect()
#'
#' find_downstream_course(shus, ggplot_fig = T)
#' }
#'
find_downstream_course = function(wb_polygon,
                                  wb_is_lake = T,
                                  wb_separate = T,
                                  ggplot_fig = F){

  wb_poly_m = wb_polygon |>
    sf::st_zm() |>
    dplyr::summarise()

  if(sf::st_is_empty(wb_poly_m)) stop("Error: waterbody input has no geometry")

  # If user submitted a lake polygon, find stream polygon.
  if(wb_is_lake){
    target_stream = tryCatch(
      expr = bcdata::bcdc_query_geodata('freshwater-atlas-stream-network') |>
        bcdata::filter(bcdata::INTERSECTS(wb_poly_m)) |>
        bcdata::filter(FEATURE_SOURCE == 'lake-def skelet') |>
        bcdata::collect() |>
        sf::st_zm() |>
        dplyr::group_by(FWA_WATERSHED_CODE,GNIS_NAME,BLUE_LINE_KEY) |>
        dplyr::summarise(.groups = 'drop'),
      error = function(e) NULL
    )
  } else {
    target_stream = wb_polygon
  }

  if(!'FWA_WATERSHED_CODE' %in% names(target_stream)) stop("Error: submitted stream waterbody lacks FWA_WATERSHED_CODE column")
  if(is.null(target_stream)) stop("Error: no stream found for waterbody input")

  fwa_code = unique(target_stream$FWA_WATERSHED_CODE)

  # Remove empty 0s
  fwa_code_no_blanks = stringr::str_remove(fwa_code, '-00000.*')

  # Find number of stream connections
  num_st_con = stringr::str_count(fwa_code_no_blanks,'-')
  # Start log of number of streams successfully dl'd from BC FWA.
  streams_dld = 1

  print(paste0(num_st_con," stream connections found downstream"))

  downstream_course = list(target_stream)

  for(i in 1:num_st_con){
    print(paste0('working on stream juncture ',i,' of ',num_st_con))
    if(i == 1){
      dr_stream = target_stream
      dr_fwa_code = fwa_code_no_blanks
    } else {
      # Use the receiving stream from prior loop as new draining stream
      dr_stream = rec_stream
      dr_fwa_code = rec_fwa_code
    }

    # Find suffix id for draining stream
    dr_fwa_code_suffix = stringr::str_extract(stringr::str_remove(dr_fwa_code,'-00000.*'),'[0-9]{6}$')

    # Find receiving stream id
    rec_fwa_code = stringr::str_remove(dr_fwa_code, '-[0-9]*$')

    # Formulate CQL query to find receiving stream
    cql_query = paste0("FWA_WATERSHED_CODE like '",rec_fwa_code,"-000000%'")

    rec_stream = bcdata::bcdc_query_geodata('freshwater-atlas-stream-network') |>
      bcdata::filter(bcdata::CQL(cql_query)) |>
      bcdata::collect() |>
      sf::st_zm()

    if(nrow(rec_stream) > 0){
      # Running record of what stream order we are on. We are always going to go up.
      max_st_order = max(rec_stream$STREAM_ORDER)

      # Find min and max route measures for receiving stream.
      max_downstream_measure = max(rec_stream$DOWNSTREAM_ROUTE_MEASURE)
      min_downstream_measure = min(rec_stream$DOWNSTREAM_ROUTE_MEASURE)

      # Find where along (%) receiving stream the draining stream intersects
      percent_downstream_intersection = as.numeric(dr_fwa_code_suffix)/1000000

      # Convert % to meter measure.
      downstream_cutoff_m = percent_downstream_intersection * max_downstream_measure

      # Find the most likely BLUE_LINE_KEY; i.e., the most numerous one.
      likely_blk = rec_stream |>
        sf::st_drop_geometry() |>
        dplyr::count(BLUE_LINE_KEY) |>
        dplyr::arrange(dplyr::desc(n)) |>
        dplyr::slice(1) |>
        dplyr::pull(BLUE_LINE_KEY)

      # Only keep portion of receiving stream that is downstream of
      # juncture with draining stream

      # if(i == 3) browser()
      rec_stream = rec_stream |>
        dplyr::filter(DOWNSTREAM_ROUTE_MEASURE <= downstream_cutoff_m) |>
        # Filter for rows where stream order is at least the max we've encountered so far
        # and the BLK is the same as the most numerous BLK (to trim away little single streams)
        # from a big river, OR rows that are 'lake-def skelet'.
        # dplyr::filter((STREAM_ORDER >= max_st_order & BLUE_LINE_KEY == likely_blk) | FEATURE_SOURCE == 'lake-def skelet') |>
        # dplyr::filter((STREAM_ORDER >= max_st_order) | FEATURE_SOURCE == 'lake-def skelet') |>
        dplyr::filter(STREAM_ORDER >= max_st_order-1) |>
        dplyr::group_by(FWA_WATERSHED_CODE,GNIS_NAME,BLUE_LINE_KEY) |>
        dplyr::summarise(.groups = 'drop')

      downstream_course = append(downstream_course, list(rec_stream))
      streams_dld = streams_dld + 1
    }
  }

  # Find likely name for submitted water body polygon
  likely_name_col = names(wb_polygon)[stringr::str_detect(names(wb_polygon),'GNIS_NAME.*')][1]

  if(!is.null(likely_name_col)){
    likely_name = sf::st_drop_geometry(wb_polygon)[1,likely_name_col]
  } else {
    # No name column easily identified; just going to call it 'Unnamed'
    likely_name = 'Unnamed'
  }

  # Attempting cleaning of very distant lake-def skelet pieces.
  # lake_skelets = downstream_course
  if(ggplot_fig){
    colours = RColorBrewer::brewer.pal(num_st_con+1, 'Spectral')
    p = ggplot2::ggplot()
    for(y in 1:(streams_dld)) {
      p = p + ggplot2::geom_sf(data = downstream_course[[y]], col = colours[y]) +
        ggplot2::theme(plot.background = ggplot2::element_blank())
    }

    # Package up results
    downstream_course = dplyr::bind_rows(downstream_course)

    if(!wb_separate){
      downstream_course = downstream_course |>
        dplyr::summarise(query = paste0('downstream route from ',sf::st_drop_geometry(wb_polygon)[1,likely_name_col]))
    }

    output = list(downstream_course)
    names(output) = 'downstream_course'
    output$plot = p
  } else {

    # Package up results
    downstream_course = dplyr::bind_rows(downstream_course)

    if(!wb_separate){
      downstream_course = downstream_course |>
        dplyr::summarise(query = paste0('downstream route from ',sf::st_drop_geometry(wb_polygon)[1,likely_name_col]))
    }
    output = downstream_course
  }
  return(output)
}
