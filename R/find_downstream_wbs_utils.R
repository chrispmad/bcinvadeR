# Functions to be used in finding downstream waterbodies from a given waterbody.

# Extract a node (first or last, typically) from a stream linestring.
# (from https://dewey.dunnington.ca/post/2020/stream-networks-using-r-and-sf/)
extract_node_single <- function(x, n) {
  # extract the nodes
  nodes <- suppressWarnings(st_cast(x, "POINT", warn = F))

  # lets you specify n = -1 for the last node
  if (n < 0) {
    n <- nrow(nodes) + 1 + n
  }

  # ensures that the output is always length(n)
  if (n > nrow(nodes)) {
    sfc <- st_sfc(st_point(), crs = st_crs(x))
    st_as_sf(tibble(geometry = rep(sfc, length(n))))
  } else {
    nodes[n, ]
  }
}

# Download waterbodies for the analysis.
get_relevant_data = function(focus_wb_poly,
                             dist_from_wb_to_search){

  # Get all lake/stream/river polygons within X kilometers, depending on the
  # options we wrote above.
  nlakes = bcdata::bcdc_query_geodata('freshwater-atlas-lakes') |>
    bcdata::filter(bcdata::DWITHIN(focus_wb_poly, dist_from_wb_to_search, 'kilometers')) |>
    bcdata::collect()

  nstreams = bcdata::bcdc_query_geodata('freshwater-atlas-stream-network') |>
    bcdata::filter(bcdata::DWITHIN(focus_wb_poly, dist_from_wb_to_search, 'kilometers')) |>
    bcdata::collect()

  stream_flow_dir = bcdata::bcdc_query_geodata('freshwater-atlas-stream-directions') |>
    bcdata::filter(bcdata::DWITHIN(focus_wb_poly, dist_from_wb_to_search, 'kilometers')) |>
    bcdata::collect() |>
    dplyr::select(direction = DOWNSTREAM_DIRECTION)

  # Simplify columns.
  nlakes = nlakes |>
    dplyr::mutate(polytype = 'lake') |>
    dplyr::select(name = GNIS_NAME_1, polytype)

  nstreams = nstreams |>
    dplyr::mutate(polytype = 'stream') |>
    sf::st_zm() |>
    dplyr::select(name = GNIS_NAME, polytype)

  output_list = list(nlakes,nstreams,stream_flow_dir)

  names(output_list) <- c("lakes","streams","flow_markers")

  output_list
}

# Extend flow direction markers by x m in the
# direction of flow.
extend_flow_markers = function(stream_flow_dir, flow_line_length){

  dat_for_flow_lines = stream_flow_dir |>
    dplyr::mutate(direction_within_90 = direction %% 90) |>
    dplyr::mutate(og_x = sf::st_coordinates(geometry)[,1],
           og_y = sf::st_coordinates(geometry)[,2]) |>
    dplyr::mutate(dir_rads = (direction * pi) / (180)) |>
    dplyr::mutate(x_add = flow_line_length*cos(dir_rads),
           y_add = flow_line_length*sin(dir_rads)) |>
    dplyr::mutate(new_x = og_x + x_add,
           new_y = og_y + y_add) |>
    sf::st_drop_geometry() |>
    dplyr::mutate(flow_id = dplyr::row_number()) |>
    dplyr::select(flow_id,direction,og_x,new_x,og_y,new_y)

  tidyr::tibble(
    flow_id = rep(dat_for_flow_lines$flow_id,2)
  ) |>
    dplyr::group_by(flow_id) |>
    dplyr::mutate(coord_id = paste0(flow_id,"-",dplyr::row_number())) |>
    dplyr::left_join(
      dat_for_flow_lines |>
        dplyr::select(flow_id,og_x,new_x) |>
        tidyr::pivot_longer(-flow_id,values_to = 'X') |>
        dplyr::select(-name) |>
        dplyr::group_by(flow_id) |>
        dplyr::mutate(coord_id = paste0(flow_id,"-",dplyr::row_number()))) |>
    dplyr::left_join(
      dat_for_flow_lines |>
        dplyr::select(flow_id,og_y,new_y) |>
        tidyr::pivot_longer(-flow_id,values_to = 'Y') |>
        dplyr::select(-name) |>
        dplyr::group_by(flow_id) |>
        dplyr::mutate(coord_id = paste0(flow_id,"-",dplyr::row_number()))) |>
    dplyr::ungroup() |>
    sf::st_as_sf(coords = c("X","Y"), crs = 3005) |>
    dplyr::arrange(flow_id, coord_id) |>
    dplyr::group_by(flow_id) |>
    dplyr::summarize(do_union=FALSE) |>
    sf::st_cast("LINESTRING", warn = F)
}


# Find the downstream node of a stream's two end nodes.
find_downstream_node_of_stream = function(stream, flow_lines){

  browser()

  stream_line_nodes = sf::st_cast(sf::st_boundary(stream),'POINT',warn=F)

  the_flows = flow_lines |>
    dplyr::filter(sf::st_intersects(geometry, stream, sparse = F))

  the_flows_as_points = sf::st_cast(the_flows, 'POINT', warn = F) |>
    dplyr::mutate(flow_id = dplyr::row_number()) |>
    dplyr::group_by(flow_id) |>
    dplyr::mutate(start = dplyr::row_number() == 1) |>
    dplyr::ungroup()

  downstream_flow_marker_flow_id = the_flows_as_points |>
    dplyr::mutate(dist_to_stream = as.numeric(sf::st_distance(geometry, stream))) |>
    dplyr::filter(dist_to_stream != 0) |>
    dplyr::arrange(desc(dist_to_stream)) |>
    dplyr::slice(1) |>
    dplyr::pull(flow_id)

  the_flows_as_points = the_flows_as_points |>
    dplyr::mutate(downstream = flow_id == downstream_flow_marker_flow_id)

  downstream_flow_marker = the_flows_as_points |>
    dplyr::filter(downstream == TRUE, start == TRUE)

  #Overlay the downstream flow line (specifically, the start node of that flow line)
  # with the node of the stream piece.
  stream_line_nodes = stream_line_nodes |>
    dplyr::mutate(downstream_node = sf::st_intersects(geometry, downstream_flow_marker, sparse = F))

  stream_line_nodes |>
    dplyr::filter(downstream_node)
}


# Complicated little ditty: tests to see if a stream piece that touches
# our graph_seed flow network should be added or not; to do this, it looks

contender_stream_downstream_from_graph = function(current_graph_seed, contender_stream, loop){

  flow_markers_for_stream_to_add = flow_line_pool |>
    dplyr::filter(sf::st_intersects(geometry, contender_stream, sparse = F))

  # Look at the further flow marker. Does its downstream point lie closer
  # or further from our graph_seed? If closer, then drop this stream piece!
  farther_flow_marker_for_stream_to_add = flow_markers_for_stream_to_add |>
    dplyr::mutate(distance_from_graph = sf::st_distance(geometry, current_graph_seed)) |>
    dplyr::arrange(desc(distance_from_graph)) |>
    dplyr::slice(1)

  distance_twixt_flow_point_and_graph_seed = sf::st_boundary(farther_flow_marker_for_stream_to_add) |>
    sf::st_cast("POINT", warn = F) |>
    dplyr::mutate(distance_from_graph = as.numeric(sf::st_distance(geometry, current_graph_seed))) |>
    dplyr::pull(distance_from_graph)

  # If the flow line is more or less pointing toward our graph_seed (i.e. the end of the
  # flow line is 25 meters or more closer to our graph_seed than the flow marker start point),
  #
  if(distance_twixt_flow_point_and_graph_seed[1] > (distance_twixt_flow_point_and_graph_seed[2] + 25)){
    return(F)
  }

  # If the delta distance between flow point and extruded flow point (50m downstream)
  # is less than, say, 35m, then we likely have a flow line that is overlapping the graph_seed;
  # Also make sure that the flow marker start point is under 10 m (likely a flow marker indicating
  # flow into our graph seed, if it's this close and it overlaps the graph_seed)
  # Reject this stream piece!
  if(distance_twixt_flow_point_and_graph_seed[1] != 0 & distance_twixt_flow_point_and_graph_seed[1] <= 10 & (distance_twixt_flow_point_and_graph_seed[1] + 35 > distance_twixt_flow_point_and_graph_seed[2])){
    return(F)
  }
  return(TRUE)
}

# Functions for stream joining loop below.
downstream_setup = function(focus_wb,
                            focus_wb_poly,
                            lakes,
                            streams,
                            flow_lines){
  #Set up lakes / streams / flow marker pools to search in each loop.
  lakes_pool <<- lakes |> dplyr::filter(name != focus_wb | is.na(name))

  streams_pool <<- streams |> dplyr::mutate(stream_number = dplyr::row_number())

  flow_line_pool <<- flow_lines

  consecutive_misses_for_downstream_point <<- 0

  # Get stream pieces inside our focal wb
  graph_seed <<- streams_pool |>
    dplyr::filter(sf::st_intersects(geometry, focus_wb_poly, sparse = F)) |>
    dplyr::slice(1) |>
    dplyr::mutate(name = 'graph')

  # Remove streams that are inside our focal wb from stream_pool
  streams_pool <<- streams_pool |>
    dplyr::filter(!stream_number %in% graph_seed$stream_number)
}

find_downstream_graph = function(focus_wb,
                                 focus_wb_poly,
                                 lakes,
                                 streams,
                                 flow_markers,
                                 max_iterations = 100,
                                 show_graph_growth = F) {

  for(i in 1:max_iterations){

    if(i == 1) downstream_setup(focus_wb,
                                focus_wb_poly,
                                lakes,
                                streams,
                                flow_markers)

    if(show_graph_growth) {
      print(plot(graph_seed$geometry))
      title(paste0('Iteration ',i))
    }

    downstream_point = find_downstream_node_of_stream(graph_seed, flow_lines = flow_line_pool)

    # If there's no downstream point, try seeing if there's a stream piece that touches
    # the overlapping segment from the end of the last loop
    if(nrow(downstream_point) == 0){

      # Add one to our counter for this kind of situation happening consecutively...
      consecutive_misses_for_downstream_point = consecutive_misses_for_downstream_point + 1

      # Have we had 2 consecutive misses for downstream points? If so, break!
      if(consecutive_misses_for_downstream_point == 2) break

      stream_to_add = streams_pool |>
        dplyr::filter(sf::st_intersects(geometry, overlap_of_graph_seed_and_downstream, sparse = F))

      # No such stream piece that's touching? This should indicate we've reached
      # the end of the line and there are no more contender stream pieces
      # touching our graph (we try to remove such intersecting stream pieces
      # if it looks like they are flowing into our graph)
      if(nrow(stream_to_add) == 0){
        cat("\nCompleted network analysis.\n")
        return(graph_seed)
      }

      # Remember to remove this stream piece from the stream pool!
      streams_pool = streams_pool |>
        dplyr::filter(!sf::st_intersects(geometry, overlap_of_graph_seed_and_downstream, sparse = F))

      add_stream_assessment_results = list()
      for(z in 1:nrow(stream_to_add)){
        add_stream_assessment_results[[z]] = contender_stream_downstream_from_graph(graph_seed, stream_to_add[z,], i)
      }

      # Merge any streams passing the test above with our graph_seed!
      overlap_of_graph_seed_and_downstream = stream_to_add[unlist(add_stream_assessment_results),]

      graph_seed = graph_seed |>
        dplyr::bind_rows(overlap_of_graph_seed_and_downstream) |>
        dplyr::summarise(name = 'graph') |>
        dplyr::mutate(loop = i)
    } else {

      # We found a downstream point!

      # Reset the counter for consecutive misses of downstream points...
      consecutive_misses_for_downstream_point = 0

      # Remove flow line(s) that intersects with downstream point from flow line pool
      flow_line_pool = flow_line_pool |>
        dplyr::filter(!sf::st_intersects(geometry, downstream_point, sparse = F))

      # See which streams from the streams_pool overlap with our downstream point.
      overlap_of_graph_seed_and_downstream = streams_pool |>
        dplyr::filter(sf::st_intersects(geometry, downstream_point, sparse = F))
      # If none do, supposedly we have tried matching all streams to all
      # downstream points and have removed all that flow into our graph seed...
      # Assuming this to be true, we can jump out of the loop here.
      if(nrow(overlap_of_graph_seed_and_downstream) == 0){
        return(graph_seed)
      }

      if(nrow(overlap_of_graph_seed_and_downstream) >= 2){
        # Are there multiple overlapping sections, and at least one of them is
        # very short? If that's the case, skip the tests and merge both.
        if(!(as.numeric(sf::st_length(overlap_of_graph_seed_and_downstream))[1] <= 50 | (as.numeric(sf::st_length(overlap_of_graph_seed_and_downstream))[1] <= 50))){
          # Otherwise! Check that these contender streams aren't flowing into our graph seed.
          add_stream_assessment_results = list()
          for(z in 1:nrow(overlap_of_graph_seed_and_downstream)){
            add_stream_assessment_results[[z]] = contender_stream_downstream_from_graph(graph_seed, overlap_of_graph_seed_and_downstream[z,], i)
          }

          overlap_of_graph_seed_and_downstream = overlap_of_graph_seed_and_downstream[unlist(add_stream_assessment_results),]

          # The following line may no longer be necessary... but it also might be!
          overlap_of_graph_seed_and_downstream = overlap_of_graph_seed_and_downstream |>
            dplyr::summarise()
        }
      }

      # Remove stream that intersects with downstream point from stream pool
      streams_pool = streams_pool |>
        dplyr::filter(!sf::st_intersects(geometry, downstream_point, sparse = F))

      graph_seed = graph_seed |>
        dplyr::bind_rows(overlap_of_graph_seed_and_downstream) |>
        dplyr::summarise(name = 'graph') |>
        dplyr::mutate(loop = i)
    }
  }
  print(plot(graph_seed$geometry))
  title(paste0('Iteration ',max_iterations))

  return(graph_seed)
}
