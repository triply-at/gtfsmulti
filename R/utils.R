#' Pair origins with destinations
#'
#' @param origins A [data.frame()] or similar representing the origin
#' locations. Should have only three columns, with the first column containing
#' unique indices for each location, and the second and third column containing
#' respectively the longitude and latitude coordinates of each location.
#'
#' @param destinations A [data.frame()] or similar representing the possible
#' destination locations. Should have only three columns, with the first column
#' containing unique indices for each location, and the second and third column
#' containing respectively the longitude and latitude coordinates of each
#' location.
#'
#' @param d_limit Upper distance limit (as-the-crow-flies) in meters to pair
#' an origin with a destination. Defaults to `Inf`, meaning that each origin
#' will be paired with all possible destinations.
#'
#' @return A [data.table::data.table()] object containing one
#' origin-destination pair per row. The table has six columns named *from*,
#' *to*, *from_lon*, *from_lat*, *to_lon* and *to_lat*, with the first two
#' containing the indices of respectively the origin and destination, and the
#' latter four containing their coordinates.
#'
#' @note Coordinates should be expressed in coordinate reference system
#' [EPSG:4326](https://epsg.io/4326). The function assumes this to be true, and
#' does not check for it.
#'
#' @export
get_od_pairs = function(origins, destinations, d_limit = Inf) {
  # Destinations will be connected to each origin iteratively.
  run_iteration = function(i) {
    O = origins[i, ]
    D = get_neighbours(c(O[[2]], O[[3]]), destinations, d_limit)
    if (nrow(D) > 0) {
      data.table::data.table(
        from = rep(O[[1]], nrow(D)),
        to = D[[1]],
        from_lon = rep(O[[2]], nrow(D)),
        from_lat = rep(O[[3]], nrow(D)),
        to_lon = D[[2]],
        to_lat = D[[3]]
      )
    } else {
      NULL
    }
  }
  data.table::rbindlist(lapply(seq_len(nrow(origins)), run_iteration))
}

#' Select neighbouring locations of a given origin
#'
#' @param origin A numeric vector of length two containing respectively
#' the longitude and latitude coordinate of the origin location.
#'
#' @param candidates A [data.frame()] or similar representing the set of
#' possible neighbour locations to choose from. Should have only three columns,
#' with the first column containing unique indices for each location, and the
#' second and third column containing respectively the longitude and latitude
#' coordinates of each location.
#'
#' @param d_limit Upper distance limit (as-the-crow-flies) in meters to
#' consider a candidate location as a neighbour of the origin. Defaults to
#' `Inf`, meaning that each candidate location will be a neighbour.
#'
#' @return A subset of `candidates`.
#'
#' @note Coordinates should be expressed in coordinate reference system
#' [EPSG:4326](https://epsg.io/4326). The function assumes this to be true, and
#' does not check for it.
#'
#' @export
get_neighbours = function(origin, candidates, d_limit = Inf) {
  if (is.infinite(d_limit)) return(candidates)
  from = stats::setNames(origin, c("lon", "lat"))
  to = candidates[, c(2, 3)]
  distances = geodist::geodist(from, to, measure = "haversine")[1, ]
  candidates[distances <= d_limit, ]
}

#' Find the nearest neighbour to a given origin
#'
#' @param origin A numeric vector of length two containing respectively
#' the longitude and latitude coordinate of the origin location.
#'
#' @param candidates A [data.frame()] or similar representing the set of
#' possible neighbour locations to choose from. Should have only three columns,
#' with the first column containing unique indices for each location, and the
#' second and third column containing respectively the longitude and latitude
#' coordinates of each location.
#'
#' @return A list of length two with element *idx* containing the index of the
#' nearest neighbour and element *dist* containing the distance to it.
#'
#' @note Coordinates should be expressed in coordinate reference system
#' [EPSG:4326](https://epsg.io/4326). The function assumes this to be true, and
#' does not check for it.
#'
#' @export
get_nearest_neighbour = function(origin, candidates) {
  from = stats::setNames(origin, c("lon", "lat"))
  to = candidates[, c(2, 3)]
  distances = suppressMessages(geodist::geodist(from, to))
  list(idx = candidates[[1]][which.min(distances)], dist = min(distances))
}

#' @importFrom data.table :=
set_transfer_time = function(table, modes = NULL, max_time = Inf) {
  # Get transfer times for all specified modes.
  # If no modes are specified use all modes present in the table.
  mode_cols = grep("transfer_time_", names(table))
  if (is.null(modes)) {
    all_times = lapply(mode_cols, function(x) table[[x]])
  } else {
    get_mode_times = function(name) {
      col = paste0("transfer_time_", name)
      if (! (col %in% names(table))) {
        stop("No travel times found for mode '", name, "'", call. = FALSE)
      }
      table[[col]]
    }
    all_times = lapply(modes, get_mode_times)
  }
  # Select minimum transfer time among the modes for each transfer connection.
  min_times = do.call("pmin", c(all_times, list(na.rm = TRUE)))
  new_table = table[, "min_transfer_time" := min_times]
  # Remove original mode-specific transfer time columns.
  new_table = table[, !mode_cols, with = FALSE]
  # Remove connections without a specified transfer time.
  # This happens when for none of the selected modes a time was specified.
  new_table = new_table[!(is.na(new_table$min_transfer_time)), ]
  # Return only connections with a transfer time below the given upper bound.
  new_table = new_table[new_table$min_transfer_time <= max_time, ]
  new_table
}

create_empty_transfer_table = function() {
  data.table::data.table(
    from_stop_id = character(),
    to_stop_id = character(),
    transfer_type = integer(),
    min_transfer_time = integer()
  )
}

mergelist = function(list, key) {  
  Reduce(function(...) merge(..., by = key, all = TRUE), list)
}

dodgr_to_sfnetwork = function(x) {
  nodes = dodgr::dodgr_vertices(x)
  x$from = x$from_id
  x$to = x$to_id
  names(x)[which(names(x) == "d")] = "distance"
  x$distance = units::set_units(x$distance, "m")
  names(x)[which(names(x) == "d_weighted")] = "weight"
  sfnetworks::sfnetwork(
    nodes = nodes,
    edges = x,
    force = TRUE,
    node_key = "id",
    coords = c("x", "y"),
    crs = 4326
  )
}

sfnetwork_to_dodgr = function(x) {
  df = data.frame(igraph::edge.attributes(x))
  class(df) = c("data.frame", "dodgr_streetnet")
  df
}

time_to_seconds = function(x) {
  hms_times = strsplit(x, ":")
  seconds = function(hms) {
    as.integer(hms[1]) * 3600 + as.integer(hms[2]) * 60 + as.integer(hms[3])
  }
  do.call("c", lapply(hms_times, seconds))
}

seconds_to_time = function(x) {
  as.character(hms::as_hms(x))
}

stops_to_sf = function(x) {
  sf::st_as_sf(x, coords = c("stop_lon", "stop_lat"), crs = 4326)
}