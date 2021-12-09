#' Create a multinet object from a list of data tables
#'
#' @param x A list of [data.table::data.table()] objects each correspoding to
#' a dataset file of a GTFS-Multi feed.
#'
#' @return An object of class `multinet`, which is a list of multiple
#' [data.table::data.table()] objects corresponding to the different dataset
#' files of a GTFS-Multi feed.
#'
#' @note Parameter `x` can also be an object of class
#' [gtfs][gtfsio::import_gtfs()], since this is nothing more than a list of
#' `data.table` objects. However, the GTFS-Multi specific tables should be
#' present in the object. These are the grid, access, direct, egress
#' and transfer tables. Use [add_grid_table()], [add_access_tables()],
#' [add_direct_tables()], [add_egress_tables()] and
#' [add_transfer_tables()] to add them to a regular GTFS feed.
#'
#' @export
as_multinet = function(x) {
  assert_multinet(x)
  class(x) = c("multinet", "gtfs", "list")
  x
}

#' Add a grid table to a GTFS feed
#'
#' @param gtfs An object of class [gtfs][gtfsio::import_gtfs()].
#'
#' @param grid A [data.table::data.table()] object created with
#' created with [create_grid_table()].
#'
#' @return An object of class [gtfs][gtfsio::import_gtfs()]. This is a
#' list of multiple [data.table::data.table()] objects corresponding
#' to the different dataset files of a GTFS feed. In addition to those, the
#' returned object will contain a `data.table` representing the reference
#' grid of a GTFS-Multi feed. See details.
#'
#' @details The grid table of a GTFS-Multi feed always has the same structure
#' as the [stops](https://developers.google.com/transit/gtfs/reference#stopstxt)
#' table in a regular GTFS feed. Instead of a true transit stop location, each
#' row represents a grid point (i.e. the centroid of a pixel in the reference
#' grid).
#'
#' @seealso [create_grid_table()]
#'
#' @export
add_grid_table = function(gtfs, grid) {
  gtfs$grid = grid
  gtfs
}

#' Add an access table to a GTFS feed
#'
#' @param gtfs An object of class [gtfs][gtfsio::import_gtfs()].
#'
#' @param ... One or more keyword arguments, with the key being the name of a
#' specific transport mode, and the value being a
#' [data.table::data.table()] object containing the travel times
#' between grid points and transit stops for that mode. Such a table can be
#' created with [create_transfer_table()], using grid points as
#' origins and transit stop locations as destinations.
#'
#' @return An object of class [gtfs][gtfsio::import_gtfs()]. This is a
#' list of multiple [data.table::data.table()] objects corresponding
#' to the different dataset files of a GTFS feed. In addition to those, the
#' returned object will contain a `data.table` representing the access
#' trips of a GTFS-Multi feed. See details.
#'
#' @details The access table of a GTFS-Multi feed always has the same
#' structure as the
#' [transfer](https://developers.google.com/transit/gtfs/reference#transferstxt)
#' table in a regular GTFS feed. Each row contains
#' the minimum travel time needed to travel from a grid point in the reference
#' grid to a transit stop location, using a certain transport mode. For each
#' transport mode, corresponding travel times are stored in a separate column.
#'
#' @note An access table already present in the input gtfs object will be
#' overwritten.
#'
#' @seealso [create_transfer_table()]
#'
#' @export
add_access_tables = function(gtfs, ...) {
  gtfs$access = merge_transfer_tables(...)
  gtfs
}

#' Add a direct table to a GTFS feed
#'
#' @param gtfs An object of class [gtfs][gtfsio::import_gtfs()].
#'
#' @param ... One or more keyword arguments, with the key being the name of a
#' specific transport mode, and the value being a
#' [data.table::data.table()] object containing the travel times
#' between different grid points for that mode. Such a table can be
#' created with [create_transfer_table()], using grid points as
#' both origins and destinations.
#'
#' @return An object of class [gtfs][gtfsio::import_gtfs()]. This is a
#' list of multiple [data.table::data.table()] objects corresponding
#' to the different dataset files of a GTFS feed. In addition to those, the
#' returned object will contain a `data.table` representing the direct
#' trips of a GTFS-Multi feed. See details.
#'
#' @details The direct table of a GTFS-Multi feed always has the same
#' structure as the
#' [transfer](https://developers.google.com/transit/gtfs/reference#transferstxt)
#' table in a regular GTFS feed. Each row contains
#' the minimum travel time needed to travel between two different grid points
#' in the reference grid, using a certain transport mode. For each
#' transport mode, corresponding travel times are stored in a separate column.
#'
#' @note A direct table already present in the input gtfs object will be
#' overwritten.
#'
#' @seealso [create_transfer_table()]
#'
#' @export
add_direct_tables = function(gtfs, ...) {
  gtfs$direct = merge_transfer_tables(...)
  gtfs
}

#' Add an egress table to a GTFS feed
#'
#' @param gtfs An object of class [gtfs][gtfsio::import_gtfs()].
#'
#' @param ... One or more keyword arguments, with the key being the name of a
#' specific transport mode, and the value being a
#' [data.table::data.table()] object containing the travel times
#' between transit stops and grid pointsfor that mode. Such a table can be
#' created with [create_transfer_table()], using transit stop
#' locations as origins and grid points as destinations.
#'
#' @return An object of class [gtfs][gtfsio::import_gtfs()]. This is a
#' list of multiple [data.table::data.table()] objects corresponding
#' to the different dataset files of a GTFS feed. In addition to those, the
#' returned object will contain a `data.table` representing the egress
#' trips of a GTFS-Multi feed. See details.
#'
#' @details The egress table of a GTFS-Multi feed always has the same
#' structure as the
#' [transfer](https://developers.google.com/transit/gtfs/reference#transferstxt)
#' table in a regular GTFS feed. Each row contains
#' the minimum travel time needed to travel from a transit stop location to a
#' grid point in the reference grid, using a certain transport mode. For each
#' transport mode, corresponding travel times are stored in a separate column.
#'
#' @note An egress table already present in the input gtfs object will be
#' overwritten.
#'
#' @seealso [create_transfer_table()]
#'
#' @export
add_egress_tables = function(gtfs, ...) {
  gtfs$egress = merge_transfer_tables(...)
  gtfs
}

#' Add a transfer table to a GTFS feed
#'
#' @param gtfs An object of class [gtfs][gtfsio::import_gtfs()].
#'
#' @param ... One or more keyword arguments, with the key being the name of a
#' specific transport mode, and the value being a
#' [data.table::data.table()] object containing the travel times
#' between different transit stop locations for that mode. Such a table can be
#' created with [create_transfer_table()], using transit stop
#' locations as both origins and destinations.
#'
#' @return An object of class [gtfs][gtfsio::import_gtfs()]. This is a
#' list of multiple [data.table::data.table()] objects corresponding
#' to the different dataset files of a GTFS feed. In addition to those, the
#' returned object will contain a `data.table` representing the transfer
#' trips of a GTFS-Multi feed. See details.
#'
#' @details The transfer table of a GTFS-Multi feed always has the same
#' structure as the
#' [transfer](https://developers.google.com/transit/gtfs/reference#transferstxt)
#' table in a regular GTFS feed. Each row contains
#' the minimum travel time needed to travel between two different transit stop
#' locations, using a certain transport mode. For each
#' transport mode, corresponding travel times are stored in a separate column.
#'
#' @note A transfer table already present in the input gtfs object will be
#' overwritten.
#'
#' @seealso [create_transfer_table()]
#'
#' @export
add_transfer_tables = function(gtfs, ...) {
  gtfs$transfers = merge_transfer_tables(...)
  gtfs
}

#' Create a grid table for a reference grid
#'
#' @param grid The reference grid as object of class
#' [stars][stars::st_as_stars()].
#'
#' @return A [data.table::data.table()] object. See details.
#'
#' @details The grid table of a GTFS-Multi feed always has the same structure
#' as the [stops](https://developers.google.com/transit/gtfs/reference#stopstxt)
#' table in a regular GTFS feed. Instead of a true transit stop location, each
#' row represents a grid point (i.e. the centroid of a pixel in the reference
#' grid).
#'
#' @seealso [create_reference_grid()]
#'
#' @export
create_grid_table = function(grid) {
  points = sf::st_geometry(stars::st_xy2sfc(grid, as_points = TRUE))
  lonlat = sf::st_coordinates(sf::st_transform(points, 4326))
  rows = rep(c(1:nrow(grid)), ncol(grid))[!is.na(grid[[1]])]
  cols = rep(c(1:ncol(grid)), rep(nrow(grid), ncol(grid)))[!is.na(grid[[1]])]
  idxs = paste0("I", rows, "J", cols)
  data.table::data.table(
    stop_id = idxs,
    row_id = rows,
    col_id = cols,
    stop_name = paste("Grid cell", idxs),
    stop_lat = lonlat[, "Y"],
    stop_lon = lonlat[, "X"]
  )
}

#' Create a transfer table for a set of origins and destinations
#'
#' @param origins A [data.table::data.table()] object containing the
#' origin locations of the transfer trips. See details.
#'
#' @param destinations A [data.table::data.table()] object containing
#' the destination locations of the transfer trips. See details.
#'
#' @param streetnet The street network as an object of class
#' [sfnetworks::sfnetwork()] or [dodgr::dodgr_streetnet()].
#'
#' @param d_limit Upper distance limit (as-the-crow-flies) in meters to consider
#' a possible transfer between an origin and a destination.
#'
#' @param min_time Minimum travel time for any transfer trip in seconds,
#' independent of the calculated travel time by the routing algorithm.
#'
#' @param ... Additional parameters passed on to [streetnet_traveltimes()].
#' When parameter `streetnet` is of class [sfnetworks::sfnetwork()], these
#' include the `time_column` and `weight_column` parameters. See
#' [streetnet_traveltimes()] for details.
#'
#' @details Both origins and destinations should be given in the format of a
#' [stops](https://developers.google.com/transit/gtfs/reference#stopstxt) table
#' of a GTFS feed.
#'
#' The format of a
#' [transfer](https://developers.google.com/transit/gtfs/reference#transferstxt)
#' table can be used to model both true transit transfer trips as well as
#' access, direct and egress trips:
#'
#' * When modelling access trips, the origins will be grid points and the
#' destinations transit stop locations.
#' * When modelling egress trips the origins will be transit stop locations and
#' the destinations grid points.
#' * When modelling direct trips both origins and destinations will be grid
#' points.
#' * When modelling true transit transfer trips, both origins and destinations
#' will be transit stop locations.
#'
#' @seealso [add_access_tables()], [add_direct_tables()],
#' [add_egress_tables()], [add_transfer_tables()]
#'
#' @export
create_transfer_table = function(origins, destinations, streetnet,
                                 d_limit = Inf, min_time = 120, ...) {
  olocs = origins[, c("stop_id", "stop_lon", "stop_lat")]
  dlocs = destinations[, c("stop_id", "stop_lon", "stop_lat")]
  pairs = get_od_pairs(olocs, dlocs, d_limit)
  times = streetnet_traveltimes(streetnet, pairs, ...)
  times[times < min_time] = min_time
  data.table::data.table(
    from_stop_id = pairs$from,
    to_stop_id = pairs$to,
    transfer_type = 2,
    transfer_time = ceiling(times)
  )
}

merge_transfer_tables = function(...) {
  x = list(...)
  update_colnames = function(mode) {
    table = x[[mode]]
    names(table)[4] = paste(names(table)[4], mode, sep = "_")
    table
  }
  key = c("from_stop_id", "to_stop_id", "transfer_type")
  mergelist(lapply(names(x), update_colnames), key)
}