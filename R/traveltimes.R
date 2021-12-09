#' Calculate travel times from a single origin to all destinations
#'
#' This function calculates travel times in a multi-modal network from a
#' specified location anywhere within the grid of a GTFS-Multi feed to all grid
#' points of that grid. The travel times are the shortest multi-modal
#' travel times within the bounds of acceptable departure times. Travel
#' times may also be single-modal in case it is faster to travel directly
#' from the origin to the destination, i.e. without using transit at all.
#'
#' @param x An object of class `multinet`, which is a list of multiple
#' [data.table::data.table()] objects corresponding to the different dataset
#' files of a GTFS-Multi feed.
#'
#' @param from Origin of the routes. May be the index of a grid point. In that
#' case it should correspond to a value in the *stop_id* column of the grid
#' table in x. May also be any location inside the extent of the grid. Such a
#' location may be given as a single `POINT` geometry inside an object of
#' either class [sf::sf()] or class [sfc][sf::st_sfc()]. May also be given as
#' a numeric vector containing the two coordinate values. In any case,
#' coordinates are expected to be expressed as longitude and latitude values
#' with WGS84 as geodetic datum.
#'
#' @param start_time_limits A vector of two integer values denoting the start
#' and end of the time interval in which trips may be started. Values should
#' be given in seconds since midnight.
#'
#' @param access_modes Vector of transport modes to consider for access trips,
#' i.e. trips from grid points towards transit stop locations.
#' Each of these transport modes should have their access travel time for each
#' access trip specified in the access table of `x`, in a column named
#' *transfer_time_{<}mode{>}*. If `NULL`, all modes that have such a column
#' in the access table are considered. If `NA`, access trips are not included
#' at all. Defaults to `NULL`.
#'
#' @param direct_modes Vector of transport modes to consider for direct trips,
#' i.e. trips between grid points.
#' Each of these transport modes should have their direct travel time for each
#' direct trip specified in the direct table of `x`, in a column named
#' *transfer_time_{<}mode{>}*. If `NULL`, all modes that have such a column
#' in the direct table are considered. If `NA`, direct trips are not included
#' at all. Defaults to `NULL`.
#'
#' @param egress_modes Vector of transport modes to consider for egress trips,
#' i.e. trips from transit stop locations towards grid points.
#' Each of these transport modes should have their egress travel time for each
#' egress trip specified in the egress table of `x`, in a column named
#' *transfer_time_{<}mode{>}*. If `NULL`, all modes that have such a column
#' in the egress table are considered. If `NA`, egress trips are not included
#' at all. Defaults to `NULL`.
#'
#' @param transfer_modes Vector of transport modes to consider for true transit
#' transfer trips, i.e. trips between transit stop locations.
#' Each of these transport modes should have their transfer travel time for
#' each transfer trip specified in the transfer table of `x`, in a column named
#' *transfer_time_{mode}*. If `NULL`, all modes that have such a column
#' in the transfer table are considered. If `NA`, transfer trips are not
#' included at all. Defaults to `NULL`.
#'
#' @param access_limit Maximum time in seconds for access trips. Any access
#' trip in the access table of `x` with a travel time higher than this limit
#' will not be considered in the travel time calculation. Defaults to `Inf`,
#' meaning that all specified access trips will be considered.
#'
#' @param direct_limit Maximum time in seconds for direct trips. Any direct
#' trip in the direct table of `x` with a travel time higher than this limit
#' will not be considered in the travel time calculation. Defaults to `Inf`,
#' meaning that all specified direct trips will be considered.
#'
#' @param egress_limit Maximum time in seconds for egress trips. Any egress
#' trip in the egress table of `x` with a travel time higher than this limit
#' will not be considered in the travel time calculation. Defaults to `Inf`,
#' meaning that all specified egress trips will be considered.
#'
#' @param transfer_limit Maximum time in seconds for transfer trips. Any
#' transfer trip in the transfer table of `x` with a travel time higher than
#' this limit will not be considered in the travel time calculation. Defaults
#' to `Inf`, meaning that all specified transfer trips will be considered.
#'
#' @param minimise_transfers Boolean. If `TRUE`, travel times are calculated
#' for routes with minimal transfer connections between the origin and each
#' destination, even if those are slower than travel times of alternative
#' routes with more transfers. Defaults to `FALSE`.
#'
#' @param include_grid_access Boolean. If `TRUE`, the straight-line distance
#' between the origin location and its nearest grid point is assumed to be
#' travelled at the speed given by the `grid_access_speed` parameter. The
#' resulting travel time is added to the calculated travel times originating
#' from the nearest grid point. If `FALSE`, this "pre-leg" of the route is
#' ignored. Defaults to `TRUE`.
#'
#' @param iterate Boolean. If `TRUE`, a separate route search is performed for
#' every possible departure time of an access trip within the time interval.
#' For each of these departure times, the shortest travel times to all
#' destinations are returned in a separate column, with the restriction that
#' trips need to start *exactly* at this time, instead of at anytime within a
#' time interval. This is similar to the
#' [R5 approach](https://docs.conveyal.com/learn-more/faq#how-does-conveyal-analysis-calculate-travel-times)
#' for multi-modal routing. With the output, it is possible to create a
#' distribution of travel times, and thus evaluate the quality of multi-modal
#' connections not only by the shortest possible travel time. The frequency of
#' departure times can be tuned by setting the `access_frequency` parameter.
#'
#' @param quiet Boolean. If `FALSE`, display progress information on screen.
#' Defaults to `FALSE`.
#'
#' @param access_frequency At what frequency should access trips be allowed to
#' start? Value should be given in seconds. Smaller values will provide more
#' accurate results, since access trips can in reality be started at anytime,
#' but also increase processing time. Defaults to 60, i.e. every minute.
#'
#' @param grid_access_speed Speed at which the straight-line distance between
#' the origin location and its nearest grid point can be travelled. Only
#' considered when `include_grid_access = TRUE`. Value should be given in m/s.
#' Defaults to 5 / 3.6, i.e. 5 km/h.
#
#' @param router_timeout Cumulative travel time in seconds after which the
#' routing algorithm will exit the search for a route. Hence, if during the
#' search for a route between the selected origin grid point and a transit stop
#' (see step 2 in Details) the cumulative travel time exceeds this value, the
#' algorithm will stop looking for a connection and marks the particular
#' transit stop as unreachable. Defaults to 60 * 60, i.e. one hour.
#'
#' @param day Day of the week for which the timetable should be created. May
#' be given as unambiguous string (e.g. "tu" and "th" for Tuesday and
#' Thursday), or an integer between 1 = Sunday and 7 = Saturday. If `NULL`,
#' the current day will be used, except when `date` is not set to `NULL`.
#'
#' @param date Date for which the timetable should be created. Should be given
#' as a single 8-digit integer representing "yyyymmdd". This parameter is meant
#' to be used with feeds that contain a
#' [calendar_dates](https://developers.google.com/transit/gtfs/reference#calendar_datestxt)
#' table, either to specify service availability separately for each date, or
#' to list exceptions to the regular service availability as specified in the
#' [calendar](https://developers.google.com/transit/gtfs/reference#calendartxt)
#' table. Ignored when `day` is not set to `NULL`.
#'
#' @return The grid table of the GTFS-Multi feed with an additonal column
#' containing for each grid point the calculated travel time from the given
#' origin location to that grid point. Grid points that could not be reached
#' get a travel time value of `NA`. If `iterate = TRUE`, there will be multiple
#' travel time columns instead of only one. Each column then represents the
#' travel time for a fixed departure time.
#'
#' @details Travel times are calculated as with the following steps:
#'
#' 1. The nearest grid point to `origin` is found. An imaginary pre-leg of the
#' trip accounts to some extent for the distance between these two points. This
#' distance (as-the-crow-flies) is assumed to be travelled at a fixed speed,
#' specified in the `grid_access_speed` parameter. By default its value is set
#' to a walking-like speed of 5 / 3.6 m/s, i.e. 5 km/h. When setting
#' `include_grid_access = FALSE` the distance between the actual origin
#' location and its nearest grid point is not accounted for and the trip is
#' assumed to effectively start at the nearest grid point. Keep again in mind
#' that no matter what approach you choose, results get less accurate with a
#' coarser grid.
#'
#' 2. For each transit stop listed in the *stops table* of `x`, the shortest
#' multi-modal travel time from the selected grid point to that stop is
#' calculated. This is done by a purely timetable-based routing algorithm.
#' Access trips listed in the *access table* of `x` that originate from the
#' selected grid point are included in the transit timetable by modelling them
#' as direct transit trips from the selected grid point towards reachable
#' transit stops, operating on a frequency-based schedule. The frequency of
#' these trips is defined by the `access_frequency` parameter, which by default
#' is set to 60 seconds, i.e. one departure every minute. The length of such an
#' access trip is the minimum travel time for a given access connection among
#' all different modes that are present in the access table. The modes can be
#' subsetted by setting the `access_modes` parameter, e.g.
#' `access_modes = "walk"` to include only access times by foot. Possible trips
#' itself may be filtered as well, by setting a maximum allowed travel time
#' through the `access_limit` parameter. During the transit leg of the trip it
#' is possible to transfer from one vehicle to another using a transfer
#' connection listed in the *transfers table* of `x`. The travel time of such
#' a transfer connection is the minimum travel time among all different modes
#' that are present in the transfer table. The modes can be subsetted by
#' setting the `transfer_modes` parameter, e.g. `transfer_modes = "walk"` to
#' include only transfer times by foot. Possible trips itself may be filtered
#' as well, by setting a maximum allowed travel time through the
#' `transfer_limit` parameter. For efficiency reasons, the routing algorithm in
#' this phase has an upper bound defined, meaning that if the cumulative travel
#' time of a trip from the selected grid point exceeds this upper bound before
#' the transit stop is reached, the search for a route to that stop is
#' terminated and the transit stop is marked as unreachable. This upper bound
#' can be defined through the `router_timeout` parameter and defaults to
#' 60 * 60 seconds, i.e. 1 hour.
#'
#' 3. For each destination grid point (i.e. all grid points except the one
#' selected as origin) listed in the *grid table* of `x`, multi-modal travel
#' times from the selected origin grid point to that destination grid point are
#' calculated. This is done by extracting travel times of egress trips that
#' lead to the destination grid point from the *egress table* of `x`, and
#' adding those to the travel times from the origin grid point towards transit
#' stops as calculated in the previous step. For a destination grid point
#' this will often result in a set of multiple travel times (since there may be
#' multiple egress trips leading to the same grid point), of which only the
#' shortest one is preserved. The travel time of an egress connection is the
#' minimum travel time among all different modes that are present in the egress
#' table. The modes can be subsetted by setting the `egress_modes` parameter,
#' e.g. `egress_modes = "walk"` to include only egress times by foot. Possible
#' trips itself may be filtered as well, by setting a maximum allowed travel
#' time through the `egress_limit` parameter.
#'
#' 4. For each destination grid point listed in the *grid table* of `x`, the
#' shortest multi-modal travel time (as calculated in the step above) is
#' compared to the travel time of a single-modal direct trip listed in the
#' *direct table* of `x`. The travel time of such a direct connection is the
#' minimum travel time among all different modes that are present in the
#' direct table. The modes can be subsetted by setting the `direct_modes`
#' parameter, e.g. `direct_modes = "bike"` to include only direct times by
#' bicycle. Possible trips itself may be filtered as well, by setting a
#' maximum allowed travel time through the `direct_limit` parameter. The
#' minimum between the multi-modal travel times and the single-modal travel
#' time is the returned shortest travel time from the selected origin grid
#' point to the destination grid point.
#'
#' @seealso [gtfsrouter::gtfs_traveltimes()]
#'
#' @export
multinet_traveltimes = function(x, from, start_time_limits,
                                access_modes = NULL,
                                direct_modes = NULL,
                                egress_modes = NULL,
                                transfer_modes = NULL,
                                access_limit = Inf,
                                direct_limit = Inf,
                                egress_limit = Inf,
                                transfer_limit = Inf,
                                minimise_transfers = FALSE,
                                include_grid_access = TRUE,
                                iterate = FALSE,
                                quiet = FALSE,
                                access_frequency = 60,
                                grid_access_speed = 5 / 3.6,
                                router_timeout = 60 * 60,
                                day = NULL, date = NULL) {
  # Parse origin.
  if (inherits(from, "sf") || inherits(from, "sfc")) {
    grid_pts = x$grid[, c("stop_id", "stop_lon", "stop_lat")]
    from = get_nearest_neighbour(sf::st_coordinates(from), grid_pts)
  } else if (is.numeric(from)) {
    grid_pts = x$grid[, c("stop_id", "stop_lon", "stop_lat")]
    from = get_nearest_neighbour(from, grid_pts)
  }
  # Construct the timetable.
  x = multinet_timetable(
    x,
    from$idx,
    start_time_limits,
    day = day,
    date = date,
    access_modes = access_modes,
    direct_modes = direct_modes,
    egress_modes = egress_modes,
    transfer_modes = transfer_modes,
    access_limit = access_limit,
    direct_limit = direct_limit,
    egress_limit = egress_limit,
    transfer_limit = transfer_limit,
    access_model = "trips",
    direct_model = "ignore",
    egress_model = "ignore",
    access_frequency = access_frequency,
    all = FALSE,
    quiet = quiet
  )
  # Calculate traveltime from the origin to the nearest grid point.
  if (include_grid_access) {
    grid_access_time = from$dist * grid_access_speed
  } else {
    grid_access_time = 0
  }
  # Calculate traveltimes from the origin to all grid points.
  timetable_traveltimes(
    x,
    start_time_limits,
    router_timeout = router_timeout,
    add_traveltime = grid_access_time,
    minimise_transfers = minimise_transfers,
    iterate = iterate,
    quiet = quiet
  )
}

timetable_traveltimes = function(x, start_time_limits,
                                 router_timeout = 60 * 60,
                                 add_traveltime = 0,
                                 minimise_transfers = FALSE,
                                 iterate = FALSE,
                                 quiet = FALSE) {
  # Calculate traveltimes.
  if (iterate) {
    traveltimes = get_all_traveltimes(
      x,
      start_time_limits,
      router_timeout = router_timeout,
      add_traveltime = add_traveltime,
      minimise_transfers = minimise_transfers,
      quiet = quiet
    )
  } else {
    traveltimes = get_shortest_traveltimes(
      x,
      start_time_limits,
      router_timeout = router_timeout,
      add_traveltime = add_traveltime,
      minimise_transfers = minimise_transfers,
      quiet = quiet
    )
  }
  # Add travel times to grid.
  grid = merge(x$grid, traveltimes, by = "stop_id", all.x = TRUE)
  grid[order(grid$col_id, grid$row_id), ]
}

get_all_traveltimes = function(x, start_time_limits,
                               column_prefix = "travel_time",
                               router_timeout = 60 * 60,
                               add_traveltime = 0,
                               minimise_transfers = FALSE,
                               quiet = FALSE) {
  frequency = attr(x, "access")$frequency
  start_times = seq(start_time_limits[1], start_time_limits[2], frequency)
  run_iteration = function(start_time) {
    colname = paste(column_prefix, start_time, sep = "_")
    get_shortest_traveltimes(
      x,
      start_time_limits = c(start_time, start_time),
      column_name = colname,
      router_timeout = router_timeout,
      add_traveltime = add_traveltime,
      minimise_transfers = minimise_transfers,
      quiet = quiet
    )
  }
  if (quiet) {
    traveltimes = lapply(start_times, run_iteration)
  } else {
    traveltimes = pbapply::pblapply(start_times, run_iteration)
  }
  mergelist(traveltimes, "stop_id")
}

get_shortest_traveltimes = function(x, start_time_limits,
                                    column_name = "travel_time",
                                    router_timeout = 60 * 60,
                                    add_traveltime = 0,
                                    minimise_transfers = FALSE,
                                    quiet = FALSE) {
  # Calculate travel times from origin to all transit stops.
  to_stops = gtfsrouter::gtfs_traveltimes(
    x,
    from = attr(x, "origin"),
    start_time_limits = start_time_limits,
    max_traveltime = router_timeout,
    minimise_transfers = minimise_transfers,
    from_is_id = TRUE
  )
  to_stops$duration = as.integer(to_stops$duration)
  # Add the egress time from the transit stop to a grid cell.
  # This gives us travel times from the origin through transit to grid cells.
  to_cells = merge(
    x$egress,
    to_stops[, c("stop_id", "duration")],
    by.x = "from_stop_id",
    by.y = "stop_id",
    all.x = TRUE
  )
  to_cells$travel_time = to_cells$min_transfer_time + to_cells$duration
  # To obtain all travel times add direct trips between origin and grid cells.
  # Those are the travel times without using transit.
  traveltimes = rbind(
    to_cells[, c("to_stop_id", "travel_time")],
    x$direct[, c("to_stop_id", "min_transfer_time")],
    use.names = FALSE
  )
  # Keep only the minimum travel time for each grid cell.
  traveltimes = traveltimes[order(traveltimes$travel_time), ]
  traveltimes = traveltimes[!duplicated(traveltimes$to_stop_id), ]
  # Add the constant additional travel time.
  traveltimes$travel_time = traveltimes$travel_time + add_traveltime
  # Return.
  names(traveltimes) = c("stop_id", column_name)
  traveltimes
}