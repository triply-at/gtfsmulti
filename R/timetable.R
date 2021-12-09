#' Convert a GTFS-Multi feed into timetable format
#'
#' The timetable format is used by
#' [gtfsrouter][gtfsrouter::gtfs_traveltimes()] to calculate routes on a
#' transit network more efficiently. Besides the regular GTFS datasets, the
#' timetable format contains additional tables that restructure the timetable
#' information into formats that can easily be scanned by the routing
#' algorithm.
#'
#' @param x An object of class `multinet`, which is a list of multiple
#' [data.table::data.table()] objects corresponding to the different dataset
#' files of a GTFS-Multi feed.
#'
#' @param origin Index of the grid point that serves as access location, i.e.
#' the grid point from which access trips can be started. This index should
#' correspond to a value in the *stop_id* column of the grid table in the
#' feed.
#'
#' @param start_time_limits A vector of two integer values denoting the
#' start and end of the time interval for which the timetable should be
#' created. Values should be given in seconds since midnight. Defaults to
#' `c(0, 24 * 60 * 60)`, meaning a timetable for a full day will be
#' constructed. Which day of the week that is should be specified in the
#' `day` parameter.
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
#' in the direct table are considered. If `NA`, egress trips are not included
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
#' *transfer_time_{<}mode{>}*. If `NULL`, all modes that have such a column
#' in the transfer table are considered. If `NA`, transfer trips are not
#' included at all. Defaults to `NULL`.
#'
#' @param access_limit Maximum time in seconds for access trips. Any access
#' trip in the access table of `x` with a travel time higher than this limit
#' will not be considered in the timetable construction. Defaults to `Inf`,
#' meaning that all specified access trips will be considered.
#'
#' @param direct_limit Maximum time in seconds for direct trips. Any direct
#' trip in the direct table of `x` with a travel time higher than this limit
#' will not be considered in the timetable construction. Defaults to `Inf`,
#' meaning that all specified direct trips will be considered.
#'
#' @param egress_limit Maximum time in seconds for egress trips. Any egress
#' trip in the egress table of `x` with a travel time higher than this limit
#' will not be considered in the timetable construction. Defaults to `Inf`,
#' meaning that all specified egress trips will be considered.
#'
#' @param transfer_limit Maximum time in seconds for transfer trips. Any
#' transfer trip in the transfer table of `x` with a travel time higher than
#' this limit will not be considered in the timetable construction. Defaults
#' to `Inf`, meaning that all specified transfer trips will be considered.
#'
#' @param access_model How to model access trips, i.e. trips from grid points
#' towards transit stop locations. Can either be "transfers" to model them as
#' if they where transfers between transit stops or "trips" to model them as
#' if they where regular transit trips departing at a certain frequency. This
#' frequency is given in the `access_frequency` parameter. May also be "ignore"
#' to not include them at all in the relevant tables for routing.
#'
#' @param direct_model How to model direct trips, i.e. trips between grid
#' points. Can either be "transfers" to model them as if they where
#' transfers between transit stops or "trips" to model them as if they
#' where regular transit trips departing at a certain frequency. This
#' frequency is given in the `direct_frequency` parameter. May also be "ignore"
#' to not include them at all in the relevant tables for routing.
#'
#' @param egress_model How to model egress trips, i.e. trips from transit stop
#' locations towards grid points. Can either be "transfers" to model them as
#' if they where transfers between transit stops or "trips" to model them as
#' if they where regular transit trips departing at a certain frequency. This
#' frequency is given in the `egress_frequency` parameter. May also be "ignore"
#' to not include them at all in the relevant tables for routing.
#'
#' @param access_frequency If access trips should be modelled as trips
#' (i.e. if `access_model = "trips"`), at what frequency should these trips
#' operate? Value should be given in seconds. Smaller values will provide more
#' accurate results, since access trips can in reality be started at anytime,
#' but also increase processing time. Defaults to 60, i.e. every minute.
#'
#' @param direct_frequency If direct trips should be modelled as trips
#' (i.e. if `direct_model = "trips"`), at what frequency should these trips
#' operate? Value should be given in seconds. Smaller values will provide more
#' accurate results, since direct trips can in reality be started at anytime,
#' but also increase processing time. Defaults to 60, i.e. every minute.
#'
#' @param egress_frequency If egress trips should be modelled as trips
#' (i.e. if `egress_model = "trips"`), at what frequency should these trips
#' operate? Value should be given in seconds. Smaller values will provide more
#' accurate results, since egress trips can in reality be started at anytime,
#' but also increase processing time. Defaults to 60, i.e. every minute.
#'
#' @param all If access, direct and/or egress trips should be modelled as if
#' they where regular transit trips, should all affected tables in the feed
#' be updated? If `FALSE`, only those tables that are relevant for routing
#' tasks are updated. Irrelevant tables for routing include the routes,
#' trips and stop_times tables, since all the relevant information
#' contained in them is already part of the additonal timetable specific
#' tables.
#'
#' @param quiet Boolean. If `FALSE`, display progress information on screen.
#' Defaults to `FALSE`.
#'
#' @return The original GTFS-Multi feed plus the additional timetable specific
#' tables.
#'
#' @details The timetable format of a GTFS-Multi feed differs from the
#' timetable format of a regular GTFS feed. In the GTFS-Multi version, a
#' timetable is created for a *single* origin location, i.e. a single grid
#' point from which access trips may be started. Also, access trips are bound
#' to be started inside a given time interval.
#'
#' @seealso [multinet_traveltimes()], [gtfsrouter::gtfs_timetable()]
#'
#' @noRd
multinet_timetable = function(x, origin,
                              start_time_limits = c(0, 24 * 60 * 60),
                              day = NULL, date = NULL,
                              access_modes = NULL,
                              direct_modes = NULL,
                              egress_modes = NULL,
                              transfer_modes = NULL,
                              access_limit = Inf,
                              direct_limit = Inf,
                              egress_limit = Inf,
                              transfer_limit = Inf,
                              access_model = "trips",
                              direct_model = "trips",
                              egress_model = "transfers",
                              access_frequency = 60,
                              direct_frequency = 60,
                              egress_frequency = 60,
                              all = TRUE,
                              quiet = FALSE) {
  # ----- PREPARE -----
  # Add a transit timetable to the network.
  G = gtfsrouter::gtfs_timetable(x, day = day, date = date, quiet = quiet)
  G$timetable = G$timetable[G$timetable$departure_time >= start_time_limits[1], ]
  # Prepare access trips.
  # Only consider access trips from the given origin.
  # Set time of each access trips to minimum travel time among specified modes.
  # If specified modes are NA don't include access trips at all.
  if (!is.null(access_modes) && is.na(access_modes)) {
    G$access = create_empty_transfer_table()
    access_model = "ignore"
  } else {
    G$access = set_transfer_time(
      G$access[G$access$from_stop_id == origin, ],
      modes = access_modes,
      max_time = access_limit
    )
  }
  # Prepare direct trips.
  # Only consider direct trips from the given origin.
  # Set time of each direct trips to minimum travel time among specified modes.
  # If specified modes are NA don't include direct trips at all.
  if (!is.null(direct_modes) && is.na(direct_modes)) {
    G$direct = create_empty_transfer_table()
    direct_model = "ignore"
  } else {
    G$direct = set_transfer_time(
      G$direct[G$direct$from_stop_id == origin, ],
      modes = direct_modes,
      max_time = direct_limit
    )
  }
  # Prepare egress trips.
  # Set time of each egress trips to minimum travel time among specified modes.
  # If specified modes are NA don't include egress trips at all.
  if (!is.null(egress_modes) && is.na(egress_modes)) {
    G$egress = create_empty_transfer_table()
    egress_model = "ignore"
  } else {
    G$egress = set_transfer_time(
      G$egress,
      modes = egress_modes,
      max_time = egress_limit
    )
  }
  # Prepare transfers.
  # Set time of each transfer to minimum travel time among specified modes.
  # If specified modes are NA don't include transfers at all.
  if (!is.null(transfer_modes) && is.na(transfer_modes)) {
    G$transfers = create_empty_transfer_table()
  } else {
    G$transfers = set_transfer_time(
      G$transfers,
      modes = transfer_modes,
      max_time = transfer_limit
    )
  }
  # ----- MODEL -----
  # Model grid points as transit stop locations.
  G$stops = rbind(G$stops, G$grid, fill = TRUE)
  G$stop_ids = G$stops[, "stop_id"]
  names(G$stop_ids) = c("stop_ids")
  # Model access trips.
  # How to model them is given by the 'access_model' argument.
  G = switch(
    access_model,
    trips = model_as_timetable(
      G$access,
      gtfs = G,
      bounds = start_time_limits,
      frequency = access_frequency,
      prefix = "(access)",
      all = all
    ),
    transfers = model_as_transfers(
      G$access,
      gtfs = G,
      timetable = TRUE
    ),
    ignore = G,
    stop("Unknown model for access trips: '", access_model, "'", call. = FALSE)
  )
  # Model direct trips.
  # How to model them is given by the 'direct_model' argument.
  prefix = "(direct)"
  G = switch(
    direct_model,
    trips = model_as_timetable(
      G$direct,
      gtfs = G,
      bounds = start_time_limits,
      frequency = direct_frequency,
      prefix = "(direct)",
      all = all
    ),
    transfers = model_as_transfers(
      G$direct,
      gtfs = G,
      timetable = TRUE
    ),
    ignore = G,
    stop("Unknown model for direct trips: '", direct_model, "'", call. = FALSE)
  )
  # Model egress trips.
  # How to model them is given by the 'egress_model' argument.
  prefix = "(egress)"
  G = switch(
    egress_model,
    trips = model_as_timetable(
      G$egress,
      gtfs = G,
      bounds = start_time_limits,
      frequency = egress_frequency,
      prefix = "(egress)",
      all = all
    ),
    transfers = model_as_transfers(
      G$egress,
      gtfs = G,
      timetable = TRUE
    ),
    ignore = G,
    stop("Unknown model for egress trips: '", egress_model, "'", call. = FALSE)
  )
  # ----- POST-PROCESS ------
  # Add attributes
  attr(G, "origin") = origin
  attr(G, "bounds") = start_time_limits
  attr(G, "access") = list(
    model = access_model,
    frequency = if (access_model == "trips") access_frequency else NULL
  )
  attr(G, "direct") = list(
    model = direct_model,
    frequency = if (direct_model == "trips") direct_frequency else NULL
  )
  attr(G, "egress") = list(
    model = egress_model,
    frequency = if (egress_model == "trips") egress_frequency else NULL
  )
  # Return the updated feed.
  G
}

model_as_timetable = function(x, gtfs, bounds, frequency, prefix, all = TRUE) {
  # Parse.
  from = match(x$from_stop_id, gtfs$stop_ids[[1]]) # rownumbers of origins
  to = match(x$to_stop_id, gtfs$stop_ids[[1]]) # rownumbers of destinations
  cons = nrow(x) # number of origin-destination connections
  durs = x$min_transfer_time # duration of travel for each connection
  deps = seq(bounds[1], bounds[2], frequency) # departure times of trips
  reps = rep(length(deps), cons) # number of trips for each connection
  idx = nrow(gtfs$trip_ids) + 1 # trip id of first trip
  # Construct timetable for new trips.
  timetable = data.table::data.table(
    departure_station = rep(from, reps),
    arrival_station = rep(to, reps),
    departure_time = rep(deps, cons),
    arrival_time = rep(deps, cons) + rep(durs, reps),
    trip_id = seq(idx, idx + sum(reps) - 1)
  )
  # Create indices for new trips.
  # In the timetable itself ids are trip rownumbers rather than real indices.
  # Here we map these rownumbers to a real indices.
  trip_ids = data.table::data.table(
    trip_ids = paste(prefix, timetable$trip_id, sep = "_")
  )
  # Write timetable of new trips to feed.
  gtfs$timetable = rbind(gtfs$timetable, timetable)
  gtfs$timetable = gtfs$timetable[order(gtfs$timetable$departure_time), ]
  # Write indices of new trips to feed.
  gtfs$trip_ids = rbind(gtfs$trip_ids, trip_ids)
  # Update all other tables in feed if requested.
  if (all) {
    # --> Agency
    agency = data.table::data.table(
      agency_id = prefix,
      agency_name = prefix,
      agency_url = "https://example.com/",
      agency_timezone = gtfs$agency$agency_timezone[1]
    )
    gtfs$agency = rbind(gtfs$agency, agency, fill = TRUE)
    # --> Calendar
    if ("calendar" %in% names(gtfs)) {
      service = data.table::data.table(
        service_id = prefix,
        monday = 1,
        tuesday = 1,
        wednesday = 1,
        thursday = 1,
        friday = 1,
        saturday = 1,
        sunday = 1,
        start_date = min(gtfs$calendar$start_date),
        end_date = max(gtfs$calendar$end_date)
      )
      gtfs$calendar = rbind(gtfs$calendar, service, fill = TRUE)
    } else {
      service = data.table::data.table(
        service_id = prefix,
        date = unique(gtfs$calendar_dates$date),
        exception_type = 1
      )
      gtfs$calendar_dates = rbind(gtfs$calendar_dates, service, fill = TRUE)
    }
    # --> Routes
    route_ids = paste(prefix, seq_len(cons), sep = "_")
    routes = data.table::data.table(
      route_id = route_ids,
      agency_id = prefix,
      route_short_name = prefix,
      route_type = 100
    )
    if (is.null(gtfs$routes$agency_id)) {
      gtfs$routes$agency_id = gtfs$agency$agency_id[1]
    }
    gtfs$routes = rbind(gtfs$routes, routes, fill = TRUE)
    # --> Trips
    trips = data.table::data.table(
      route_id = rep(route_ids, reps),
      service_id = prefix,
      trip_id = trip_ids
    )
    gtfs$trips = rbind(gtfs$trips, trips, fill = TRUE)
    # --> Stop times
    times = data.table::data.table(
      trip_id = rep(trip_ids, 2),
      stop_id = c(rep(x$from_stop_id, reps), rep(x$to_stop_id, reps)),
      stop_sequence = c(rep(1, sum(reps)), rep(2, sum(reps))),
      arrival_time = c(rep(deps, cons), rep(deps, cons) + rep(durs, reps)),
      departure_time = c(rep(deps, cons), rep(deps, cons) + rep(durs, reps))
    )
    gtfs$stop_times = rbind(gtfs$stop_times, times, fill = TRUE)
  }
  gtfs
}