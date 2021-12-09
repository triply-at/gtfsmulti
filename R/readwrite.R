#' Import a GTFS-Multi feed from a zip archive
#'
#' @param path Path to a `.zip` file containing all required GTFS-Multi
#' datasets as `.txt` files.
#'
#' @return An object of class `multinet`, which is a list of multiple
#' [`data.table`][data.table::data.table()] objects corresponding to the
#' different dataset files of a GTFS-Multi feed.
#'
#' @export
import_multinet = function(path) {
  G = gtfsrouter::extract_gtfs(path, quiet = TRUE)
  assert_multinet(G)
  G$transfers$min_transfer_time = NULL # gtfsrouter automatically adds this
  # Force from and to stop indices to be character.
  # gtfsrouter already does this automatically for stops and stop times.
  G$access$from_stop_id = as.character(G$access$from_stop_id)
  G$access$to_stop_id = as.character(G$access$to_stop_id)
  G$direct$from_stop_id = as.character(G$direct$from_stop_id)
  G$direct$to_stop_id = as.character(G$direct$to_stop_id)
  G$egress$from_stop_id = as.character(G$egress$from_stop_id)
  G$egress$to_stop_id = as.character(G$egress$to_stop_id)
  G$transfers$from_stop_id = as.character(G$transfers$from_stop_id)
  G$transfers$to_stop_id = as.character(G$transfers$to_stop_id)
  class(G) = c("multinet", "gtfs", "list")
  G
}

#' Write a GTFS-Multi feed to disk
#'
#' @param x An object of class `multinet`, which is a list of multiple
#' [`data.table`][data.table::data.table()] objects corresponding to the
#' different dataset files of a GTFS-Multi feed.
#'
#' @param path Path to a `.zip` file where the GTFS-Multi feed should be
#' written to.
#'
#' @param ... Additional arguments passed on to [gtfsio::export_gtfs()].
#'
#' @export
export_multinet = function(x, path, ...) {
  assert_multinet(x)
  G = data.table::copy(x)
  # Convert stop times from seconds to H:M:S strings.
  if (!is.null(G$stop_times) && !is.null(G$stop_times$arrival_time)) {
    G$stop_times$arrival_time = seconds_to_time(G$stop_times$arrival_time)
    G$stop_times$departure_time = seconds_to_time(G$stop_times$departure_time)
  }
  # Same for start and end times of frequency based trips (if present).
  if (!is.null(G$frequencies)) {
    G$frequencies$start_time = seconds_to_time(G$frequencies$start_time)
    G$frequencies$end_time = seconds_to_time(G$frequencies$end_time)
  }
  gtfsio::export_gtfs(G, path, standard_only = FALSE, ...)
}

#' Assert a GTFS feed is a valid GTFS-Multi feed
#'
#' @param x An object of class `multinet` or `gtfs`.
#'
#' @return Returns nothing if x is a valid GTFS-Multi feed, throws and error
#' otherwise.
#'
#' @details Currently it is only checked if all required datasets are present.
#'
#' @export
assert_multinet = function(x) {
  required = c("agency", "routes", "trips", "stops", "stop_times", "grid",
               "access", "direct", "egress", "transfers")
  present = names(x)
  missing = setdiff(required, present)
  if (length(missing) > 0) {
    stop(
      "Tables (", paste(missing, collapse = ", "), ") required but missing",
      call. = FALSE
    )
  }
}

#' Convert a GTFS-Multi feed to a regular GTFS feed
#'
#' @param x An object of class `multinet`, which is a list of multiple
#' [`data.table`][data.table::data.table()] objects corresponding to the
#' different dataset files of a GTFS-Multi feed.
#'
#' @param access_modes Vector of transport modes to consider for access trips,
#' i.e. trips from grid points towards transit stop locations.
#' Each of these transport modes should have their access travel time for each
#' access trip specified in the access table of `x`, in a column named
#' *transfer_time_{<}mode{>}*. If `NULL`, all modes that have such a column
#' in the access table are considered. Defaults to `NULL`.
#'
#' @param direct_modes Vector of transport modes to consider for direct trips,
#' i.e. trips between grid points.
#' Each of these transport modes should have their direct travel time for each
#' direct trip specified in the direct table of `x`, in a column named
#' *transfer_time_{<}mode{>}*. If `NULL`, all modes that have such a column
#' in the direct table are considered. Defaults to `NULL`.
#'
#' @param egress_modes Vector of transport modes to consider for egress trips,
#' i.e. trips from transit stop locations towards grid points.
#' Each of these transport modes should have their egress travel time for each
#' egress trip specified in the egress table of `x`, in a column named
#' *transfer_time_{<}mode{>}*. If `NULL`, all modes that have such a column
#' in the egress table are considered. Defaults to `NULL`.
#'
#' @param transfer_modes Vector of transport modes to consider for true transit
#' transfer trips, i.e. trips between transit stop locations.
#' Each of these transport modes should have their transfer travel time for
#' each transfer trip specified in the transfer table of `x`, in a column named
#' *transfer_time_{<}mode{>}*. If `NULL`, all modes that have such a column
#' in the transfer table are considered. Defaults to `NULL`.
#'
#' @param access_model How to model access trips, i.e. trips from grid points
#' towards transit stop locations. Can either be "transfers" to model them as
#' if they where transfers between transit stops or "trips" to model them as
#' if they where regular transit trips departing at a certain frequency. This
#' frequency is given in the `access_frequency` parameter. May also be "ignore"
#' to not include them at all in the GTFS feed.
#'
#' @param direct_model How to model direct trips, i.e. trips between grid
#' points. Can either be "transfers" to model them as if they where
#' transfers between transit stops or "trips" to model them as if they
#' where regular transit trips departing at a certain frequency. This
#' frequency is given in the `direct_frequency` parameter. May also be "ignore"
#' to not include them at all in the GTFS feed.
#'
#' @param egress_model How to model egress trips, i.e. trips from transit stop
#' locations towards grid points. Can either be "transfers" to model them as
#' if they where transfers between transit stops or "trips" to model them as
#' if they where regular transit trips departing at a certain frequency. This
#' frequency is given in the `egress_frequency` parameter. May also be "ignore"
#' to not include them at all in the GTFS feed.
#'
#' @param access_frequency If access trips should be modelled as
#' frequency-based transit trips (i.e. if `access_model = "trips"`), at what
#' frequency should these trips operate? Value should be given in seconds.
#' Smaller values will provide more accurate results, since access trips can
#' in reality be started at anytime, but also increase processing time.
#' Defaults to 60, i.e. every minute.
#'
#' @param direct_frequency If direct trips should be modelled as
#' frequency-based transit trips (i.e. if `direct_model = "trips"`), at what
#' frequency should these trips operate? Value should be given in seconds.
#' Smaller values will provide more accurate results, since direct trips can
#' in reality be started at anytime, but also increase processing time.
#' Defaults to 60, i.e. every minute.
#'
#' @param egress_frequency If egress trips should be modelled as
#' frequency-based transit trips (i.e. if `egress_model = "trips"`), at what
#' frequency should these trips operate? Value should be given in seconds.
#' Smaller values will provide more accurate results, since egress trips can
#' in reality be started at anytime, but also increase processing time.
#' Defaults to 60, i.e. every minute.
#'
#' @param time_as_seconds Should arrival and departure times be stored as
#' seconds since midnight? If `FALSE`, they are stored as regular
#' time strings in format "h:m:s". Defaults to `FALSE`.
#'
#' @return An object of class [`gtfs`][gtfsio::import_gtfs()], which is a
#' list of multiple [`data.table`][data.table::data.table()] objects
#' corresponding to the different dataset files of a GTFS feed.
#'
#' @export
multinet_to_gtfs = function(x,
                            access_modes = NULL,
                            direct_modes = NULL,
                            egress_modes = NULL,
                            transfer_modes = NULL,
                            access_model = "transfers",
                            direct_model = "transfers",
                            egress_model = "transfers",
                            access_frequency = 60,
                            direct_frequency = 60,
                            egress_frequency = 60,
                            time_as_seconds = FALSE) {
  G = data.table::copy(x)
  # Get minimum travel times among different modes for all transfer-like trips.
  access = set_transfer_time(G$access, access_modes)
  direct = set_transfer_time(G$access, direct_modes)
  egress = set_transfer_time(G$egress, egress_modes)
  transfers = set_transfer_time(G$transfers, transfer_modes)
  # Model grid points as transit stop locations.
  G$stops = rbind(G$stops, G$grid, fill = TRUE)
  G$grid = NULL
  # Model access trips.
  # How to model them is given by the 'access_model' argument.
  prefix = "(access)"
  G = switch(
    access_model,
    trips = model_as_frequencies(G$access, G, access_frequency, prefix),
    transfers = model_as_transfers(G$access, G),
    ignore = G,
    stop("Unknown model for access trips: '", access_model, "'", call. = FALSE)
  )
  G$access = NULL
  # Model direct trips.
  # How to model them is given by the 'direct_model' argument.
  prefix = "(direct)"
  G = switch(
    direct_model,
    trips = model_as_frequencies(G$direct, G, direct_frequency, prefix),
    transfers = model_as_transfers(G$direct, G),
    ignore = G,
    stop("Unknown model for direct trips: '", direct_model, "'", call. = FALSE)
  )
  G$direct = NULL
  # Model egress trips.
  # How to model them is given by the 'egress_model' argument.
  prefix = "(egress)"
  G = switch(
    egress_model,
    trips = model_as_frequencies(G$egress, G, egress_frequency, prefix),
    transfers = model_as_transfers(G$egress, G),
    ignore = G,
    stop("Unknown model for egress trips: '", egress_model, "'", call. = FALSE)
  )
  G$egress = NULL
  # Convert timestamps stored as seconds since midnight to "h:m:s" strings.
  if (! time_as_seconds) {
    # Arrival and departure times of trips at each stop.
    times = G$stop_times
    if (! is.null(times)) {
      times$arrival_time = seconds_to_time(times$arrival_time)
      times$departure_time = seconds_to_time(times$departure_time)
    }
    # Operating interval bounds of frequency based trips.
    freqs = G$frequencies
    if (! is.null(freqs)) {
      freqs$start_time = seconds_to_time(freqs$start_time)
      freqs$end_time = seconds_to_time(freqs$end_time)
    }
  }
  # Return as object of class "gtfs".
  class(G) = c("gtfs", "list")
  G
}

model_as_frequencies = function(x, gtfs, frequency, prefix) {
  # Define indices.
  # Each connection in x is a unique route and a unique trip.
  # Hence the same indices can be used for routes and trips.
  idxs = paste(prefix, seq_len(nrow(x)), sep = "_")
  # Model all connections as frequency based trips.
  # These are stored in the "frequencies" table of a GTFS feed.
  freqs = data.table::data.table(
    trip_id = idxs,
    start_time = 0,
    end_time = max(24 * 60 * 60, max(gtfs$stop_times$departure_time)),
    headway_secs = frequency,
    exact_times = 0
  )
  gtfs$frequencies = rbind(gtfs$frequencies, freqs, fill = TRUE)
  # Update all other affected tables of the GTFS feed.
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
  routes = data.table::data.table(
    route_id = idxs,
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
    route_id = idxs,
    service_id = prefix,
    trip_id = idxs
  )
  gtfs$trips = rbind(gtfs$trips, trips, fill = TRUE)
  # --> Stop times
  times = data.table::data.table(
    trip_id = rep(idxs, 2),
    stop_id = c(x$from_stop_id, x$to_stop_id),
    stop_sequence = c(rep(1, length(idxs)), rep(2, length(idxs)))
  )
  gtfs$stop_times = rbind(gtfs$stop_times, times, fill = TRUE)
  # Return the updated feed.
  gtfs
}

model_as_transfers = function(x, gtfs, timetable = FALSE) {
  if (timetable) {
    x = data.table::copy(x)
    x$from_stop_id = match(x$from_stop_id, gtfs$stop_ids[[1]])
    x$to_stop_id = match(x$to_stop_id, gtfs$stop_ids[[1]])
  }
  gtfs$transfers = rbind(gtfs$transfers, x, fill = TRUE)
  gtfs
}