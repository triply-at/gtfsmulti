#' Import a regular GTFS feed from a zip archive
#'
#' @param path Path to a `.zip` file containing all required GTFS
#' datasets as `.txt` files.
#'
#' @param extent The area of interest as object of class [sf::sf()]. Only
#' transit stop locations inside this area will be imported. Trips, routes,
#' agencies, services and if present also transfers, pathways and shapes will
#' be filtered accordingly, such that only those with a relation to the
#' selected stops are imported. If `NULL`, the complete GTFS feed is imported.
#'
#' @return An object of class [gtfs][gtfsio::import_gtfs()], which is a
#' list of multiple [data.table::data.table()] objects corresponding
#' to the different dataset files of a GTFS feed.
#'
#' @export
import_transitnet = function(path, extent = NULL) {
  G = suppressWarnings(gtfsrouter::extract_gtfs(path, quiet = TRUE))
  if (! is.null(extent)) {
    # GTFS file will be spatially filtered by the given extent.
    # First transform the extent to EPSG:4326.
    extent = sf::st_transform(extent, 4326)
    # Suppress no-visible binding notes.
    stop_id = trip_id = route_id = agency_id = service_id = shape_id = NULL
    from_stop_id = to_stop_id = NULL
    # Filter stops.
    # Only keep stops that intersect with the given spatial extent.
    stop_pts = sfheaders::sfc_point(G$stops[, c("stop_lon", "stop_lat")])
    sf::st_crs(stop_pts) = 4326
    keep_stops = G$stops[sf::st_intersects(extent, stop_pts)[[1]], ]$stop_id
    G$stops = subset(G$stops, stop_id %in% keep_stops)
    # Filter trips.
    # Only keep trips that contain filtered stops.
    keep_trips = unique(G$stop_times$trip_id)
    G$trips = subset(G$trips, trip_id %in% keep_trips)
    # Filter timetable.
    # Only keep exact stop times at filtered stops.
    # Timetables may be specified frequency based rather than with exact times.
    # In that case only keep frequencies belonging to filtered trips.
    G$stop_times = subset(G$stop_times, stop_id %in% keep_stops)
    if (! is.null(G$frequencies)) {
      G$frequencies = subset(G$trips, trip_id %in% keep_trips)
    }
    # Filter routes.
    # Only keep routes that contain filtered trips.
    keep_routes = unique(G$trips$route_id)
    G$routes = subset(G$routes, route_id %in% keep_routes)
    # Filter agencies.
    # Only keep agencies that operate filtered routes.
    # Routes don't need to specify agency ids if there is only one agency.
    # In that case just keep all agencies.
    if (! is.null(G$routes$agency_id)) {
      keep_agencies = unique(G$routes$agency_id)
      G$agency = subset(G$agency, agency_id %in% keep_agencies)
    }
    # Filter services.
    # Only keep services that serve filtered trips.
    keep_services = unique(G$trips$service_id)
    if (! is.null(G$calendar)) {
      G$calendar = subset(G$calendar, service_id %in% keep_services)
    }
    if (! is.null(G$calendar_dates)) {
      G$calendar_dates = subset(G$calendar_dates, service_id %in% keep_services)
    }
    # Filter shapes.
    # Only keep shapes belonging to filtered trips.
    if (!is.null(G$shapes) && !is.null(G$trips$shape_id)) {
      keep_shapes = unique(G$trips$shape_id)
      G$shapes = subset(G$shapes, shape_id %in% keep_shapes)
    }
    # Filter transfers.
    # Only keep transfers between filtered stops.
    if (! is.null(G$transfers)) {
      G$transfers = subset(G$transfers, from_stop_id %in% keep_stops & to_stop_id %in% keep_stops)
    }
    # Filter pathways.
    # Only keep pathways between filtered stops.
    if (! is.null(G$pathways)) {
      G$pathways = subset(G$pathways, from_stop_id %in% keep_stops & to_stop_id %in% keep_stops)
    }
  }
  G
}