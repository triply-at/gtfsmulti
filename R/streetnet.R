#' Default highway types to be included in OpenStreetMap street import
#'
#' @seealso [import_streetnet()]
#'
#' @export
DEFAULT_HIGHWAY_TYPES = c(
  "motorway",
  "motorway_link",
  "trunk",
  "trunk_link",
  "primary",
  "primary_link",
  "secondary",
  "secondary_link",
  "tertiary",
  "tertiary_link",
  "unclassified",
  "residential",
  "living_street",
  "service",
  "pedestrian",
  "track",
  "footway",
  "bridleway",
  "steps",
  "path",
  "cycleway"
)

#' Default tags to be included as columns in OpenStreetMap street import
#'
#' @seealso [import_streetnet()]
#'
#' @export
DEFAULT_TAGS = c(
  "highway",
  "oneway",
  "name",
  "lanes",
  "maxspeed",
  "surface"
)

#' Import streets from an OpenStreetMap extract
#'
#' @param path Path to a `.osm.pbf` file containing the extracted OpenStreetMap
#' data for at least the extent of the area of interest.
#'
#' @param extent The area of interest as object of class [sf::sf()]. Only
#' streets that intersect this area will be imported. If `NULL`, streets are
#' imported for the complete OpenStreetMap extract.
#'
#' @param clip Boolean. If `TRUE`, linestrings will be clipped by the given
#' extent. If `FALSE`, linestrings that intersect the given extent will be
#' imported as a whole. Defaults to `FALSE`. Ignored when `extent = NULL`.
#'
#' @param highway_types A vector of possible values for OpenStreetMaps
#' [highway tag](https://wiki.openstreetmap.org/wiki/Key:highway). Only streets
#' that have one of these values for the highway tag will be imported. Defaults
#' to [DEFAULT_HIGHWAY_TYPES].
#'
#' @param tags A vector of OpenStreetMap
#' [tags](https://wiki.openstreetmap.org/wiki/Tags) that should be included as
#' attribute columns in the imported data. Defaults to [DEFAULT_TAGS].
#'
#' @param quiet Boolean. If `FALSE`, display progress information on screen.
#' Defaults to `FALSE`.
#'
#' @return An object of class [sf::sf()] with `LINESTRING` or
#' `MULTILINESTRING` geometries.
#'
#' @export
import_streetnet = function(path, extent = NULL, clip = FALSE,
                            highway_types = DEFAULT_HIGHWAY_TYPES,
                            tags = DEFAULT_TAGS, quiet = FALSE) {
  # Define query.
  types = paste0("IN (", toString(paste0("'", highway_types, "'")), ")")
  query = paste("SELECT * FROM 'lines' WHERE highway", types)
  # Define tags to include as columns.
  # Some tags are always included when extracting lines from a pbf file.
  # This is defined in the osmconf.ini file that the GDAL driver uses.
  # If these fixed tags are not in the requested tags we need to delete them.
  fix = c("name", "highway", "waterway", "aerialway", "barrier", "man_made")
  del = fix[!(fix %in% tags)]
  add = tags[!(tags %in% fix)]
  # Some additional columns are added automatically during extracting.
  # 'z_order' defines in what order to render the lines.
  # 'other_tags' contains key:value pairs of all non-column tags.
  # We will not include any of this additional columns in the output.
  del = c(del, "z_order", "other_tags")
  # Extract data from the source.
  if (is.null(extent)) {
    data = osmextract::oe_read(
      path,
      query = query,
      extra_tags = add,
      quiet = quiet
    )
  } else {
    # OSM data will be spatially filtered by the given extent.
    # First transform the extent to EPSG:4326.
    extent = sf::st_transform(extent, 4326)
    data = osmextract::oe_read(
      path,
      query = query,
      boundary = extent,
      boundary_type = if (clip) "clipsrc" else "spat",
      extra_tags = add,
      quiet = quiet
    )
  }
  data = data[, !(names(data) %in% del)]
  data
}

#' Build a routable graph from a set of linestrings
#'
#' @param x An object of class [sf::sf()] with `LINESTRING` or
#' `MULTILINESTRING` geometries.
#'
#' @param protect Set of points that protect their nearest linestring vertex
#' from being smoothed (see Details). That is, the closest linestring vertices
#' to these points are guaranteed to be a node in the resulting graph, even if
#' they are neither a terminal node nor a junction node. May be given either as
#' an object of class [sf::sf()] with `POINT` geometries, or as a two-column
#' numeric matrix containing respectively the longitude and latitude
#' coordinates of the points.
#'
#' @return An object of class [sfnetworks::sfnetwork()].
#'
#' @details Graph building is implemented as a step-wise process. The graph
#' will be directed, so first all linestring geometries of `x` that are *not*
#' marked as being a oneway street (through the *oneway* attribute column if
#' present) are duplicated and reversed. Then, all the linestring geometries
#' are split such that their segments become separate linestring geometries
#' on their own. These serve as the edges in the initial graph, and their
#' endpoints become the nodes. Endpoints that are shared between multiple edges
#' become a single node, such that these edges are connected. Only the largest
#' connected component of the initial graph is preserved. Finally, the initial
#' graph is smoothed by removing all nodes that are neither a terminal node
#' (i.e. a node at the end of an edge without a connection to any other edge)
#' nor a junction node (i.e. a node that connects more than two edges) nor a
#' nearest node to any of the points passed to the `protect` parameter.
#'
#' @note Coordinates should be expressed in coordinate reference system
#' [EPSG:4326](https://epsg.io/4326). The function assumes this to be true, and
#' does not check for it.
#'
#' @export
build_streetnet = function(x, protect = NULL) {
  # Create a custom dodgr weighting profile.
  # This is just because we always need a weighting profile in dodgr.
  wp = data.frame(
    name = "init",
    way = unique(x$highway),
    value = rep(1, length(unique(x$highway)))
  )
  # Define the columns that should be present in the output.
  # dodgr always includes a fixed set of columns when building a streetnet.
  # All other columns are by default removed by dodgr.
  # You need to explicitly request dodgr to keep columns.
  # We want to tune the default behaviour by keeping all osm tag columns.
  # And by deleting some of the columns that dodgr added automatically.
  fix = c("lanes", "maxspeed", "surface")
  tag = setdiff(names(x), c(attr(x, "sf_column"), "osm_id"))
  del = fix[!(fix %in% tag)]
  add = tag[!(tag %in% fix)]
  # Build the initial graph as a dodgr style streetnet.
  net = dodgr::weight_streetnet(x, wp, keep_cols = add)
  # Keep only the largest connected component.
  net = net[net$component == 1, ]
  # Smooth the initial graph but keep the protected vertices.
  verts = dodgr::dodgr_vertices(net)
  if (is.null(protect)) {
    keep_verts = NULL
  } else {
    keep_verts = verts$id[dodgr::match_points_to_graph(verts, protect)]
  }
  net = dodgr::dodgr_contract_graph(net, verts = unique(keep_verts))
  # Return as sfnetwork.
  dodgr_to_sfnetwork(net[, !(names(net) %in% del)])
}

#' Calculate travel times on a street network
#'
#' @param x The street network as object of class [sfnetworks::sfnetwork()],
#' [dodgr::dodgr_streetnet()] or
#' [dodgr::dodgr_streetnet_sc()][dodgr::dodgr_streetnet()].
#'
#' @param od_pairs A [data.table::data.table()] object in which each row
#' contains the origin and destination location of a route for which the travel
#' time needs to be calculated. The table should contain the columns
#' *from_lon*, *from_lat*, *to_lon* and *to_lat* containing respectively the
#' longitude and latitude coordinates of respectively the origin and
#' destination.
#'
#' @param ... Ignored.
#'
#' @return An integer vector of travel times in seconds for each of the given
#' OD pairs.
#'
#' @note Coordinates should be expressed in coordinate reference system
#' [EPSG:4326](https://epsg.io/4326). The function assumes this to be true, and
#' does not check for it.
#'
#' @export
streetnet_traveltimes = function(x, od_pairs, ...) {
  UseMethod("streetnet_traveltimes")
}

#' @name streetnet_traveltimes
#'
#' @param time_column Name of the column in the edges table of the street
#' network that contains the travel time values for each edge.
#'
#' @param weight_column Name of the column in the edges table of the street
#' network that contains the weight of each edge. These value are used to find
#' the shortest path (i.e. the path with minimum weight) between an origin and
#' a destination.
#'
#' @export
streetnet_traveltimes.sfnetwork = function(x, od_pairs, time_column = "time",
                                           weight_column = "weight", ...) {
  x = sfnetwork_to_dodgr(x)
  # Check if referenced columns are present.
  for (col in c(time_column, weight_column)) {
    exists = col %in% names(x)
    if (!exists) stop("Column '", col, "' does not exist", call. = FALSE)
  }
  # Construct dodgr streetnet only with required columns.
  required = c("edge_id", "from_id", "from_lon", "from_lat", "to_id", "to_lon",
               "to_lat", "component")
  net = x[, required]
  net$d = x[[time_column]]
  net$d_weighted = x[[weight_column]]
  net = net[!is.na(net$d_weighted), ] # Remove edges with NA weight.
  # Get traveltimes
  get_streetnet_traveltimes(net, od_pairs, func = dodgr::dodgr_dists)
}

#' @name streetnet_traveltimes
#'
#' @export
streetnet_traveltimes.dodgr_streetnet = function(x, od_pairs, ...) {
  get_streetnet_traveltimes(x, od_pairs)
}

#' @name streetnet_traveltimes
#'
#' @export
streetnet_traveltimes.dodgr_streetnet_sc = function(x, od_pairs, ...) {
  get_streetnet_traveltimes(x, od_pairs)
}

get_streetnet_traveltimes = function(x, od_pairs, func = dodgr::dodgr_times) {
  # Match origins and destinations to nearest network nodes.
  V = dodgr::dodgr_vertices(x)
  O = od_pairs[, c("from_lon", "from_lat")]
  D = od_pairs[, c("to_lon", "to_lat")]
  from_idxs = V$id[dodgr::match_points_to_graph(V, O)]
  to_idxs = V$id[dodgr::match_points_to_graph(V, D)]
  # Compute cost matrix with all unique origins and destinations.
  # This is apparently faster than calculating costs pairwise.
  idxs = unique(c(from_idxs, to_idxs))
  costs = do.call(func, list(graph = x, from = idxs, to = idxs))
  # Match calculated costs back to original OD pairs.
  costs[match(to_idxs, idxs) + (match(from_idxs, idxs) - 1) * nrow(costs)]
}