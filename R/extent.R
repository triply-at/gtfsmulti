#' Create a rectangular spatial extent for the analysis
#'
#' @param bounds Coordinates marking the boundaries of the extent, as an
#' ordered vector in the form `c(xmin, ymin, xmax, ymax)`.
#'
#' @param input_crs Coordinate reference system in which the coordinate values
#' of `bounds` are expressed. Can be anything understood by [sf::st_crs()]. By
#' default equal to 4326, i.e. longitude and latitude coordinates with WGS84 as
#' geodetic datum.
#'
#' @param output_crs Coordinate reference system to create the extent in. Can
#' be anything understood by [sf::st_crs()]. By default equal to `input_crs`.
#'
#' @return The rectangular extent as a single `POLYGON` geometry in an object
#' of class [`sfc`][sf::st_sfc()].
#'
#' @export
create_extent = function(bounds, input_crs = 4326, output_crs = input_crs) {
  names(bounds) = c("xmin", "ymin", "xmax", "ymax")
  class(bounds) = "bbox"
  extent = sf::st_as_sfc(bounds)
  sf::st_crs(extent) = input_crs
  sf::st_transform(extent, output_crs)
}