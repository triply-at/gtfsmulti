#' Import a grid from a GeoTIFF file
#'
#' @param path Path to a `.geotiff` or `.tiff` file containing the grid.
#'
#' @param quiet Boolean. If `FALSE`, display progress information on screen.
#' Defaults to `FALSE`.
#'
#' @return The grid as an object of class [stars][stars::st_as_stars()].
#'
#' @seealso [create_reference_grid()], [create_traveltime_grid()]
#'
#' @export
import_grid = function(path, quiet = FALSE) {
  stars::read_stars(path, driver = "GTiff", quiet = quiet)
}

#' Write a grid to disk
#'
#' @param x The grid as object of class [stars][stars::st_as_stars()].
#'
#' @param path Path to a `.geotiff` or `tiff` file where the grid
#' should be written to.
#'
#' @param layer Name or index of the attribute to be written. Defaults to 1,
#' meaning the first attribute is written.
#'
#' @param quiet Boolean. If `FALSE`, display progress information on screen.
#' Defaults to `FALSE`.
#'
#' @seealso [create_reference_grid()], [create_traveltime_grid()]
#'
#' @export
export_grid = function(x, path, layer = 1, quiet = FALSE) {
  stars::write_stars(x, path, layer = layer, driver = "GTiff", quiet = quiet)
}

#' Create a reference grid over the spatial extent of the analysis
#'
#' @param extent The spatial extent of the analysis as a single `POLYGON`
#' geometry in either object of class [sf::sf()] or class [sfc][sf::st_sfc()].
#'
#' @param cellsize Cellsize in units of `output_crs` (usually meters). Cells
#' are always squares, i.e. the width and height of the cells are both equal
#' to `cellsize`.
#'
#' @param output_crs CRS to create the grid in. Can be anything understood by
#' [sf::st_crs()]. By default equal to the CRS of `extent`.
#'
#' @return The reference grid as an object of class
#' [stars][stars::st_as_stars()].
#'
#' @seealso [create_extent()], [create_grid_table()]
#'
#' @export
create_reference_grid = function(extent, cellsize, output_crs = sf::st_crs(extent)) {
  box = sf::st_bbox(sf::st_transform(extent, output_crs))
  stars::st_as_stars(box, dx = cellsize, dy = cellsize, values = 1)
}

#' Mask the reference grid by a spatial vector layer
#'
#' @param grid The reference grid as object of class
#' [stars][stars::st_as_stars()].
#'
#' @param mask Object of class [sf::sf()] or [sfc][sf::st_sfc()] to use as
#' mask.
#'
#' @param predicate Geometric binary predicate to use for masking. See
#' [sf::geos_binary_pred].
#'
#' @return The updated grid as object of class [stars][stars::st_as_stars()].
#' The updated grid has the same dimensions as the original grid, but pixel
#' values are set to `NA` if the specified spatial relation (as described by
#' `predicate`) between that pixel and the mask is evaluated to `FALSE`.
#'
#' @export
mask_grid = function(grid, mask, predicate = sf::st_intersects) {
  if (!(sf::st_crs(grid) == sf::st_crs(mask))) {
    mask = sf::st_transform(mask, sf::st_crs(grid))
  }
  cells = sf::st_geometry(stars::st_xy2sfc(grid, as_points = FALSE))
  cover = lengths(do.call(predicate, list(cells, mask))) == 0
  for (i in seq_along(grid)) grid[[i]][cover] = NA
  #grid[["values"]] = matrix(as.integer(keep), nrow(grid), ncol(grid))
  grid
}

#' Write travel times as attributes to a grid
#'
#' @param x The object returned by [multinet_traveltimes()], i.e. the grid
#' table of a GTFS-Multi feed as object of class [data.table::data.table()]
#' with one or multiple columns containing calculated travel time values.
#' These columns should have names that start with "travel_time".
#'
#' @param grid Reference grid of `x` as object of class
#' [stars][stars::st_as_stars()].
#'
#' @param dimname If `x` contains multiple columns containing travel time
#' values, they will be stored along an additional dimension. How should this
#' dimension be called? Defaults to "time", assuming that the different columns
#' refer to different departure times.
#'
#' @return The travel time grid as an object of class
#' [stars][stars::st_as_stars()]. This grid has the same spatial dimensions as
#' `x` but contains only travel time values for each pixel.
#'
#' @seealso [multinet_traveltimes()], [create_reference_grid()]
#'
#' @export
create_traveltime_grid = function(x, grid, dimname = "time") {
  # The function returns a new grid with only travel times.
  # This new grid has the same spatial dimensions as the input grid.
  # Therefore the empty input grid forms a template for the travel time grid.
  for (i in seq_along(grid)) grid[[i]] = NULL
  # Construct a template attribute table with one row per pixel in the grid.
  attr = data.table::data.table(
    row_id = rep(c(1:nrow(grid)), ncol(grid)),
    col_id = rep(c(1:ncol(grid)), rep(nrow(grid), ncol(grid)))
  )
  # Match time values in x to their corresponding row in the attribute table.
  # Base this on the 'row_id' and 'col_id' columns present in x.
  time_cols = names(x)[grep("travel_time", names(x))]
  times = x[, c("row_id", "col_id", time_cols), with = FALSE]
  times = merge(attr, times, by = c("row_id", "col_id"), all.x = TRUE)
  # Order correctly.
  # I.e. first all rows in column 1, then all rows in column 2, etc.
  times = times[order(times$col_id, times$row_id), ]
  # Subset to only keep time value columns.
  times = times[, -c("row_id", "col_id")]
  # First check if there are multiple travel time colums.
  # If true: create additional dimension to store multiple values per pixel.
  # If false: just add a single attribute along existing spatial dimensions.
  if (ncol(times) > 1) {
    # Add each column as attribute to the grid.
    for (col in names(times)) grid[[col]] = times[[col]]
    # Convert attributes to a dimension.
    grid = merge(grid)
    # Set name and coordinate labels of the new dimension.
    labels = as.integer(sapply(strsplit(names(times), "_"), utils::tail, 1))
    grid = stars::st_set_dimensions(grid, 3, values = labels, names = dimname)
  } else {
    # Add column as attribute to the grid.
    grid[[1]] = times[[1]]
  }
  names(grid) = "travel_time" # Name of the attribute.
  grid
}