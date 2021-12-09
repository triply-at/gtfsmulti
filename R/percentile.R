#' Reduce multiple travel times to a set of percentile values
#'
#' @param x Object to reduce. May be a travel time grid as object of class
#' [`stars`][stars::st_as_stars()] created by [create_traveltime_grid()] and
#' having multiple travel time values per spatial location stored along a third
#' dimension. May also be an object of class
#' [`data.table`][data.table::data.table()] created by [multinet_traveltimes()]
#' and having multiple travel time columns with names starting with
#' "travel_time".
#'
#' @param percentiles Percentile values that should be calculated.
#'
#' @return In case `x` is a travel time grid: the updated travel time grid as
#' object of class [`stars`][stars::st_as_stars()], with the third now having
#' one coordinate per calculated percentile value. The original third
#' dimension is removed. In case `x` is a data table: the updated table having
#' one travel time column per calculated percentile, all named "travel_time_p"
#' followed by the corresponding percentile value. The original travel time
#' columns are removed.
#'
#' @seealso [create_traveltime_grid()], [multinet_traveltimes()]
#'
#' @export
reduce_to_percentiles = function(x, percentiles = c(5, 25, 50, 75, 95)) {
  UseMethod("reduce_to_percentiles")
}

#' @name reduce_to_percentiles
#' @export
reduce_to_percentiles.data.table = function(x, percentiles = c(5, 25, 50, 75, 95)) {
  time_cols = grep("travel_time", names(x))
  times = x[, time_cols, with = FALSE]
  percs = t(apply(times, 1, percentile, levels = percentiles))
  percs = data.table::data.table(percs)
  names(percs) = paste0("travel_time_p", names(percs))
  cbind(x[, -time_cols, with = FALSE], percs)
}

#' @name reduce_to_percentiles
#' @export
reduce_to_percentiles.stars = function(x, percentiles = c(5, 25, 50, 75, 95)) {
  percs = stars::st_apply(x, c("x", "y"), percentile, levels = percentiles)
  aperm(percs, c(2, 3, 1))
}

percentile = function(x, levels) {
  stats::quantile(x, levels / 100, na.rm = TRUE)
}