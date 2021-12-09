#' GTFS-Multi feed for the city centre of Tampere, Finland
#'
#' @format An object of class `multinet`, which is a list of multiple
#' [data.table::data.table()] objects corresponding to the different dataset
#' files of a GTFS-Multi feed.
#'
#' @source Original GTFS feed downloaded from
#' [OpenMobilityData](https://transitfeeds.com/p/tampereen-joukkoliikenne/727).
#'
#' @note To reduce its size the original GTFS feed was first filtered. As a
#' spatial filter: only stops with coordinates inside spatial bounding box
#' `23.556649, 61.44593, 23.892189, 61.557975` and associated trips where kept.
#' As a temporal filter: only stop time entries with departure time inside
#' time interval `06:00, 12:00` and associated trips where kept. Tables
#' *shapes.txt*, *fare_attributes.txt* and *fare_rules.txt* where removed.
#'
#' @seealso The introductory vignette shows in detail how this feed was
#' created.
"tampere"