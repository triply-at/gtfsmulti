% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tampere.R
\docType{data}
\name{tampere}
\alias{tampere}
\title{GTFS-Multi feed for the city centre of Tampere, Finland}
\format{
An object of class \code{multinet}, which is a list of multiple
\code{\link[data.table:data.table]{data.table::data.table()}} objects corresponding to the different dataset
files of a GTFS-Multi feed.
}
\source{
Original GTFS feed downloaded from
\href{https://transitfeeds.com/p/tampereen-joukkoliikenne/727}{OpenMobilityData}.
}
\usage{
tampere
}
\description{
GTFS-Multi feed for the city centre of Tampere, Finland
}
\note{
To reduce its size the original GTFS feed was first filtered. As a
spatial filter: only stops with coordinates inside spatial bounding box
\verb{23.556649, 61.44593, 23.892189, 61.557975} and associated trips where kept.
As a temporal filter: only stop time entries with departure time inside
time interval \verb{06:00, 12:00} and associated trips where kept. Tables
\emph{shapes.txt}, \emph{fare_attributes.txt} and \emph{fare_rules.txt} where removed.
}
\seealso{
The introductory vignette shows in detail how this feed was
created.
}
\keyword{datasets}
