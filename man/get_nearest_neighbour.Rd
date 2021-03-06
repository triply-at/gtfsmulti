% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_nearest_neighbour}
\alias{get_nearest_neighbour}
\title{Find the nearest neighbour to a given origin}
\usage{
get_nearest_neighbour(origin, candidates)
}
\arguments{
\item{origin}{A numeric vector of length two containing respectively
the longitude and latitude coordinate of the origin location.}

\item{candidates}{A \code{\link[=data.frame]{data.frame}} or similar representing the
set of possible neighbour locations to choose from. Should have only three
columns, with the first column containing unique indices for each location,
and the second and third column containing respectively the longitude and
latitude coordinates of each location.}
}
\value{
A list of length two with element \emph{idx} containing the index of the
nearest neighbour and element \emph{dist} containing the distance to it.
}
\description{
Find the nearest neighbour to a given origin
}
\note{
Coordinates should be expressed in coordinate reference system
\href{https://epsg.io/4326}{EPSG:4326}. The function assumes this to be true, and
does not check for it.
}
