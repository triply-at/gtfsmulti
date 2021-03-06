% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/traveltimes.R
\name{multinet_traveltimes}
\alias{multinet_traveltimes}
\title{Calculate travel times from a single origin to all destinations}
\usage{
multinet_traveltimes(
  x,
  from,
  start_time_limits,
  access_modes = NULL,
  direct_modes = NULL,
  egress_modes = NULL,
  transfer_modes = NULL,
  access_limit = Inf,
  direct_limit = Inf,
  egress_limit = Inf,
  transfer_limit = Inf,
  minimise_transfers = FALSE,
  include_grid_access = TRUE,
  iterate = FALSE,
  quiet = FALSE,
  access_frequency = 60,
  grid_access_speed = 5/3.6,
  router_timeout = 60 * 60,
  day = NULL,
  date = NULL
)
}
\arguments{
\item{x}{An object of class \code{multinet}, which is a list of multiple
\code{\link[data.table:data.table]{data.table}} objects corresponding to the
different dataset files of a GTFS-Multi feed.}

\item{from}{Origin of the routes. May be the index of a grid point. In that
case it should correspond to a value in the \emph{stop_id} column of the grid
table in x. May also be any location inside the extent of the grid. Such a
location may be given as a single \code{POINT} geometry inside an object of
either class \code{\link[sf:sf]{sf}} or \code{\link[sf:sfc]{sfc}}. May also be given as
a numeric vector containing the two coordinate values. In any case,
coordinates are expected to be expressed as longitude and latitude values
with WGS84 as geodetic datum.}

\item{start_time_limits}{A vector of two integer values denoting the start
and end of the time interval in which trips may be started. Values should
be given in seconds since midnight.}

\item{access_modes}{Vector of transport modes to consider for access trips,
i.e. trips from grid points towards transit stop locations.
Each of these transport modes should have their access travel time for each
access trip specified in the access table of \code{x}, in a column named
\emph{transfer_time_{<}mode{>}}. If \code{NULL}, all modes that have such a column
in the access table are considered. If \code{NA}, access trips are not included
at all. Defaults to \code{NULL}.}

\item{direct_modes}{Vector of transport modes to consider for direct trips,
i.e. trips between grid points.
Each of these transport modes should have their direct travel time for each
direct trip specified in the direct table of \code{x}, in a column named
\emph{transfer_time_{<}mode{>}}. If \code{NULL}, all modes that have such a column
in the direct table are considered. If \code{NA}, direct trips are not included
at all. Defaults to \code{NULL}.}

\item{egress_modes}{Vector of transport modes to consider for egress trips,
i.e. trips from transit stop locations towards grid points.
Each of these transport modes should have their egress travel time for each
egress trip specified in the egress table of \code{x}, in a column named
\emph{transfer_time_{<}mode{>}}. If \code{NULL}, all modes that have such a column
in the egress table are considered. If \code{NA}, egress trips are not included
at all. Defaults to \code{NULL}.}

\item{transfer_modes}{Vector of transport modes to consider for true transit
transfer trips, i.e. trips between transit stop locations.
Each of these transport modes should have their transfer travel time for
each transfer trip specified in the transfer table of \code{x}, in a column named
\emph{transfer_time_{mode}}. If \code{NULL}, all modes that have such a column
in the transfer table are considered. If \code{NA}, transfer trips are not
included at all. Defaults to \code{NULL}.}

\item{access_limit}{Maximum time in seconds for access trips. Any access
trip in the access table of \code{x} with a travel time higher than this limit
will not be considered in the travel time calculation. Defaults to \code{Inf},
meaning that all specified access trips will be considered.}

\item{direct_limit}{Maximum time in seconds for direct trips. Any direct
trip in the direct table of \code{x} with a travel time higher than this limit
will not be considered in the travel time calculation. Defaults to \code{Inf},
meaning that all specified direct trips will be considered.}

\item{egress_limit}{Maximum time in seconds for egress trips. Any egress
trip in the egress table of \code{x} with a travel time higher than this limit
will not be considered in the travel time calculation. Defaults to \code{Inf},
meaning that all specified egress trips will be considered.}

\item{transfer_limit}{Maximum time in seconds for transfer trips. Any
transfer trip in the transfer table of \code{x} with a travel time higher than
this limit will not be considered in the travel time calculation. Defaults
to \code{Inf}, meaning that all specified transfer trips will be considered.}

\item{minimise_transfers}{Boolean. If \code{TRUE}, travel times are calculated
for routes with minimal transfer connections between the origin and each
destination, even if those are slower than travel times of alternative
routes with more transfers. Defaults to \code{FALSE}.}

\item{include_grid_access}{Boolean. If \code{TRUE}, the straight-line distance
between the origin location and its nearest grid point is assumed to be
travelled at the speed given by the \code{grid_access_speed} parameter. The
resulting travel time is added to the calculated travel times originating
from the nearest grid point. If \code{FALSE}, this "pre-leg" of the route is
ignored. Defaults to \code{TRUE}.}

\item{iterate}{Boolean. If \code{TRUE}, a separate route search is performed for
every possible departure time of an access trip within the time interval.
For each of these departure times, the shortest travel times to all
destinations are returned in a separate column, with the restriction that
trips need to start \emph{exactly} at this time, instead of at anytime within a
time interval. This is similar to the
\href{https://docs.conveyal.com/learn-more/faq#how-does-conveyal-analysis-calculate-travel-times}{R5 approach}
for multi-modal routing. With the output, it is possible to create a
distribution of travel times, and thus evaluate the quality of multi-modal
connections not only by the shortest possible travel time. The frequency of
departure times can be tuned by setting the \code{access_frequency} parameter.}

\item{quiet}{Boolean. If \code{FALSE}, display progress information on screen.
Defaults to \code{FALSE}.}

\item{access_frequency}{At what frequency should access trips be allowed to
start? Value should be given in seconds. Smaller values will provide more
accurate results, since access trips can in reality be started at anytime,
but also increase processing time. Defaults to 60, i.e. every minute.}

\item{grid_access_speed}{Speed at which the straight-line distance between
the origin location and its nearest grid point can be travelled. Only
considered when \code{include_grid_access = TRUE}. Value should be given in m/s.
Defaults to 5 / 3.6, i.e. 5 km/h.}

\item{router_timeout}{Cumulative travel time in seconds after which the
routing algorithm will exit the search for a route. Hence, if during the
search for a route between the selected origin grid point and a transit stop
(see step 2 in Details) the cumulative travel time exceeds this value, the
algorithm will stop looking for a connection and marks the particular
transit stop as unreachable. Defaults to 60 * 60, i.e. one hour.}

\item{day}{Day of the week for which the timetable should be created. May
be given as unambiguous string (e.g. "tu" and "th" for Tuesday and
Thursday), or an integer between 1 = Sunday and 7 = Saturday. If \code{NULL},
the current day will be used, except when \code{date} is not set to \code{NULL}.}

\item{date}{Date for which the timetable should be created. Should be given
as a single 8-digit integer representing "yyyymmdd". This parameter is meant
to be used with feeds that contain a
\href{https://developers.google.com/transit/gtfs/reference#calendar_datestxt}{calendar_dates}
table, either to specify service availability separately for each date, or
to list exceptions to the regular service availability as specified in the
\href{https://developers.google.com/transit/gtfs/reference#calendartxt}{calendar}
table. Ignored when \code{day} is not set to \code{NULL}.}
}
\value{
The grid table of the GTFS-Multi feed with an additonal column
containing for each grid point the calculated travel time from the given
origin location to that grid point. Grid points that could not be reached
get a travel time value of \code{NA}. If \code{iterate = TRUE}, there will be multiple
travel time columns instead of only one. Each column then represents the
travel time for a fixed departure time.
}
\description{
This function calculates travel times in a multi-modal network from a
specified location anywhere within the grid of a GTFS-Multi feed to all grid
points of that grid. The travel times are the shortest multi-modal
travel times within the bounds of acceptable departure times. Travel
times may also be single-modal in case it is faster to travel directly
from the origin to the destination, i.e. without using transit at all.
}
\details{
Travel times are calculated as with the following steps:
\enumerate{
\item The nearest grid point to \code{origin} is found. An imaginary pre-leg of the
trip accounts to some extent for the distance between these two points. This
distance (as-the-crow-flies) is assumed to be travelled at a fixed speed,
specified in the \code{grid_access_speed} parameter. By default its value is set
to a walking-like speed of 5 / 3.6 m/s, i.e. 5 km/h. When setting
\code{include_grid_access = FALSE} the distance between the actual origin
location and its nearest grid point is not accounted for and the trip is
assumed to effectively start at the nearest grid point. Keep again in mind
that no matter what approach you choose, results get less accurate with a
coarser grid.
\item For each transit stop listed in the \emph{stops table} of \code{x}, the shortest
multi-modal travel time from the selected grid point to that stop is
calculated. This is done by a purely timetable-based routing algorithm.
Access trips listed in the \emph{access table} of \code{x} that originate from the
selected grid point are included in the transit timetable by modelling them
as direct transit trips from the selected grid point towards reachable
transit stops, operating on a frequency-based schedule. The frequency of
these trips is defined by the \code{access_frequency} parameter, which by default
is set to 60 seconds, i.e. one departure every minute. The length of such an
access trip is the minimum travel time for a given access connection among
all different modes that are present in the access table. The modes can be
subsetted by setting the \code{access_modes} parameter, e.g.
\code{access_modes = "walk"} to include only access times by foot. Possible trips
itself may be filtered as well, by setting a maximum allowed travel time
through the \code{access_limit} parameter. During the transit leg of the trip it
is possible to transfer from one vehicle to another using a transfer
connection listed in the \emph{transfers table} of \code{x}. The travel time of such
a transfer connection is the minimum travel time among all different modes
that are present in the transfer table. The modes can be subsetted by
setting the \code{transfer_modes} parameter, e.g. \code{transfer_modes = "walk"} to
include only transfer times by foot. Possible trips itself may be filtered
as well, by setting a maximum allowed travel time through the
\code{transfer_limit} parameter. For efficiency reasons, the routing algorithm in
this phase has an upper bound defined, meaning that if the cumulative travel
time of a trip from the selected grid point exceeds this upper bound before
the transit stop is reached, the search for a route to that stop is
terminated and the transit stop is marked as unreachable. This upper bound
can be defined through the \code{router_timeout} parameter and defaults to
60 * 60 seconds, i.e. 1 hour.
\item For each destination grid point (i.e. all grid points except the one
selected as origin) listed in the \emph{grid table} of \code{x}, multi-modal travel
times from the selected origin grid point to that destination grid point are
calculated. This is done by extracting travel times of egress trips that
lead to the destination grid point from the \emph{egress table} of \code{x}, and
adding those to the travel times from the origin grid point towards transit
stops as calculated in the previous step. For a destination grid point
this will often result in a set of multiple travel times (since there may be
multiple egress trips leading to the same grid point), of which only the
shortest one is preserved. The travel time of an egress connection is the
minimum travel time among all different modes that are present in the egress
table. The modes can be subsetted by setting the \code{egress_modes} parameter,
e.g. \code{egress_modes = "walk"} to include only egress times by foot. Possible
trips itself may be filtered as well, by setting a maximum allowed travel
time through the \code{egress_limit} parameter.
\item For each destination grid point listed in the \emph{grid table} of \code{x}, the
shortest multi-modal travel time (as calculated in the step above) is
compared to the travel time of a single-modal direct trip listed in the
\emph{direct table} of \code{x}. The travel time of such a direct connection is the
minimum travel time among all different modes that are present in the
direct table. The modes can be subsetted by setting the \code{direct_modes}
parameter, e.g. \code{direct_modes = "bike"} to include only direct times by
bicycle. Possible trips itself may be filtered as well, by setting a
maximum allowed travel time through the \code{direct_limit} parameter. The
minimum between the multi-modal travel times and the single-modal travel
time is the returned shortest travel time from the selected origin grid
point to the destination grid point.
}
}
\seealso{
\code{\link[gtfsrouter:gtfs_traveltimes]{gtfsrouter::gtfs_traveltimes()}}
}
