# Creating, storing and analyzing multi-modal transportation networks with GTFS-Multi feeds

The `gtfsmulti` R package implements a very experimental approach to creating, storing and analyzing multi-modal transportation networks.

The broad idea is as follows. We first create a fine regular grid over an area of interest. Each grid point forms a possible origin and destination of a multi-modal trip. Then we pre-calculate street network travel times for different legs of a multi-modal trip. That is: from each grid point to nearby transit stops (*access trips*), from each transit stop to nearby grid points (*egress trips*), between nearby transit stops (*transfer trips*) and between nearby grid points (*direct trips*). Multiple transport modes (e.g. walking, biking) can be included in these calculations, and edge weights can be flexibly defined for each of the modes (i.e. not limited to only optimizing travel time).

The grid itself can be added to a [GTFS feed](https://developers.google.com/transit/gtfs) feed as a table in similar format as a standard [stops table](https://developers.google.com/transit/gtfs/reference#stopstxt), while the access, egress, transfer and direct trips are all added to the GTFS feed as separate tables in similar format as a standard [transfer table](https://developers.google.com/transit/gtfs/reference#transferstxt). All together this forms a *GTFS-Multi feed*.

When finding multi-modal routes with such a GTFS-Multi feed, all street network travel times can be integrated in the timetable of the transit network, and *only* a timetable-based routing algorithm is enough to solve the multi-modal routing task.

The ideas and applications are explained in much more detail in two articles (since they are quite large they are currently not included as regular package vignettes):

- [Intro](vignettes/intro.Rmd): This article explains how the ideas behind the approach developed and how you can construct a GTFS-Multi feed.
- [Route](vignettes/route.Rmd): This article shows how to calculate multi-modal routes with a GTFS-Multi feed.

## Installation

At this moment the package can only be installed from source. Using `remotes` is the easiest way to do so:

```r
remotes::install_github("triply-at/gtfsmulti")
```

## Contribution

The ideas are still very experimental. If you like them, you are very welcome to contribute in some way. For example, contributing code in the form of pull requests, but also reporting bug, requesting features, asking questions or sharing ideas of any kind. For writing code, I try to use the [tidyverse styleguide](https://style.tidyverse.org/), although I tend to prefer `=` instead of `<-` for assignments.

## Acknowledgements

A lot of the conceptual ideas behind the approach are inspired by how Conveyal is approaching multi-modal routing with their [R5 routing engine](https://github.com/conveyal/r5). For basically all the hard work in the background, however, `gtfsmulti` borrows the horsepower of R packages [dodgr](https://github.com/ATFutures/dodgr) and [gtfsrouter](https://github.com/ATFutures/gtfs-router).

## Copyright

Copyright (C) 2021 triply GmbH

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>
