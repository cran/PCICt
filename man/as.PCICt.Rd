\name{as.PCICt}
\alias{as.PCICt}
\alias{as.PCICt.default}
\alias{as.PCICt.POSIXlt}
\alias{as.PCICt.POSIXct}
\alias{.PCICt}
\alias{as.POSIXct.PCICt}
\alias{as.POSIXlt.PCICt}
\title{PCICt}
\description{
  These functions convert between PCICt objects and other types of data.
}
\usage{
as.PCICt(x, cal, ...)
.PCICt(x, cal)
as.PCICt.default(x, cal, ...)
as.PCICt.POSIXlt(x, cal, ...)
as.PCICt.POSIXct(x, cal, ...)
as.POSIXct.PCICt(x, tz="", ...)
as.POSIXlt.PCICt(x, tz="", ...)
}
\arguments{
  \item{x}{the input data.}
  \item{cal}{the calendar type.}
  \item{tz}{the time zone to put the data in.}
  \item{...}{any additional arguments passed on.}
}

\details{
  as.PCICt converts the x argument, where x is of type POSIXlt, POSIXct,
  or character, to a PCICt object with the given calendar type. Calendar
  types include 360 day calendars("360_day", "360"), 365 day calendars
  ("365_day", "365", "noleap"), and Gregorian calendars ("gregorian",
  "proleptic_gregorian"). When converting a character object, one can
  also specify the format with fmt=, which uses a format documented in
  the help page for strptime.

  .PCICt converts numeric objects into PCICt objects, using x as seconds
  since 1970-01-01 and applying the supplied calendar to the data.

  as.POSIXct.PCICt and as.POSIXlt.PCICt convert PCICt objects into
  POSIXct or POSIXlt objects, respectively. With POSIXct objects, this
  may result in apparent gaps in the timeseries, and the transformation
  will not be trivially reversible. See the example below for how to
  transition between PCICt and POSIXct.

  as.PCICt.default, as.PCICt.POSIXct, and as.PCICt.POSIXlt are helpers
  which are called by as.PCICt. Normally you will not need to call them
  directly.
}

\value{
  For as.PCICt and .PCICt, a PCICt object with the given calendar type.

  For as.POSIXct.PCICt and as.POSIXlt.PCICt, a POSIXct or POSIXlt
  object, respectively.
}

\examples{
## Convert these strings to PCICt objects.
x <- as.PCICt(c("1961-09-02", "1963-02-01"), cal="360_day")

## Convert these strings to POSIXlt objects, then coerce them into PCICt objects.
y <- as.POSIXlt(c("1961-09-02", "1963-02-01"))
x <- as.PCICt(y, cal="360_day")

## Convert a string to PCICt using a format string.
q <- as.PCICt("03292001", cal="365_day", format="\%m\%d\%Y")

## This will cause a parsing error.
\dontrun{bad.r <- as.PCICt("moo", cal="365_day")}

## Convert a POSIXct to PCICt 360-day
foo <- as.POSIXct("2011-04-01")
bar <- as.PCICt(as.character(foo), cal="360_day")

## Test whether the result is the same
baz <- as.PCICt("2011-04-01", cal="360_day")
bar == baz

## Convert a sequence of days plus an origin to PCICt (as seen in NetCDF files)
cal <- "365_day"
origin <- "1968-01-01"
seconds.per.day <- 86400
ts.dat.days <- 0:100
origin.pcict <- as.PCICt(origin, cal)
ts.dat.pcict <- origin.pcict + (ts.dat.days * seconds.per.day)
}

\seealso{
  \code{\link{as.POSIXlt}}, \code{\link{as.POSIXct}}, \code{\link{strptime}}
}

\keyword{ts}