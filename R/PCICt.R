origin.year <- 1970
origin.year.POSIXlt <- 1900

setOldClass("PCICt")

## TODO:
## - Implement axis functions (Axis.POSIXt/axis.POSIXct) so that plots will line up nicely
## - S4 class to avoid stripping of attributes?
## - Proleptic gregorian?

PCICt.get.months <- function(cal) {
  m.365 <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  m.360 <- c(30, 28, 31, 30, 30, 30, 30, 31, 30, 30, 30, 30)
  switch(cal, "365_day"=m.365, "360_day"=m.360, "365"=m.365, "360"=m.360)
}

.PCICt <- function(x, cal) {
  if(missing(cal)) stop("Can't create a PCICt with no calendar type")
  ## FIXME: Add check for sane calendar type.
  structure(x, cal=cal, months=PCICt.get.months(cal), class=c("PCICt", "POSIXct", "POSIXt"), dpy=switch(cal, "365_day"=365, "360_day"=360, "365"=365, "360"=360), tzone="GMT", units="secs")
}

range.PCICt <- function(..., na.rm=FALSE) {
  args <- list(...)
  stopifnot(length(unique(lapply(args, function(x) { attr(x, "cal") }))) == 1)
  args.flat <- unlist(args)
  ret <- c(min(args.flat, na.rm=na.rm), max(args.flat, na.rm=na.rm))
  ret <- copy.atts.PCICt(args[[1]], ret)
  class(ret) <- c("PCICt", "POSIXct", "POSIXt")
  return(ret)
}

c.PCICt <- function(..., recursive=FALSE) {
  ##stopifnot(length(unique(lapply(..., function(x) { attr(x, "cal") }))) == 1)
  cal <- attr(..1, "cal")
  .PCICt(c(unlist(lapply(list(...), unclass))), cal)
}

## FIXME: Broken for difftime objects
`+.PCICt` <- `-.PCICt` <- Ops.PCICt <- function (e1, e2){
  cal <- attr(e1, "cal")
  if(inherits(e2, "POSIXt")) {
    stopifnot(inherits(e2, "PCICt") & attr(e1, "cal") == attr(e2, "cal"))
    class(e2) <- c("POSIXct", "POSIXt")
  }
  class(e1) <- c("POSIXct", "POSIXt")
  x <- NextMethod()
  if(inherits(x, "POSIXct")) {
    x <- copy.atts.PCICt(e1, x)
    class(x) <- c("PCICt", "POSIXct", "POSIXt")
  }
  return(x)
}

rep.PCICt <- function(x, ...) {
  y <- NextMethod()
  .PCICt(y, cal=attr(x, "cal"))
}

mean.PCICt <- function(x, ...) {
  .PCICt(mean(unclass(x), ...), attr(x, "cal"))
}

min.PCICt <- function(x, ...) {
  res <- NextMethod()
  return(copy.atts.PCICt(x, res))
}

max.PCICt <- function(x, ...) {
  res <- NextMethod()
  return(copy.atts.PCICt(x, res))
}

seq.PCICt <- function(from, to, by, length.out = NULL, along.with = NULL, ...) {
  stopifnot(attr(from, "cal") == attr(to, "cal"))
  class(from) <- class(to) <- c("POSIXct", "POSIXt")
  ret <- NextMethod()
  class(ret) <- NULL
  return(.PCICt(ret, cal=attr(from, "cal")))
}

trunc.PCICt <- function(x, units = c("secs", "mins", "hours", "days"), ...) {
  class(x) <- c("POSIXct", "POSIXt")
  ret <- as.POSIXct(NextMethod())
  class(ret) <- NULL
  return(.PCICt(ret, cal=attr(x, "cal")))
}

copy.atts.PCICt <- function(from, to) {
  return(structure(to, cal=attr(from, "cal"), months=attr(from, "months"), class=class(from), dpy=attr(from, "dpy"), tzone=attr(from, "tzone"), units=attr(from, "units")))
}

`[.PCICt` <- function(x, ...) {
  cl <- class(x)
  class(x) <- NULL
  val <- NextMethod("[")
  val <- copy.atts.PCICt(x, val)
  class(val) <- cl
  val
}

`[<-.PCICt` <- function (x, ..., value) {
  if (!as.logical(length(value)))
    return(x)
  origin <- .PCICt(0, attr(x, "cal"))
  stopifnot(class(value) == class(x) & attr(x, "cal") == attr(value, "cal"))
  cl <- oldClass(x)
  class(x) <- class(value) <- c("POSIXct", "POSIXt")
  x <- NextMethod("[<-")
  x <- copy.atts.PCICt(value, x)
  class(x) <- cl
  x
}

as.PCICt <- function(x, cal, ...) {
  UseMethod("as.PCICt")
}

as.PCICt.default <- function(x, cal, ...) {
  tz <- "GMT"
  if (inherits(x, "PCICt"))
    return(x)
  if (is.character(x) || is.factor(x)) {
    ## This is a specific hack to get around 30-days-per-month 360-day calendars.
    x <- as.character(x)
    if(cal == "360" || cal == "360_day")
      x <- gsub("([0-9]+)-02-29", "\\1-03-01", gsub("([0-9]+)-02-30", "\\1-03-02", x))

    return(as.PCICt(as.POSIXlt(x, tz, ...), cal, ...))
  }
  if (is.logical(x) && all(is.na(x)))
    return(.PCICt(as.numeric(x), cal))
  stop(gettextf("do not know how to convert '%s' to class \"PCICt\"", deparse(substitute(x))))
}

as.PCICt.POSIXlt <- function(x, cal, ...) {
  tz <- "GMT"
  year.length <- switch(cal, "360_day"=360, "365_day"=365, "365"=365, "360"=360, "noleap"=365, "gregorian"=NULL, "proleptic_gregorian"=NULL)
  ## FIXME: Add check for sane calendar type.
  ## Warning about proleptic gregorian
  if(cal == "proleptic_gregorian") warning("Proleptic gregorian is implemented as gregorian, which is off by 3+ days. Make sure you know what you are doing here.")
  if(is.null(year.length)) {
    d <- as.POSIXct(x, tz="GMT")
    class(d) <- NULL
    return(.PCICt(d, "gregorian"))
  } else {
    months <- PCICt.get.months(cal)
    months.off <- cumsum(c(0, months[1:(length(months) - 1)]))
    seconds.per.day <- 86400
    seconds.per.hour <- 3600
    return(.PCICt((x$year + origin.year.POSIXlt - origin.year) * year.length * seconds.per.day +
                  months.off[x$mon + 1] * seconds.per.day + (x$mday - 1) * seconds.per.day + x$hour * seconds.per.hour + x$min * 60 + x$sec, cal=cal))
  }
}

as.PCICt.POSIXct <- function(x, cal, ...) {
  as.PCICt.POSIXlt(as.POSIXlt(x), cal, ...)
}

as.POSIXlt.PCICt <- function(x, tz="", ...) {
  seconds.per.day <- 86400
  seconds.per.hour <- 3600

  tzone <- attr(x, "tzone")
  if (length(tz) == 0 && !is.null(tzone))
    tz <- tzone[1L]

  if(is.null(attr(x, "months"))) {
    class(x) <- c("POSIXct", "POSIXt")
    return(as.POSIXlt(x))
  } else {
    months <- attr(x, "months")
    months.off <- cumsum(c(0, months[1:(length(months) - 1)]))
    months.idx <- unlist(lapply(1:12, function(x) { rep(x, months[x]) } ))

    days.per.year <- attr(x, "dpy")
    remainder <- as.numeric(x) %% (days.per.year * seconds.per.day)
    remainder[remainder < 0] <- days.per.year * seconds.per.day - remainder[remainder < 0]

    year <- floor(as.numeric(x) / (days.per.year * seconds.per.day)) + origin.year
    yday <- floor(remainder / seconds.per.day) + 1
    month <- months.idx[yday]
    day <- yday - months.off[month]

    ## Need to compute wday
    wday <- (as.numeric(x) / 86400) %% 7
    hms.remainder <- remainder %% seconds.per.day
    hour <- floor(hms.remainder / seconds.per.hour)
    minute <- floor((hms.remainder %% seconds.per.hour) / 60)
    second <- hms.remainder %% 60
    return(.POSIXlt(list(sec=second, min=minute, hour=hour, mday=day, mon=month - 1, year=year - origin.year.POSIXlt, wday=wday, yday=yday - 1, isdst=0), tz))
  }
}

as.POSIXct.PCICt <- function(x, tz="", ...) {
  return(as.POSIXct(as.POSIXlt(x, tz)))
}

cut.PCICt <- function (x, breaks, labels = NULL, start.on.monday = TRUE, right = FALSE, ...) {
  cut.POSIXt(as.POSIXct(x), breaks, labels, start.on.monday, right, ...)
}

diff.PCICt <- function (x, lag = 1L, differences = 1L, ...) {
  class(x) <- c("POSIXct", "POSIXt")
  diff(x, lag, differences, ...)
}

is.numeric.PCICt <- function(x) FALSE

julian.PCICt <- function (x, origin=NULL, ...) {
  if(is.null(origin))
    origin <- "1970-01-01"
  else
    stopifnot(attr(x, "cal") == attr(origin, "cal"))

  origin <- as.PCICt(origin, cal=attr(x, "cal"))
  class(x) <- class(origin) <- c("POSIXct", "POSIXt")
  if (length(origin) != 1L)
    stop("'origin' must be of length one")

  res <- difftime(x, origin, units = "days")
  structure(res, origin = origin)
}
