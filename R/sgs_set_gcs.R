#################################################################################
#                                                                               #
# Adapted from 'Geo-Coordinates-OSGB-2.20', a Perl routine by Toby Thurston:    #
# https://metacpan.org/release/Geo-Coordinates-OSGB                             #
#                                                                               #
#################################################################################


#' @encoding UTF-8
#' @title Set GCS of a set of points
#'
#' @description
#' Changes the geodetic coordinate system of a set of points using a single Helmert
#' transformation.
#'
#' @name sgs_set_gcs
#' @usage sgs_set_gcs(x, to = NULL)
#' @param x A \code{sgs_points} object describing a set of points in a geodetic
#' coordinate system.
#' @param to Specifies the EPSG code to convert the coordinates to. Currently it
#' can take any of the following values: \code{4258}, \code{4326} or \code{4277}.
#' @details
#' Changes the geodetic coordinate system of a set of points. Note that precision
#' of various datums will vary, and (original)WGS-84 is not defined to be
#' accurate to better than ±1 metre. Most transformations shouldn't be assumed to be
#' accurate to better than a meter; between OSGB36 and WGS84 somewhat less - the
#' lost of accuracy can be up to ±5m when using single Helmert transformations).
#'
#' Input points with a projected coordinate system (eg 27700 or 3857) are not allowed.
#'
#' \strong{Warning}
#' This function is mainly for internal use of the program. Since it relies on a
#' single Helmert transformation it is not recommended to call it directly. Use any
#' other of the transformation functions available (\link{sgs}).
#' @return
#' An object of class 'sgs_points'.
#' @seealso \code{\link{sgs_points}}, \code{\link{sgs_transform}}.
#' @examples
#' lat <- c(55.86424, 55.95325)
#' lon <- c(-4.25181,-3.18827)
#' p <- sgs_points(list(latitude=lat, longitude=lon), epsg=4326)
#' p2 <- sgs_set_gcs(p, to=4277) #warning: a single Helmert transformation was used
#' # if higher precision is required to transform between OSGB36 latlon and ETRS89/WGS84 latlon
#' # then use the OSTN15 transformation (will be slower):
#' # Transform from WGS84 latlon coordinates to EPSG:4277 using OSTN15
#' p2 <- sgs_transform(p, to=4277)
#' @export
sgs_set_gcs <- function(x, to=NULL) UseMethod("sgs_set_gcs")

#' @export
sgs_set_gcs.sgs_points <- function (x, to=NULL) {

  if (x$epsg == to) { return (x) }

  # Check x is a GCS (not projected)
  coord.system <- epsgs[epsgs[, "epsg"]==x$epsg, "type"]
  if (coord.system != "GCS") stop("This routine only only accepts Geodetic Coordinate Systems")

  core.cols <- sgs_points.core
  #  core.cols <- sgs_points.core[!sgs_points.core %in% c("easting", "northing")]

  additional.elements <- !names(x) %in% core.cols
  num.elements <- sum(additional.elements, na.rm=TRUE)

  to.datum <- epsgs[epsgs[, "epsg"]==to, "datum"]
  transform <- NULL

  if (x$datum == "WGS84") {
    # converting from WGS84
    transform <- latlon.datum[latlon.datum$datum==to.datum, 3:9]
  }
  if (to.datum == "WGS84") {
    # converting to WGS84; use inverse transform
    transform <- -latlon.datum[latlon.datum$datum==x$datum, 3:9]
  }
  if (is.null(transform)) {
    # neither x$datum nor to.datum are WGS84 pipe throuhgh WGS84 first
    x <- sgs_set_gcs(x, to=4326)
    transform <- latlon.datum[latlon.datum$datum==to.datum, 3:9]
  }

  old.cartesian <- latlon_to_cartesian(x)                     # convert polar to cartesian...
  new.cartesian <- apply_transform(old.cartesian, transform)  # ...apply transform...
  new.latlon <- cartesian_to_latlon(new.cartesian, to)        # ...and convert cartesian to polar

  # return sgs_points object
  if (num.elements > 0) new.latlon <- c(x[additional.elements], new.latlon)
  sgs_points(new.latlon, epsg=to)

}


# Converts from geodetic latitude/longitude coordinates to geocentric
# cartesian (x/y/z) coordinates.
latlon_to_cartesian <- function(points) {

  datum <- points$datum
  ellipsoid <- latlon.datum[latlon.datum$datum==datum, "ellipsoid"]

  phi <- points$latitude / 57.29577951308232087679815481410517
  lambda <- points$longitude / 57.29577951308232087679815481410517
  h = 0L # height above ellipsoid - not currently used
  a <- latlon.ellipsoid[latlon.ellipsoid$ellipsoid==ellipsoid, "a"]
  e2 <- latlon.ellipsoid[latlon.ellipsoid$ellipsoid==ellipsoid, "e2"]

  sin.phi <- sin(phi)
  cos.phi <- cos(phi)
  sin.lambda <- sin(lambda)
  cos.lambda <- cos(lambda)

  nu <- a / sqrt(1L - e2 * sin.phi * sin.phi)   # radius of curvature in prime vertical

  x <- (nu + h) * cos.phi * cos.lambda
  y <- (nu + h) * cos.phi * sin.lambda
  z <- (nu * (1L - e2) + h) * sin.phi

  list(x=x, y=y, z=z)

}


# Applies Helmert transform  using transform parameters t.
apply_transform <- function(points, t)   {

  # current points
  x1 <- points$x; y1 <- points$y; z1 <- points$z

  # transform parameters
  tx <- t$tx                  # x-shift
  ty <- t$ty                  # y-shift
  tz <- t$tz                  # z-shift
  s1 <- t$s + 1L              # scale: normalise parts-per-million to (s+1)
  rx <- (t$rx/3600L) / 57.29577951308232087679815481410517  # x-rotation: normalise arcseconds to radians
  ry <- (t$ry/3600L) / 57.29577951308232087679815481410517  # y-rotation: normalise arcseconds to radians
  rz <- (t$rz/3600L) / 57.29577951308232087679815481410517  # z-rotation: normalise arcseconds to radians

  # apply transform
  x2 <- tx + x1*s1 - y1*rz + z1*ry
  y2 <- ty + x1*rz + y1*s1 - z1*rx
  z2 <- tz - x1*ry + y1*rx + z1*s1

  list(x=x2, y=y2, z=z2)

}


# Converts cartesian (x/y/z) point to ellipsoidal geodetic latitude/longitude
# coordinates on specified epsg/datum.
cartesian_to_latlon <- function(points, epsg) {

  x <- points$x
  y <- points$y
  z <- points$z
  datum <- epsgs[epsgs[, "epsg"]==epsg, "datum"]

  ellipsoid <- latlon.datum[latlon.datum$datum==datum, "ellipsoid"]
  params <- latlon.ellipsoid[latlon.ellipsoid$ellipsoid==ellipsoid, 2:5]
  a <- params$a
  b <- params$b
  e2 <- params$e2

  p <- sqrt(x*x + y*y)  # distance from minor axis
  lambda <- atan2(y, x) # longitude
  phi <- atan2(z, p * (1L - e2))

  nu <- NA
  old.phi <- NA
  sin.phi <- NA
  repeat {
    sin.phi <- sin(phi)
    nu <- a / sqrt(1L - e2 * sin.phi * sin.phi)
    old.phi <- phi
    phi <- atan2(z + e2 * nu * sin.phi, p)
    if ( max(abs(old.phi - phi)) < 1e-12 ) { break }
  }

  lat <- phi * 57.29577951308232087679815481410517
  lon <- lambda * 57.29577951308232087679815481410517
  # height above ellipsoid [not currently used]
  h <- unname(p / cos(phi) - nu)

  list(latitude=lat, longitude=lon)

}
