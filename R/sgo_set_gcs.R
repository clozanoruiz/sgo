#' @encoding UTF-8
#' @title Set GCS of a set of points
#'
#' @description
#' Changes the geodetic coordinate system of a set of points using a single
#' Helmert transformation.
#'
#' @name sgo_set_gcs
#' @usage sgo_set_gcs(x, to = NULL)
#' @param x A \code{sgo_points} object describing a set of points in a geodetic
#' coordinate system.
#' @param to Specifies the EPSG code to convert the coordinates to. Currently it
#' can take any of the following values: \code{4258}, \code{4937}, \code{4936},
#' \code{4326}, \code{4979}, \code{4978} or \code{4277}.
#' @details
#' Changes the geodetic coordinate system of a set of points. Note that the
#' precision of various datums will vary, and (original) WGS-84 is not defined
#' to be accurate to better than ±1 metre. Most transformations shouldn't be
#' assumed to be accurate to better than a meter; between OSGB36 and WGS84
#' somewhat less - the lost of accuracy can be up to ±5m when using single
#' Helmert transformations).
#'
#' Input points with a projected coordinate system (e.g. 27700, 7405, 3035 or
#' 3857) are not allowed.
#'
#' \strong{Warning}
#' This function is mainly for internal use of the program. Since it relies on a
#' single Helmert transformation it is not recommended to call it directly. Use
#' any other of the transformation functions available (\link{sgo}).
#' @return
#' An object of class 'sgo_points'.
#' @seealso \code{\link{sgo_points}}, \code{\link{sgo_transform}}.
#' @examples
#' lon <- c(-4.25181,-3.18827)
#' lat <- c(55.86424, 55.95325)
#' p <- sgo_points(list(longitude=lon, latitude=lat), epsg=4326)
#' # warning: a single Helmert transformation is used in the next transformation
#' p2 <- sgo_set_gcs(p, to=4277)
#' # if higher precision is required to transform between OSGB36 lon/lat and
#' # ETRS89/WGS84 lon/lat then use the OSTN15 transformation (will be slower):
#' # Transform from WGS84 lon/lat coordinates to EPSG:4277 using OSTN15
#' p2 <- sgo_transform(p, to=4277)
#' @export
sgo_set_gcs <- function(x, to=NULL) UseMethod("sgo_set_gcs")

#' @export
sgo_set_gcs.sgo_points <- function (x, to=NULL) {

  if (x$epsg == to) { return (x) }

  # Check x is a GCS (not projected)
  coord.system <- .epsgs[.epsgs$epsg==x$epsg, "type"]
  if (coord.system != "GCS")
    stop("This routine only accepts Geodetic Coordinate Systems")
  if (to %in% c(27700, 7405, 3035, 3857))
    stop("This routine only transforms to Geodetic Coordinate Systems")

  # If converting from 3D to 2D in the same datum, just remove the z coordinate
  # and return
  if ((x$epsg == 4937 && to == 4258) || (x$epsg == 4979 && to == 4326)) {
    x$z <- NULL
    x$dimension <- "XY"
    x$epsg <- to
    return (x)
  }

  # If converting from 2D to 3D in the same datum, just add a z coordinate
  # and return
  if ((x$epsg == 4258 && to == 4937) || (x$epsg == 4326 && to == 4979)) {
    x$z <- 0
    x$dimension <- "XYZ"
    x$epsg <- to
    return (x)
  }

  coord.format <- .epsgs[.epsgs$epsg==x$epsg, "format"] # ll or c
  coord.to.format <- .epsgs[.epsgs$epsg==to, "format"]

  x.3d <- x$dimension == "XYZ"
  if (x.3d) {
    names.in.core <- names(x) %in% .sgo_points.3d.core
  } else {
    names.in.core <- names(x) %in% .sgo_points.2d.core
  }
  additional.elements <- !names.in.core
  num.elements <- sum(additional.elements, na.rm=TRUE)
  lst.additional.elements <- x[additional.elements]

  to.datum <- .epsgs[.epsgs$epsg==to, "datum"]
  transform <- NULL

  # Don't need all the extra columns it might have while doing calculations
  if (num.elements > 0)
    x <- structure(x[names.in.core], class = "sgo_points")

  if (x$datum == "ETRS89") {
    # converting from ETRS89
    transform <- lonlat.datum[lonlat.datum$datum==to.datum, 3:9]
  }
  if (to.datum == "ETRS89") {
    # converting to ETRS89; use inverse transform
    transform <- -lonlat.datum[lonlat.datum$datum==x$datum, 3:9]
  }
  if (is.null(transform)) {
    # neither x$datum nor to.datum are ETRS89 pipe through ETRS89 first
    if (coord.format == "c") {
      x <- sgo_set_gcs(x, to=4936)
    } else if (x.3d) {
      x <- sgo_set_gcs(x, to=4937)
    } else {
      x <- sgo_set_gcs(x, to=4258)
    }
    transform <- lonlat.datum[lonlat.datum$datum==to.datum, 3:9]
  }

  if (coord.format == "ll") {
    old.cartesian <- .lonlat_to_cartesian(x)
  } else {
    old.cartesian <- list(x=x$x, y=x$y, z=x$z)
  }
  new.coords <- .apply_transform(old.cartesian, transform)

  if (coord.to.format == "ll") {
    new.coords <- .cartesian_to_lonlat(new.coords, to)
  }

  if (.epsgs[.epsgs$epsg==to, "dimension"] != "XYZ") {
    new.coords <- new.coords[1:2]
    dimension <- "XY"
  } else {
    if (!x.3d) new.coords$z <- rep(0, length(x$x))
    dimension <- "XYZ"
  }

  # return sgo_points object
  if (num.elements > 0)
    new.coords <- c(new.coords, lst.additional.elements)

  structure(c(new.coords, epsg = to, datum = .epsgs[.epsgs$epsg == to, "datum"],
              dimension = dimension),
            class = "sgo_points")

}


# Converts from geodetic longitude/latitude coordinates to geocentric
# cartesian (x/y/z) coordinates.
#' @noRd
#' @param points A sgo_points object, or classless list with the same elements.
.lonlat_to_cartesian <- function(points) {

  datum <- points$datum
  ellipsoid <- lonlat.datum[lonlat.datum$datum==datum, "ellipsoid"]

  phi <- points$y / RAD.TO.GRAD
  lambda <- points$x / RAD.TO.GRAD
  # height above ellipsoid
  h <- if (points$dimension=="XYZ") points$z else rep(0, length(points$x))
  a <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid, "a"]
  e2 <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid, "e2"]

  sin.phi <- sin(phi)
  cos.phi <- cos(phi)
  sin.lambda <- sin(lambda)
  cos.lambda <- cos(lambda)

  nu <- a / sqrt(1 - e2 * sin.phi * sin.phi) #r of curvature in prime vertical

  x <- (nu + h) * cos.phi * cos.lambda
  y <- (nu + h) * cos.phi * sin.lambda
  z <- (nu * (1 - e2) + h) * sin.phi

  list(x=x, y=y, z=z)

}


# Applies Helmert transform  using transform parameters t.
#' @noRd
#' @param points A sgo_points object.
#' @param t A dataframe with ellipsoid paramaters
.apply_transform <- function(points, t)   {

  # current points
  x1 <- points$x; y1 <- points$y; z1 <- points$z

  # transform parameters
  tx <- t$tx                  # x-shift
  ty <- t$ty                  # y-shift
  tz <- t$tz                  # z-shift
  s1 <- t$s + 1               # scale: normalise parts-per-million to (s+1)
  # x, y, z rotations: normalise arcseconds to radians
  rx <- (t$rx/3600) / RAD.TO.GRAD
  ry <- (t$ry/3600) / RAD.TO.GRAD
  rz <- (t$rz/3600) / RAD.TO.GRAD

  # apply transform
  x2 <- tx + x1*s1 - y1*rz + z1*ry
  y2 <- ty + x1*rz + y1*s1 - z1*rx
  z2 <- tz - x1*ry + y1*rx + z1*s1

  list(x=x2, y=y2, z=z2)

}


# Converts cartesian (x/y/z) point to ellipsoidal geodetic longitude/latitude
# coordinates on specified epsg/datum.
#' @noRd
#' @param points A sgo_points object.
#' @param epsg A scalar number with a EPSG code
.cartesian_to_lonlat <- function(points, epsg) {

  x <- points$x
  y <- points$y
  z <- points$z
  datum <- .epsgs[.epsgs$epsg==epsg, "datum"]

  ellipsoid <- lonlat.datum[lonlat.datum$datum==datum, "ellipsoid"]
  params <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid, 2:5]
  a <- params$a
  b <- params$b
  e2 <- params$e2

  p <- sqrt(x*x + y*y)  # distance from minor axis
  lambda <- atan2(y, x) # longitude
  phi <- atan2(z, p * (1 - e2))

  nu <- NA
  old.phi <- NA
  sin.phi <- NA
  repeat {
    sin.phi <- sin(phi)
    nu <- a / sqrt(1 - e2 * sin.phi * sin.phi)
    old.phi <- phi
    phi <- atan2(z + e2 * nu * sin.phi, p)
    if ( max(abs(old.phi - phi)) < 1e-12 ) { break }
  }

  lat <- phi * RAD.TO.GRAD
  lon <- lambda * RAD.TO.GRAD
  # height above ellipsoid
  h <- unname(p / cos(phi) - nu)

  list(x=lon, y=lat, z=h)

}

#' @encoding UTF-8
#' @title Geodetic Coordinate System (GCS) in polar coordinates to cartesian
#' coordinates
#'
#' @description
#' Converts a GCS expressed in Longitude and Latitude
#' (and Ellipsoid Height) to an Earth-centered Earth-fixed (ECEF) cartesian
#' coordinate system.
#'
#' @name sgo_lonlat_cart
#' @usage sgo_lonlat_cart(x)
#' @param x A \code{sgo_points} object with coordinates expressed as Longitude
#' and Latitude (and Ellipsoid Height if they are 3D points).
#' @details
#' Currently converts from EPSGs \code{4258} and \code{4937} to \code{4936} or
#' from EPSGs \code{4326}, \code{4979} to \code{4978}
#' @return
#' An object of class \code{sgo_points} whose coordinates are defined as a
#' x, y and z cartesian vector.
#' @seealso \code{\link{sgo_points}}, \code{\link{sgo_lonlat_bng}},
#' \code{\link{sgo_set_gcs}}.
#' @examples
#' p <- sgo_points(list(-5.00355049, 56.7968571), epsg=4326)
#' p.xyz <- sgo_lonlat_cart(p) #Cartesian coordinates
#' @export
sgo_lonlat_cart <- function(x) UseMethod("sgo_lonlat_cart")

#' @export
sgo_lonlat_cart.sgo_points <- function(x) {

  if (!x$epsg %in% c(4258, 4937, 4326, 4979))
    stop("This routine can only convert from epsg 4258, 4937, 4326 or 4979")

  # set the proper output EPSG
  if (x$datum == "ETRS89") {
    to.epsg <- 4936
  } else {
    to.epsg <- 4978
  }

  if (x$dimension == "XY") {
    additional.elements <- !names(x) %in% .sgo_points.2d.core
    cartesian <- .lonlat_to_cartesian(x[.sgo_points.2d.core])
  } else {
    additional.elements <- !names(x) %in% .sgo_points.3d.core
    cartesian <- .lonlat_to_cartesian(x[.sgo_points.3d.core])
  }
  #cartesian <- lapply(cartesian, round, 3) #round to mm
  num.elements <- sum(additional.elements, na.rm=TRUE)

  # return sgo_points object
  if (num.elements > 0)
    cartesian <- c(cartesian, x[additional.elements])

  structure(c(cartesian, epsg = to.epsg,
              datum = .epsgs[.epsgs$epsg == to.epsg, "datum"],
              dimension = "XYZ"),
            class = "sgo_points")

}


#' @encoding UTF-8
#' @title Geodetic Coordinate System (GCS) in cartesian coordinates to polar
#' coordinates
#'
#' @description
#' Converts a GCS expressed Earth-centered Earth-fixed (ECEF) cartesian
#' coordinate to Longitude and Latitude and Ellipsoid Height.
#'
#' @name sgo_cart_lonlat
#' @usage sgo_cart_lonlat(x)
#' @param x A \code{sgo_points} object with coordinates expressed in cartesian
#' coordinates
#' @details
#' Currently converts from EPSGs \code{4936} and \code{4978} to \code{4937} and
#' \code{4979}
#' @return
#' An object of class \code{sgo_points} with polar coordinates (Longitude,
#' Latitude and Ellipsoid Height).
#' @seealso \code{\link{sgo_points}}, \code{\link{sgo_bng_lonlat}},
#' \code{\link{sgo_set_gcs}}.
#' @examples
#' p <- sgo_points(list(3487823.234, -305433.201, 5313739.634), epsg=4936)
#' p.xyz <- sgo_cart_lonlat(p) #Cartesian coordinates
#' @export
sgo_cart_lonlat <- function(x) UseMethod("sgo_cart_lonlat")

#' @export
sgo_cart_lonlat.sgo_points <- function(x) {

  if (!x$epsg %in% c(4936, 4978))
    stop("This routine can only convert from epsg 4936, 4978")

  # set the proper output EPSG
  if (x$datum == "ETRS89") {
    to.epsg <- 4937
  } else {
    to.epsg <- 4979
  }

  additional.elements <- !names(x) %in% .sgo_points.3d.core
  num.elements <- sum(additional.elements, na.rm=TRUE)

  lonlat <- .cartesian_to_lonlat(list(x=x$x, y=x$y, z=x$z), to.epsg)
  #lonlat$z <- round(lonlat$z, 3) #round to mm

  # return sgo_points object
  if (num.elements > 0)
    lonlat <- c(lonlat, x[additional.elements])

  structure(c(lonlat, epsg = to.epsg,
              datum = .epsgs[.epsgs$epsg == to.epsg, "datum"],
              dimension = "XYZ"),
            class = "sgo_points")

}
