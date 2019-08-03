################################################################################
#                                                                              #
# Adapted from 'Geo-Coordinates-OSGB-2.20', a Perl routine by Toby Thurston:   #
# https://metacpan.org/release/Geo-Coordinates-OSGB                            #
#                                                                              #
################################################################################

#' @encoding UTF-8
#' @title GCS to BNG Easting/Northing
#'
#' @description
#' Converts a geodetic coordinate system to BNG (projected) Easting/Northing
#' coordinates.
#'
#' @name sgs_lonlat_bng
#' @usage sgs_lonlat_bng(x, OSTN=TRUE)
#' @param x A \code{sgs_points} object with coordinates defined in a Geodetic
#' Coordinate System (e.g. epsg=4258, 4326 or 4277)
#' @param OSTN Logical variable indicating whether use OSTN15 transformation
#' when TRUE or
#' a less accurate but slightly faster single Helmert transformation when FALSE.
#' @details
#' The UK Ordnance Survey defined 'OSGB36' as the datum for the UK, based on the
#' 'Airy 1830' ellipsoid. However, in 2014, they deprecated OSGB36 in favour of
#' ETRS89 for longitude/latitude coordinates. Thus, \code{x} should be defined
#' as ETRS89 (or WGS84) most of the times.
#'
#' According to the Transformations and OSGM15 User Guide, p. 8:
#' \emph{"...ETRS89 is a precise version of the better known WGS84 reference
#' system optimised for use in Europe; however, for most purposes it can be
#' considered equivalent to WGS84."} and \emph{"For all navigation, mapping,
#' GIS, and engineering applications within the tectonically stable parts of
#' Europe (including UK and Ireland), the term ETRS89 should be taken as
#' synonymous with WGS84."} This means that EPSG:4258(ETRS89) and
#' EPSG:4326(WGS84) will be considered equivalent by this routine.
#'
#' \strong{Note}: All those coordinates outside the rectangle covered by OSTN15,
#' will be automatically computed using the small Helmert transformation. Such
#' coordinates will be accurate up to about +/-5 metres, therefore the resulting
#' easting and northing coordinates will be rounded to the metre.
#' Converting from lon/lat to BNG coordinates is faster than the other way
#' around, due to the iterative nature of the algorithm that is built into
#' OSTN15.
#' @return
#' An object of class \code{sgs_points} whose coordinates are defined as
#' Easting/Northing (epsg=27700). They are adjusted to the SW corner of
#' 1m grid square.
#' @seealso \code{\link{sgs_points}}, \code{\link{sgs_bng_lonlat}},
#' \code{\link{sgs_set_gcs}}.
#' @references
#' Ordnance Survey Limited, 2018. \emph{Transformations and OSGM15 user guide}
#' @examples
#' lon <- c(-4.25181,-3.18827)
#' lat <- c(55.86424, 55.95325)
#' pts <- sgs_points(list(longitude=lon, latitude=lat), epsg=4326)
#' bng.pts <- sgs_lonlat_bng(pts)
#' @export
sgs_lonlat_bng <- function(x, OSTN=TRUE) UseMethod("sgs_lonlat_bng")

#' @export
sgs_lonlat_bng.sgs_points <- function(x, OSTN=TRUE) {

  if (x$epsg == 27700)
    stop("This routine only supports geodetic coordinate systems")

  core.cols <- sgs_points.core

  additional.elements <- !names(x) %in% core.cols
  num.elements <- sum(additional.elements, na.rm=TRUE)

  # Convert datum from WGS84 to ETRS89
  # Currently we consider both EPSGs practically equal
  #if (x$epsg == 4326) { x <- sgs_set_gcs(x, to=4258) }
  if (x$epsg == 4326) {
    x$epsg <- 4258
    x$datum <- epsgs[epsgs[, "epsg"] == 4258, "datum"]
  }

  if (OSTN) {

    projected <- project.onto.grid(x$x, x$y, x$datum)
    e <- projected[, 1]
    n <- projected[, 2]

    # If datum is OSGB36, no need to do anything else
    if (x$epsg == 4277) {

      # Round to mm precision
      e <- round(e, 3)
      n <- round(n, 3)

    }

    # If datum is WGS84/ETRS89, we need to adjust with OSTN15
    if (x$epsg == 4258) {

      shifts <- find.OSTN.shifts.at(e, n)
      # Round to mm precision
      e <- round(e + shifts$dx, 3)
      n <- round(n + shifts$dy, 3)

      # Helmert shift into OSGB36 and then reproject all those coordinates
      # that are out of bounds of OSTN15.
      if (any(shifts$out) == TRUE) {
        helmert.x <- sgs_set_gcs(x[shifts$out], to = 4277)
        helmert.projected <- project.onto.grid(helmert.x$x,
                                               helmert.x$y,
                                               helmert.x$datum)
        e[shifts$out] <- round(helmert.projected[, 1], 0) # Round to metres
        n[shifts$out] <- round(helmert.projected[, 2], 0)
      }

    }

  } else {  # single Helmert transformation

    helmert.x <- sgs_set_gcs(x, to = 4277)
    helmert.projected <- project.onto.grid(helmert.x$x,
                                           helmert.x$y,
                                           helmert.x$datum)
    e <- round(helmert.projected[, 1], 0) # Round to metres
    n <- round(helmert.projected[, 2], 0)

  } # end if (OSTN)

  # Return values
  en <- list(x=e, y=n)
  if (num.elements > 0) en <- c(x[additional.elements], en)

  return (sgs_points(en, coords=c("x", "y"), epsg=27700))

}

#' @encoding UTF-8
#' @title British National Grid (BNG) Easting/Northing to Geodetic Coordinate
#' System (GCS)
#'
#' @description
#' Converts Ordnance Survey grid reference easting/northing coordinates to GCS
#' longitude/latitude (SW corner of grid square).
#'
#' @name sgs_bng_lonlat
#' @usage sgs_bng_lonlat(x, to = 4258, OSTN = TRUE)
#' @param x A \code{sgs_points} object with coordinates defined in the projected
#' coordinate system BNG (epsg=27700)
#' @param to Numeric. Sets the \code{epsg} code of the destination Geodetic
#' Coordinate System. 4258 (ETRS89) by default.
#' @param OSTN Logical variable indicating whether use OSTN15 transformation
#' when TRUE or a less accurate but slightly faster single Helmert
#' transformation when FALSE.
#' @details
#' The UK Ordnance Survey defined 'OSGB36' as the datum for the UK, based on the
#' 'Airy 1830' ellipsoid. However, in 2014, they deprecated OSGB36 in favour of
#' ETRS89 for longitude/latitude coordinates. Thus, when converting to
#' longitude/latitude the OSGB36 datum should be always converted to ETRS89
#' (or WGS84).
#'
#' According to the Transformations and OSGM15 User Guide, p. 8:
#' \emph{"...ETRS89 is a precise version of the better known WGS84 reference
#' system optimised for use in Europe; however, for most purposes it can be
#' considered equivalent to WGS84."} and \emph{"For all navigation, mapping,
#' GIS, and engineering applications within the tectonically stable parts of
#' Europe (including UK and Ireland), the term ETRS89 should be taken as
#' synonymous with WGS84."} This means that EPSG:4258(ETRS89) and
#' EPSG:4326(WGS84) will be considered equivalent by this routine.
#'
#' If, for historical reasons, longitude/latitude coordinates must have the old
#' OSGB36 datum, then the parameter \code{to} must be set to 4277.
#'
#' \strong{Note}: All those coordinates outside the rectangle covered by OSTN15,
#' will be automatically computed using the small Helmert transformation. Such
#' coordinates will be accurate up to about +/-5 metres.
#' Converting from BNG to lon/lat coordinates is slower than the other way
#' around, due to the iterative nature of the algorithm that is built into
#' OSTN15.
#' @return
#' An object of class \code{sgs_points} whose coordinates are defined as
#' Longitude/Latitude.
#' @seealso \code{\link{sgs_points}}, \code{\link{sgs_lonlat_bng}},
#' \code{\link{sgs_set_gcs}}.
#' @references
#' Ordnance Survey Limited, 2018. \emph{Transformations and OSGM15 user guide}
#' @examples
#' p <- sgs_points(list(651409.903, 313177.270), epsg=27700)
#' p.84 <- sgs_bng_lonlat(p) #ETRS89 lon/lat
#' p.36 <- sgs_bng_lonlat(p, to=4277) #OSGB36 lon/lat
#' @export
sgs_bng_lonlat <- function(x, to=4258, OSTN=TRUE) UseMethod("sgs_bng_lonlat")

#' @export
sgs_bng_lonlat.sgs_points <- function(x, to=4258, OSTN=TRUE) {

  if (x$epsg != 27700)
    stop("This routine only supports BNG Easting and Northing entries")
  if (!to %in% c(4258, 4326, 4277))
    stop("This routine only supports converting to epsg 4258, 4277 or 4326")

  core.cols <- sgs_points.core

  additional.elements <- !names(x) %in% core.cols
  num.elements <- sum(additional.elements, na.rm=TRUE)

  if (OSTN) {

    if (to == 4277) {
      unprojected <- unproject.onto.ellipsoid(x$x, x$y, x$datum)
    }

    if (to == 4258 || to == 4326) {

      shifts <- find.OSTN.shifts.at(x$x, x$y)
      e <- x$x - shifts$dx
      n <- x$y - shifts$dy
      last.shifts <- shifts

      for (i in c(1:20)) {

        shifts <- find.OSTN.shifts.at(e, n)
        if (all(shifts$out) == TRUE) {
          # all coordinates have been shifted off the edge
          break
        }

        e <- x$x - shifts$dx
        n <- x$y - shifts$dy
        if (max(abs(shifts$dx - last.shifts$dx)) < 0.0001 &&
            max(abs(shifts$dy - last.shifts$dy)) < 0.0001) { break }
        last.shifts <- shifts

      }

      #initialise 'unprojected' matrix of coordinates
      items <- rep(NA, length(x$x))
      unprojected <- cbind(items, items, deparse.level = 0)

      # unproject any shifted coordinates
      if (any(shifts$out) == FALSE) {
        e <- x$x[!shifts$out] - shifts$dx[!shifts$out]
        n <- x$y[!shifts$out] - shifts$dy[!shifts$out]
        unprojected[!shifts$out, ] <- unproject.onto.ellipsoid(e, n,
                                        epsgs[epsgs$epsg==to, "datum"])
      }

      # unproject the rest of coordinates (the ones that couldn't be shifted)
      if (any(shifts$out) == TRUE) {
        os.ll <- unproject.onto.ellipsoid(x$x[shifts$out],
                                          x$y[shifts$out], x$datum)
        os.ll.points <- sgs_set_gcs(sgs_points(list(x=os.ll[, 1], y=os.ll[, 2]),
                                               coords=c("x", "y"),
                                               epsg=4277),
                                    to=to)
        unprojected[shifts$out, ] <- cbind(x=os.ll.points$x,
                                           y=os.ll.points$y)
      }

    }
    unprojected <- list(x=unprojected[, 1], y=unprojected[, 2])
    if (num.elements > 0) unprojected <- c(x[additional.elements], unprojected)
    unprojected <- sgs_points(unprojected, coords=c("x", "y"), epsg=to)

  } else {  # single Helmert transformation

    os.ll <- unproject.onto.ellipsoid(x$x, x$y, x$datum)

    unprojected <- list(x=os.ll[, 1], y=os.ll[, 2])
    if (num.elements > 0) unprojected <- c(x[additional.elements], unprojected)
    unprojected <- sgs_set_gcs(sgs_points(unprojected, coords=c("x", "y"),
                                          epsg=4277), to=to)

  } # end if (OSTN)

  # In truth, we should have done the transformation to 4258 and then set_gcs
  # to 4326, but we consider them practically equal

  # Return
  unprojected

}

#Helper function. Unproject BNG (OSGB36) to geodetic coordinates
unproject.onto.ellipsoid <- function(e, n, datum) {

  E <- e
  N <- n

  ellipsoid <- lonlat.datum[lonlat.datum$datum==datum, "ellipsoid"]
  a <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid, "a"]   # Major
  b <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid, "b"]   # Minor
  e2 <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid, "e2"] # ecc.²

  f0 <- 0.9996012717                # Converge factor
  af <- a * f0
  n <- (a-b) / (a+b)
  N0 <- (-100000L); E0 <- (400000L) # northing & easting of true origin, metres

  dN <- N - N0
  dE <- E - E0
  # NatGrid true origin is 49°N 2°W:
  phi0 <- 49L / 57.29577951308232087679815481410517
  lambda0 <- -2L / 57.29577951308232087679815481410517

  phi <- phi0 + dN/af
  lambda <- lambda0


  M <- NA
  phi.plus <- NA
  phi.minus <- NA
  repeat {
    phi.minus <- phi - phi0
    phi.plus <- phi + phi0

    M <- b * f0 * (
    (1L + n * (1L + 5L/4L * n * (1L + n))) * phi.minus
    - 3L * n * (1L + n * (1L + 7L / 8L * n)) * sin(phi.minus) * cos(phi.plus)
    + (15L / 8L * n * (n * (1L + n))) * sin(2L * phi.minus) * cos(2L * phi.plus)
    - 35L / 24L * n^3 * sin(3L * phi.minus) * cos(3L * phi.plus)
    ) # meridional arc

    if ( max(abs(dN - M)) < 0.00001 ) { break } # ie until < 0.01mm
    phi <- phi + (dN - M) / af
  }

  cos.phi <- cos(phi)
  sin.phi <- sin(phi)
  tan.phi <- sin.phi / cos.phi

  splat <- 1L - e2 * sin.phi * sin.phi
  sqrtsplat <- sqrt(splat)
  nu <- af / sqrtsplat                        # nu = transverse r of curvature
  rho <- af * (1L - e2) / (splat * sqrtsplat) # rho = meridional r of curvature
  eta2 <- nu / rho - 1L                       # eta = ?

  tan2.phi <- tan.phi * tan.phi
  VII <- tan.phi / (2L * rho * nu)
  VIII <- tan.phi / (24L * rho * nu^3) *
    (5L + eta2 + ( 3L - 9L * eta2 ) * tan2.phi)
  IX <- tan.phi / (720L * rho * nu^5) *
    (61L + ( 90L + 45L * tan2.phi ) * tan2.phi)

  sec.phi <- 1L / cos.phi

  X <- sec.phi / nu
  XI <- sec.phi / (6L * nu^3) * (nu / rho + 2L * tan2.phi)
  XII <- sec.phi / (120L * nu^5) * ( 5L + ( 28L + 24L * tan2.phi ) * tan2.phi)
  XIIA <- sec.phi / (5040L * nu^7) *
    ( 61L + ( 662L + (1320L + 720L * tan2.phi) * tan2.phi ) * tan2.phi )

  dE2 <- dE * dE
  phi <- phi + ( -VII + ( VIII - IX * dE2 ) * dE2) * dE2
  lambda <- lambda + ( X + ( -XI + ( XII - XIIA * dE2 ) * dE2) * dE2) * dE

  unname(cbind(lambda * 57.29577951308232087679815481410517,
               phi    * 57.29577951308232087679815481410517))

}

#Helper function. Project geodetic coordinates onto BNG
project.onto.grid <- function (lon, lat, datum) {

  phi <- lat / 57.29577951308232087679815481410517
  lambda <- lon / 57.29577951308232087679815481410517

  ellipsoid <- lonlat.datum[lonlat.datum$datum==datum, "ellipsoid"]
  a <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid, "a"]   # Major
  b <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid, "b"]   # Minor
  e2 <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid, "e2"] # ecc.²

  f0 <- 0.9996012717      # Convergence factor
  af <- a * f0            # NatGrid scale factor on central meridian
  # NatGrid true origin is 49°N 2°W:
  phi0 <- 49L / 57.29577951308232087679815481410517
  lambda0 <- -2L / 57.29577951308232087679815481410517
  n0 <- -100000L; e0 <- 400000L   # northing & easting of true origin, metres
  n <- (a-b)/(a+b)

  cos.phi <- cos(phi)
  sin.phi <- sin(phi)
  sin2.phi <- sin(phi) * sin(phi)
  tan.phi <- sin.phi / cos.phi                # cos(phi) cannot be zero in GB
  tan2.phi <- tan.phi * tan.phi
  tan4.phi <- tan2.phi * tan2.phi

  splat <- 1L - e2 * sin2.phi
  sqrtsplat <- sqrt(splat)
  nu <- af / sqrtsplat                        # nu = transverse r of curvature
  rho <- af * (1L-e2) / (splat * sqrtsplat)   # rho = meridional r of curvature
  eta2 <- nu / rho - 1L                       # eta = ?

  phi.minus <- phi - phi0
  phi.plus <- phi + phi0

  # meridional arc
  M <- b * f0 * ((1L + n * (1L + 5L/4L * n * (1L + n)))* phi.minus
      - 3L * n * (1L + n * (1L + 7L/8L * n))  * sin(phi.minus) * cos(phi.plus)
      + (15L/8L * n * (n * (1L + n))) * sin(2L * phi.minus) * cos(2L * phi.plus)
      - 35L/24L * n^3 * sin(3L * phi.minus) * cos(3L * phi.plus)
  )

  I <- M + n0
  II <- (nu/2L) *sin.phi * cos.phi
  III <- (nu/24L) * sin.phi * cos.phi^3 * (5L - tan2.phi + 9L * eta2)
  IIIA <- (nu/720L) * sin.phi * cos.phi^5 * (61L-58L * tan2.phi + tan4.phi)

  IV <- nu * cos.phi
  V <- nu/6L * cos.phi^3 * (nu/rho - tan2.phi)
  VI <- (nu/120L) * cos.phi^5 *
    (5L - 18L * tan2.phi + tan4.phi + 14L * eta2 - 58L * tan2.phi * eta2)

  dlambda <- lambda-lambda0
  dlambda2 <- dlambda * dlambda

  n <- I +  ( II + ( III + IIIA * dlambda2 ) * dlambda2 ) * dlambda2
  e <- e0 + ( IV + ( V   + VI   * dlambda2 ) * dlambda2 ) * dlambda

  unname(cbind(e, n))

}

#Helper function. Get OSTN shift of coordinates
find.OSTN.shifts.at <- function(e, n) {

  # Initialise list of shifts
  items <- rep(NA, length(e))
  out <- rep(TRUE, length(e))
  shifts <- list(dx=items, dy=items, out=out)

  # No need to continue when everything is NA
  if (all(is.na(e))) { return (shifts) }

  # OSTN15 covers grid point (0, 0) to (700000,1250000)
  out.of.bounds <- (e < 0L | e > 700000L) | (n < 0L | n > 1250000L)
  shifts$out <- out.of.bounds


  if (!all(out.of.bounds)) {

    # set coordinates to km
    os.e <- e[!out.of.bounds] / 1000L
    os.n <- n[!out.of.bounds] / 1000L

    min.ee.shift <- 82140L
    min.nn.shift <- -84180L

    east.km <- trunc(os.e)
    north.km <- trunc(os.n)

    # R 'lists' are 1-based
    lle <- min.ee.shift + ostn_east_shift[east.km + north.km * 701L + 1L]
    lre <- min.ee.shift + ostn_east_shift[east.km + north.km * 701L + 2L]
    ule <- min.ee.shift + ostn_east_shift[east.km + north.km * 701L + 702L]
    ure <- min.ee.shift + ostn_east_shift[east.km + north.km * 701L + 703L]

    lln <- min.nn.shift + ostn_north_shift[east.km + north.km * 701L + 1L]
    lrn <- min.nn.shift + ostn_north_shift[east.km + north.km * 701L + 2L]
    uln <- min.nn.shift + ostn_north_shift[east.km + north.km * 701L + 702L]
    urn <- min.nn.shift + ostn_north_shift[east.km + north.km * 701L + 703L]

    t <- os.e - east.km
    u <- os.n - north.km

    one.t <- 1L - t
    one.u <- 1L - u
    dx <- (one.t * one.u * lle
           + t * one.u * lre
           + one.t * u * ule
           + t * u * ure)/1000L
    dy <- (one.t * one.u * lln
           + t * one.u * lrn
           + one.t * u * uln
           + t * u * urn)/1000L

    shifts$dx[!out.of.bounds] <- dx
    shifts$dy[!out.of.bounds] <- dy

  }

  return (shifts)

}
