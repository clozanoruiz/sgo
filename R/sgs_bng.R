#' @encoding UTF-8
#' @title GCS to BNG Easting/Northing
#'
#' @description
#' Converts a geodetic coordinate system to BNG (projected) Easting/Northing
#' coordinates. It also transforms GPS ellipsoid heights to orthometric
#' (mean sea level) heights on the relevant Ordnance Survey mapping datum, using
#' the National Geoid Model OSGM15.
#'
#' @name sgs_lonlat_bng
#' @usage sgs_lonlat_bng(x, to=27700, OSTN=TRUE, OD=FALSE)
#' @param x A \code{sgs_points} object with coordinates defined in a Geodetic
#' Coordinate System expressed as Longitude and Latitude (e.g. epsg=4258, 4937,
#' 4326, 4979 or 4277)
#' @param to Specifies the EPSG code to convert the coordinates to. It can only
#' take the following values: \code{27700} or \code{7405}.
#' @param OSTN Logical variable indicating whether use OSTN15 transformation
#' when TRUE or a less accurate but slightly faster single Helmert
#' transformation when FALSE.
#' @param OD Logical variable. When TRUE, and the output contains a
#' column with heights, then a new column is added to the result indicating the
#' ordnance datum (OD) used on each point. It is ignored when \code{OSTN=FALSE}
#' or data doesn't contain 3D points.
#' @details
#' The UK Ordnance Survey defined 'OSGB36' as the datum for the UK, based on the
#' 'Airy 1830' ellipsoid. However, in 2014, they deprecated OSGB36 in favour of
#' ETRS89 for longitude/latitude coordinates. Thus, \code{x} should be defined
#' as ETRS89 (or WGS84) most of the times.
#'
#' Note: When transforming from EPSG=4277 any height included in the input
#' will be simply discarded (see \code{\link{sgs_points}}).
#'
#' According to the Transformations and OSGM15 User Guide, p. 8:
#' \emph{"...ETRS89 is a precise version of the better known WGS84 reference
#' system optimised for use in Europe; however, for most purposes it can be
#' considered equivalent to WGS84."} and \emph{"For all navigation, mapping,
#' GIS, and engineering applications within the tectonically stable parts of
#' Europe (including UK and Ireland), the term ETRS89 should be taken as
#' synonymous with WGS84."} This means that EPSGs with the ETRS89 datum or
#' WGS84 will be considered equivalent by this routine.
#'
#' \strong{Note}: All those coordinates outside the rectangle covered by OSTN15
#' will be automatically computed using the small Helmert transformation. Such
#' coordinates will be accurate up to about +/-5 metres, therefore the resulting
#' easting and northing coordinates will be rounded to the metre. Since those
#' coordinates are outside of the OSTN15 domain the resulting coordinates will
#' have the resulting height defined as \code{NA}.
#' Similarly, when using \code{OSTN=FALSE} on 3D coordinates, the result will
#' have all the heights defined as \code{NA}.
#' Converting from lon/lat to BNG coordinates is faster than the other way
#' around, due to the iterative nature of the algorithm that is built into
#' OSTN15.
#' @return
#' An object of class \code{sgs_points} whose coordinates are defined as
#' Easting/Northing (epsg=27700 or 7405). They are adjusted to the SW corner of
#' 1m grid square. If \code{OD=TRUE} a column named \code{height.datum} is
#' added to the resulting object.
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
sgs_lonlat_bng <- function(x, to=27700, OSTN=TRUE, OD=FALSE)
  UseMethod("sgs_lonlat_bng")

#' @export
sgs_lonlat_bng.sgs_points <- function(x, to=27700, OSTN=TRUE, OD=FALSE) {

  coord.system <- .epsgs[.epsgs$epsg==x$epsg, c("type", "format")]
  if (coord.system$type != "GCS" || coord.system$format != "ll")
    stop("This routine only only accepts Geodetic Coordinate Systems")

  x.3d <- x$dimension == "XYZ"
  out.dimension <- .epsgs[.epsgs$epsg==to, "dimension"]
  if (out.dimension == "XY/Z")
    out.dimension <- "XY"

  if (x.3d) {
    x.coords <- .sgs_points.3d.coords
    core.cols <- .sgs_points.3d.core
  } else {
    x.coords <- .sgs_points.2d.coords
    core.cols <- .sgs_points.2d.core
  }

  additional.elements <- !names(x) %in% core.cols
  num.elements <- sum(additional.elements, na.rm=TRUE)

  # Convert datum from WGS84 to ETRS89
  # Currently we consider both EPSGs practically equal
  if (x$datum == "WGS84") {
    if (x$epsg == 4326) {
      x$epsg <- 4258
      x$datum <- .epsgs[.epsgs$epsg == 4258, "datum"]
    } else {
      x$epsg <- 4937
      x$datum <- .epsgs[.epsgs$epsg == 4937, "datum"]
    }
  }

  if (OSTN) {
    out.of.bounds <- FALSE # as of now no coordinate out of bounds

    projected <- .project.onto.grid(x$x, x$y, x$datum)
    e <- projected[, 1]
    n <- projected[, 2]

    # If datum is OSGB36, no need to do anything else
    if (x$epsg == 4277) {

      # Round to mm precision
      e <- round(e, 3)
      n <- round(n, 3)

    }

    # If datum is WGS84/ETRS89, we need to adjust with OSTN15
    if (x$epsg %in% c(4258, 4937)) {

      shifts <- .find.OSTN.shifts.at(e, n, x.3d)
      # Round to mm precision
      e <- round(e + shifts$dx, 3)
      n <- round(n + shifts$dy, 3)

      # Helmert shift into OSGB36 and then reproject all those coordinates
      # that are out of bounds of OSTN15.
      if (any(shifts$out) == TRUE) {
        out.of.bounds <- TRUE
        out.x <- sgs_points(lapply(x[x.coords], function(el) el[shifts$out]),
                            coords = x.coords, epsg = x$epsg)
        helmert.x <- sgs_set_gcs(out.x, to = 4277)
        helmert.projected <- .project.onto.grid(helmert.x$x,
                                                helmert.x$y,
                                                helmert.x$datum)
        e[shifts$out] <- round(helmert.projected[, 1], 0) # Round to metres
        n[shifts$out] <- round(helmert.projected[, 2], 0)
      }

    }

    if (out.of.bounds)
      warning("There are points outside of the OSTN15 rectangle")

  } else {  # single Helmert transformation

    helmert.x <- sgs_set_gcs(sgs_points(x[x.coords], coords = x.coords,
                                        epsg = x$epsg), to = 4277)
    helmert.projected <- .project.onto.grid(helmert.x$x,
                                            helmert.x$y,
                                            helmert.x$datum)
    e <- round(helmert.projected[, 1], 0) # Round to metres
    n <- round(helmert.projected[, 2], 0)

  } # end if (OSTN)


  # Return values with correct EPSG depending on input and output.
  if (out.dimension == "XYZ") {
    if (OSTN && x.3d) {
      if (OD) {
        en <- list(x=e, y=n, z=round(x$z - shifts$dz, 3),
                   height.datum=datum.flags[match(shifts$gf,
                                             datum.flags$geoid.datum.flag), 4])
      } else {
        en <- list(x=e, y=n, z=round(x$z - shifts$dz, 3))
      }
    } else {
      en <- list(x=e, y=n, z=rep(0, length(e)))
      #warning("Converted from 2D to 3D. Hence heights default to 0")
    }
  } else {
    en <- list(x=e, y=n)
  }

  if (num.elements > 0)
    en <- c(en, x[additional.elements])

  structure(c(en, epsg=to, datum=.epsgs[.epsgs$epsg==to, "datum"],
              dimension=out.dimension),
            class="sgs_points")

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
#' @usage sgs_bng_lonlat(x, to = 4258, OSTN = TRUE, OD = FALSE)
#' @param x A \code{sgs_points} object with coordinates defined in the projected
#' coordinate system BNG (EPSGs 27700 or 7405)
#' @param to Numeric. Sets the \code{epsg} code of the destination Geodetic
#' Coordinate System. 4258 (ETRS89) by default.
#' @param OSTN Logical variable indicating whether use OSTN15 transformation
#' when TRUE or a less accurate but slightly faster single Helmert
#' transformation when FALSE.
#' @param OD Logical variable. When TRUE, and the output contains a
#' column with heights, then a new column is added to the result indicating the
#' ordnance datum (OD) used on each point. It is ignored when \code{OSTN=FALSE}
#' or data doesn't contain 3D points.
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
#' synonymous with WGS84."} This means that ETRS89 and WGS84 datums will be
#' considered equivalent by this routine.
#'
#' If, for historical reasons, longitude/latitude coordinates must have the old
#' OSGB36 datum, then the parameter \code{to} must be set to 4277.
#'
#' \strong{Note}: All those coordinates outside the rectangle covered by OSTN15
#' will be automatically computed using the small Helmert transformation. Such
#' coordinates will be accurate up to about +/-5 metres.
#' Converting from BNG to lon/lat coordinates is slower than the other way
#' around, due to the iterative nature of the algorithm that is built into
#' OSTN15.
#' @return
#' An object of class \code{sgs_points} whose coordinates are defined as
#' Longitude/Latitude.If \code{OD=TRUE} a column named \code{height.datum} is
#' added to the resulting object.
#' @seealso \code{\link{sgs_points}}, \code{\link{sgs_lonlat_bng}},
#' \code{\link{sgs_set_gcs}}.
#' @references
#' Ordnance Survey Limited, 2018. \emph{Transformations and OSGM15 user guide}
#' @examples
#' p <- sgs_points(list(651409.903, 313177.270), epsg=27700)
#' p.84 <- sgs_bng_lonlat(p) #ETRS89 lon/lat
#' p.36 <- sgs_bng_lonlat(p, to=4277) #OSGB36 lon/lat
#' @export
sgs_bng_lonlat <- function(x, to=4258, OSTN=TRUE, OD=FALSE)
  UseMethod("sgs_bng_lonlat")

#' @export
sgs_bng_lonlat.sgs_points <- function(x, to=4258, OSTN=TRUE, OD=FALSE) {

  if (!x$epsg %in% c(27700, 7405))
    stop("This routine only supports BNG Easting and Northing entries")

  if (!to %in% c(4258, 4937, 4326, 4979, 4277))
    stop("This routine only supports converting to polar coordinates")

  has.z <- x$dimension == "XYZ"
  out.dimension <- .epsgs[.epsgs$epsg==to, "dimension"]
  if (out.dimension == "XY/Z")
    out.dimension <- "XY"

  if (has.z) {
    core.cols <- .sgs_points.3d.core
  } else {
    core.cols <- .sgs_points.2d.core
  }

  additional.elements <- !names(x) %in% core.cols
  num.elements <- sum(additional.elements, na.rm=TRUE)

  if (OSTN) {
    out.of.bounds <- FALSE # as of now no coordinate out of bounds

    if (to == 4277) {
      unprojected <- .unproject.onto.ellipsoid(x$x, x$y, x$datum)
    }

    if (to %in% c(4258, 4937, 4326, 4979)) {

      shifts <- .find.OSTN.shifts.at(x$x, x$y, has.z)
      e <- x$x - shifts$dx
      n <- x$y - shifts$dy
      last.shifts <- shifts

      for (i in c(1:20)) {

        shifts <- .find.OSTN.shifts.at(e, n, has.z)
        if (all(shifts$out) == TRUE) {
          # all coordinates have been shifted off the edge
          break
        }

        e <- x$x - shifts$dx
        n <- x$y - shifts$dy
        if (max(abs(shifts$dx - last.shifts$dx), na.rm=TRUE) < 0.0001 &&
            max(abs(shifts$dy - last.shifts$dy), na.rm=TRUE) < 0.0001) {
          break
        }
        last.shifts <- shifts

      }

      #initialise 'unprojected' matrix of coordinates
      items <- rep(NA_real_, length(x$x))
      unprojected <- cbind(items, items, deparse.level = 0)

      # unproject any shifted coordinates
      if (any(shifts$out == FALSE)) {
        e <- x$x[!shifts$out] - shifts$dx[!shifts$out]
        n <- x$y[!shifts$out] - shifts$dy[!shifts$out]
        unprojected[!shifts$out, ] <- .unproject.onto.ellipsoid(e, n,
                                        .epsgs[.epsgs$epsg==to, "datum"])
      }

      # unproject the rest of coordinates (the ones that couldn't be shifted)
      if (any(shifts$out == TRUE)) {
        out.of.bounds <- TRUE
        os.ll <- .unproject.onto.ellipsoid(x$x[shifts$out],
                                           x$y[shifts$out], x$datum)
        os.ll.points <- sgs_set_gcs(sgs_points(list(x=os.ll[, 1], y=os.ll[, 2]),
                                               coords=.sgs_points.2d.coords,
                                               epsg=4277),
                                    to=to)
        unprojected[shifts$out, ] <- cbind(x=os.ll.points$x, y=os.ll.points$y)
      }

    }

    if (out.of.bounds)
      warning("There are points outside of the OSTN15 rectangle")

  } else {  # single Helmert transformation

    unprojected <- .unproject.onto.ellipsoid(x$x, x$y, x$datum)

  } # end if (OSTN)


  if (out.dimension == "XYZ") {
    if (OSTN && has.z) {
      if (OD) {

        unprojected <- list(x=unprojected[, 1], y=unprojected[, 2],
                            z=round(x$z + shifts$dz, 4),
                            height.datum=datum.flags[match(shifts$gf,
                                            datum.flags$geoid.datum.flag), 4])
      } else {
        unprojected <- list(x=unprojected[, 1], y=unprojected[, 2],
                            z=round(x$z + shifts$dz, 4))
      }
    } else {
      unprojected <- list(x=unprojected[, 1], y=unprojected[, 2],
                          z=rep(0, length(unprojected[, 1])))
      #warning("Converted from 2D to 3D. Hence heights default to 0")
    }
  } else {
    unprojected <- list(x=unprojected[, 1], y=unprojected[, 2])
  }

  if (num.elements > 0)
    unprojected <- c(unprojected, x[additional.elements])

  # We consider ETRS89/WGS84 practically equal...
  #Return
  if (OSTN) {
    structure(c(unprojected, epsg=to,
              datum=.epsgs[.epsgs$epsg==to, "datum"],
              dimension=out.dimension), class="sgs_points")
  } else {
    sgs_set_gcs(structure(c(unprojected, epsg=4277,
        datum=.epsgs[.epsgs$epsg==4277, "datum"],
        dimension="XY"), class="sgs_points"), to=to)
  }

}

# Helper function. Unproject BNG (OSGB36) to geodetic coordinates
#' @noRd
#' @param E A numeric vector with Easting coordinates
#' @param N A numeric vector with Northing coordinates
#' @param datum A string containing "OSGB36", "WGS84" or "ETRS89"
.unproject.onto.ellipsoid <- function(E, N, datum) {

  ellipsoid <- lonlat.datum[lonlat.datum$datum==datum, "ellipsoid"]
  a <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid, "a"]   # Major
  b <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid, "b"]   # Minor
  e2 <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid, "e2"] # ecc.²

  f0 <- 0.9996012717              # Converge factor
  af <- a * f0
  bf <- b * f0
  n <- (a-b) / (a+b)
  N0 <- (-100000); E0 <- (400000) # northing & easting of true origin, metres

  dN <- N - N0
  dE <- E - E0
  # NatGrid true origin is 49°N 2°W:
  phi0 <- 49 / RAD.TO.GRAD
  lambda0 <- -2 / RAD.TO.GRAD

  phi <- phi0 + dN/af
  lambda <- lambda0


  M <- NA
  phi.plus <- NA
  phi.minus <- NA
  repeat {
    phi.minus <- phi - phi0
    phi.plus <- phi + phi0

    M <- bf * (
    (1 + n * (1 + 5/4 * n * (1L + n))) * phi.minus
    - 3 * n * (1 + n * (1 + 7 / 8 * n)) * sin(phi.minus) * cos(phi.plus)
    + (15 / 8 * n * (n * (1 + n))) * sin(2 * phi.minus) * cos(2 * phi.plus)
    - 35 / 24 * n^3 * sin(3 * phi.minus) * cos(3 * phi.plus)
    ) # meridional arc

    if ( max(abs(dN - M)) < 0.00001 ) { break } # ie until < 0.01mm
    phi <- phi + (dN - M) / af
  }

  cos.phi <- cos(phi)
  sin.phi <- sin(phi)
  tan.phi <- sin.phi / cos.phi

  splat <- 1 - e2 * sin.phi * sin.phi
  sqrtsplat <- sqrt(splat)

  # radius of curvature at latitude φ perpendicular to a meridian
  nu <- af / sqrtsplat
  # radius of curvature of a meridian at latitude φ
  rho <- af * (1 - e2) / (splat * sqrtsplat)
  # East - west component of the deviation of the vertical, squared
  eta2 <- nu / rho - 1

  tan2.phi <- tan.phi * tan.phi
  VII <- tan.phi / (2 * rho * nu)
  VIII <- tan.phi / (24 * rho * nu^3) *
    (5 + eta2 + ( 3 - 9 * eta2 ) * tan2.phi)
  IX <- tan.phi / (720 * rho * nu^5) *
    (61 + ( 90 + 45 * tan2.phi ) * tan2.phi)

  sec.phi <- 1 / cos.phi

  X <- sec.phi / nu
  XI <- sec.phi / (6 * nu^3) * (nu / rho + 2 * tan2.phi)
  XII <- sec.phi / (120 * nu^5) * ( 5 + ( 28 + 24 * tan2.phi ) * tan2.phi)
  XIIA <- sec.phi / (5040 * nu^7) *
    ( 61 + ( 662 + (1320 + 720 * tan2.phi) * tan2.phi ) * tan2.phi )

  dE2 <- dE * dE
  phi <- phi + ( -VII + ( VIII - IX * dE2 ) * dE2) * dE2
  lambda <- lambda + ( X + ( -XI + ( XII - XIIA * dE2 ) * dE2) * dE2) * dE

  unname(cbind(lambda * RAD.TO.GRAD, phi * RAD.TO.GRAD))

}

# Helper function. Project geodetic coordinates onto BNG
#' @noRd
#' @param lon A numeric vector with Longitude coordinates
#' @param lat A numeric vector with Latitude coordinates
#' @param datum A string containing "OSGB36", "WGS84" or "ETRS89"
.project.onto.grid <- function (lon, lat, datum) {

  phi <- lat / RAD.TO.GRAD
  lambda <- lon / RAD.TO.GRAD

  ellipsoid <- lonlat.datum[lonlat.datum$datum==datum, "ellipsoid"]
  a <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid, "a"]   # Major
  b <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid, "b"]   # Minor
  e2 <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid, "e2"] # ecc.²

  f0 <- 0.9996012717      # Convergence factor
  af <- a * f0            # NatGrid scale factor on central meridian
  # NatGrid true origin is 49°N 2°W:
  phi0 <- 49 / RAD.TO.GRAD
  lambda0 <- -2 / RAD.TO.GRAD
  n0 <- -100000; e0 <- 400000   # northing & easting of true origin, metres
  n <- (a-b)/(a+b)

  cos.phi <- cos(phi)
  sin.phi <- sin(phi)
  sin2.phi <- sin.phi * sin.phi
  tan.phi <- sin.phi / cos.phi               # cos(phi) cannot be zero in GB
  tan2.phi <- tan.phi * tan.phi
  tan4.phi <- tan2.phi * tan2.phi

  splat <- 1 - e2 * sin2.phi
  sqrtsplat <- sqrt(splat)
  nu <- af / sqrtsplat                       # nu = transverse r of curvature
  rho <- af * (1-e2) / (splat * sqrtsplat)
  eta2 <- nu / rho - 1

  phi.minus <- phi - phi0
  phi.plus <- phi + phi0

  # meridional arc
  M <- b * f0 * ((1 + n * (1 + 5/4 * n * (1 + n)))* phi.minus
      - 3 * n * (1 + n * (1 + 7/8 * n))  * sin(phi.minus) * cos(phi.plus)
      + (15/8 * n * (n * (1 + n))) * sin(2 * phi.minus) * cos(2 * phi.plus)
      - 35/24 * n^3 * sin(3 * phi.minus) * cos(3 * phi.plus)
  )

  I <- M + n0
  II <- (nu/2) *sin.phi * cos.phi
  III <- (nu/24) * sin.phi * cos.phi^3 * (5 - tan2.phi + 9 * eta2)
  IIIA <- (nu/720) * sin.phi * cos.phi^5 * (61-58 * tan2.phi + tan4.phi)

  IV <- nu * cos.phi
  V <- nu/6 * cos.phi^3 * (nu/rho - tan2.phi)
  VI <- (nu/120) * cos.phi^5 *
    (5 - 18 * tan2.phi + tan4.phi + 14 * eta2 - 58 * tan2.phi * eta2)

  dlambda <- lambda-lambda0
  dlambda2 <- dlambda * dlambda

  n <- I +  ( II + ( III + IIIA * dlambda2 ) * dlambda2 ) * dlambda2
  e <- e0 + ( IV + ( V   + VI   * dlambda2 ) * dlambda2 ) * dlambda

  unname(cbind(e, n))

}

# Helper function. Get OSTN shift of coordinates
#' @noRd
#' @param e A numeric vector with Easting coordinates
#' @param n A numeric vector with Northing coordinates
#' @param z A numeric vector with Height coordinates
.find.OSTN.shifts.at <- function(e, n, z=FALSE) {

  # Initialise list of shifts
  len.e <- length(e)
  items <- rep(NA_real_, len.e)
  out <- rep(FALSE, len.e)
  shifts <- list(dx=items, dy=items, dz=items, gf=items, out=out)

  # No need to continue when everything is NA
  if (all(is.na(e))) {
    shifts$out <- rep(TRUE, len.e)
    return (shifts)
  }

  # OSTN15 covers grid point (0, 0) to (700000, 1250000)
  out.of.bounds <- (e < 0 | e > 700000) |
                   (n < 0 | n > 1250000) #| (is.na(e) | is.na(n))
  shifts$out <- out.of.bounds


  if (!all(out.of.bounds)) {

    # set coordinates to km
    os.e <- e[!out.of.bounds] / 1000
    os.n <- n[!out.of.bounds] / 1000

    east.km <- trunc(os.e)
    north.km <- trunc(os.n)

    # R 'lists' are 1-based (find which data records to use)
    ll <- .ostn.shifts[east.km + north.km * 701 + 1, , drop=FALSE]
    lr <- .ostn.shifts[east.km + north.km * 701 + 2, , drop=FALSE]
    ul <- .ostn.shifts[east.km + north.km * 701 + 702, , drop=FALSE]
    ur <- .ostn.shifts[east.km + north.km * 701 + 703, , drop=FALSE]

    t <- os.e - east.km
    u <- os.n - north.km

    one.t <- 1 - t
    one.u <- 1 - u
    dx <- (one.t * one.u * ll[, "e"]
           + t * one.u * lr[, "e"]
           + one.t * u * ul[, "e"]
           + t * u * ur[, "e"])
    dy <- (one.t * one.u * ll[, "n"]
           + t * one.u * lr[, "n"]
           + one.t * u * ul[, "n"]
           + t * u * ur[, "n"])

    shifts$dx[!out.of.bounds] <- dx
    shifts$dy[!out.of.bounds] <- dy

    if (z) {
      dz <- (one.t * one.u * ll[, "g"]
             + t * one.u * lr[, "g"]
             + one.t * u * ul[, "g"]
             + t * u * ur[, "g"])

      llf <- ll[, "f"]
      lrf <- lr[, "f"]
      ulf <- ul[, "f"]
      urf <- ur[, "f"]
      gf <- ifelse(llf == lrf & lrf == ulf & ulf == urf, llf #all equal
                 , ifelse(t <= 0.5 & u <= 0.5, llf #point in SW (or dead centre)
                 , ifelse(t > 0.5 & u <= 0.5, lrf #point in SE quadrant
                 , ifelse(t > 0.5 & u > 0.5, urf  #point in NE quadrant
                 , ulf))))
      shifts$dz[!out.of.bounds] <- dz
      shifts$gf[!out.of.bounds] <- gf
    }

  }

  return (shifts)

}
