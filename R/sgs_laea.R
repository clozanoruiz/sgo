#' @encoding UTF-8
#' @title ETRS89 to ETRS89-LAEA Easting/Northing
#'
#' @description
#' Converts ETRS89 geodetic coordinates to ETRS89-LAEA Easting/Northing
#' (EPSG:3035)
#'
#' @name sgs_etrs_laea
#' @usage sgs_etrs_laea(x)
#' @param x A \code{sgs_points} object describing a set of points in the
#' geodetic coordinate system EPSG=4258, 4937 or 4936.
#' @details
#' ETRS89-LAEA (EPSG:3035) is a CRS for pan-European statistical mapping at all
#' scales or other purposes where true area representation is required.
#' @return
#' An object of class \code{sgs_points} whose coordinates are defined as
#' Easting/Northing in the EPSG:3035 Projected Coordinate System.
#' @references
#' IOGP Publication 373-7-2 - Geomatics Guidance Note number 7,
#' part 2 (October 2020) \url{https://epsg.org/guidance-notes.html}
#' @seealso \code{\link{sgs_points}}, \code{\link{sgs_area}}.
#' @examples
#' p <- sgs_points(list(-3.9369, 56.1165), epsg=4258)
#' prj <- sgs_etrs_laea(p)
#' @export
sgs_etrs_laea <- function(x) UseMethod("sgs_etrs_laea")

#' @export
sgs_etrs_laea.sgs_points <- function(x) {

  if (!x$epsg %in% c(4258, 4937, 4936))
    stop("This routine only supports ETRS89 coordinates.")

  if (x$epsg == 4936)
    x <- sgs_cart_lonlat(x)

  core.cols <- sgs_points.core
  additional.elements <- !names(x) %in% core.cols
  num.elements <- sum(additional.elements, na.rm=TRUE)

  ellipsoid <- lonlat.datum[lonlat.datum$datum==x$datum, "ellipsoid"]
  params <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid,
                             c("a","e2")]
  a <- params$a
  e2 <- params$e2
  e <- sqrt(e2)

  FE <- (4321000); FN <- (3210000) # false easting and northing
  phi0 <- 52 / RAD.TO.GRAD
  lambda0 <- 10 / RAD.TO.GRAD

  phi <- x$y / RAD.TO.GRAD
  lambda <- x$x / RAD.TO.GRAD

  lambda.delta <- lambda - lambda0
  cos.lambda.delta <- cos(lambda.delta)

  sin.phi <- sin(phi)
  sin2.phi <- sin.phi * sin.phi

  sin.phi0 <- sin(phi0)
  sin2.phi0 <- sin.phi0 * sin.phi0
  splat0 <- 1 - e2 * sin2.phi0

  q.phi <- (1 - e2) * (sin.phi / (1 - e2 * sin2.phi) - 1/(2 * e) *
                         log((1 - e * sin.phi) / (1 + e * sin.phi)))
  q.phi0 <- (1 - e2) * (sin.phi0 / splat0 - 1/(2 * e) *
                         log((1 - e * sin.phi0) / (1 + e * sin.phi0)))
  #phi.p = π/2 rad, thus sin(phi.p) = 1
  q.phi.p <- (1 - e2) * (1 / (1-e2) - 1/(2 * e) * log((1 - e ) / (1 + e)))

  beta <- asin(q.phi / q.phi.p)
  beta0 <- asin(q.phi0 / q.phi.p)
  cos.beta <- cos(beta)
  cos.beta0 <- cos(beta0)
  sin.beta <- sin(beta)
  sin.beta0 <- sin(beta0)

  Rq <- a * sqrt(q.phi.p / 2)
  D <- a * (cos(phi0) / sqrt(splat0)) / (Rq * cos.beta0)
  B <- Rq * sqrt(2 / (1 + sin.beta0 * sin.beta +
                        (cos.beta0 * cos.beta * cos.lambda.delta)))

  E <- FE + B * D * cos.beta * sin(lambda.delta)
  N <- FN + (B / D) *
    (cos.beta0 * sin.beta - sin.beta0 * cos.beta * cos.lambda.delta)

  # Return values
  en <- list(x=E, y=N)
  if (num.elements > 0) en <- c(x[, additional.elements, drop=TRUE], en)

  sgs_points(en, coords=c("x", "y"), epsg=3035)

}


#' @encoding UTF-8
#' @title ETRS89-LAEA Easting/Northing to ETRS89 geodetic coordinates
#'
#' @description
#' Converts ETRS89-LAEA Easting/Northing to ETRS89 geodetic coordinates
#' (EPSG:4258)
#'
#' @name sgs_laea_etrs
#' @usage sgs_laea_etrs(x)
#' @param x A \code{sgs_points} object describing a set of points in the
#' projected coordinate system EPSG=3035.
#' @details
#' ETRS89-LAEA (EPSG:3035) is a CRS for pan-European statistical mapping at all
#' scales or other purposes where true area representation is required.
#' @return
#' An object of class \code{sgs_points} whose coordinates are defined as
#' Longitude/Latitude in the ETRS89 Coordinate Reference System.
#' @references IOGP Publication 373-7-2 - Geomatics Guidance Note number 7,
#' part 2 (October 2020) \url{https://epsg.org/guidance-notes.html}
#' @seealso \code{\link{sgs_points}}, \code{\link{sgs_area}}.
#' @examples
#' prj <- sgs_points(list(3962799.45, 2999718.85), epsg=3035)
#' p <- sgs_laea_etrs(prj)
#' @export
sgs_laea_etrs <- function(x) UseMethod("sgs_laea_etrs")

#' @export
sgs_laea_etrs.sgs_points <- function(x) {

  if (x$epsg != 3035)
    stop("This routine only supports coordinates in EPSG:3035.")

  core.cols <- sgs_points.core
  additional.elements <- !names(x) %in% core.cols
  num.elements <- sum(additional.elements, na.rm=TRUE)

  ellipsoid <- lonlat.datum[lonlat.datum$datum==x$datum, "ellipsoid"]
  params <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid,
                             c("a","e2")]
  a <- params$a
  e2 <- params$e2
  e <- sqrt(e2)
  e4 <- e2 * e2
  e6 <- e4 * e2

  FE <- (4321000); FN <- (3210000) # false easting and northing
  phi0 <- 52 / RAD.TO.GRAD
  lambda0 <- 10 / RAD.TO.GRAD

  E <- x$x
  N <- x$y
  E.delta <- E - FE
  N.delta <- N - FN

  sin.phi0 <- sin(phi0)
  sin2.phi0 <- sin.phi0 * sin.phi0
  splat0 <- 1 - e2 * sin2.phi0

  q.phi0 <- (1 - e2) * (sin.phi0 / splat0 - 1/(2 * e) *
                          log((1 - e * sin.phi0) / (1 + e * sin.phi0)))
  #phi.p = π/2 rad, thus sin(phi.p) = 1
  q.phi.p <- (1 - e2) * (1 / (1-e2) - 1/(2 * e) * log((1 - e ) / (1 + e)))

  beta0 <- asin(q.phi0 / q.phi.p)
  cos.beta0 <- cos(beta0)
  sin.beta0 <- sin(beta0)

  Rq <- a * sqrt(q.phi.p / 2)
  D <- a * (cos(phi0) / sqrt(splat0)) / (Rq * cos.beta0)

  Dtimes.N.delta <- D * N.delta
  E.delta.divD <- E.delta / D

  rho <- sqrt(E.delta.divD * E.delta.divD + Dtimes.N.delta * Dtimes.N.delta)
  C <- 2 * asin(rho / (2 * Rq))
  sin.C <- sin(C)
  cos.C <- cos(C)

  beta.prime <- asin(cos.C * sin.beta0 +
                       ((Dtimes.N.delta * sin.C * cos.beta0) / rho))

  lambda <- lambda0 + atan2(E.delta * sin.C,
                            (D * rho * cos.beta0 * cos.C -
                               D * Dtimes.N.delta * sin.beta0 * sin.C))
  phi <- beta.prime +
    (e2 / 3 + 31 * e4 / 180 + 517 * e6 / 5040) * sin(2 * beta.prime) +
    (23 * e4 / 360 + 251 * e6 / 3780) * sin(4 * beta.prime) +
    (761 * e6 / 45360) * sin(6 * beta.prime)

  # Round and Return
  xy <- list(x=round(lambda * RAD.TO.GRAD, 7),
             y=round(phi * RAD.TO.GRAD, 7))
  if (num.elements > 0) xy <- c(x[, additional.elements, drop=TRUE],xy)

  sgs_points(xy, coords=c("x", "y"), epsg=4258)

}
