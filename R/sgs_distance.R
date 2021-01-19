#' @encoding UTF-8
#' @title Calculate distance(s) between points
#'
#' @description
#' Calculates the distance between OS National Grid Reference points or using
#' the Harvesine or Vicenty formulae for points with angular coordinates.
#'
#' @name sgs_distance
#' @usage sgs_distance(x, y, by.element = FALSE,
#'   which = ifelse(isTRUE(x$epsg==27700 || x$epsg==7405), "BNG", "Harvesine"),
#'   grid.distance = ifelse(isTRUE(x$epsg==27700 || x$epsg==7405), TRUE, FALSE),
#'   iterations = 20L)
#' @param x A \code{sgs_points} object describing a set of points in a geodetic
#' coordinate system.
#' @param y A \code{sgs_points} object, defaults to \code{x}.
#' @param by.element Logical variable. If \code{TRUE}, return a vector with
#' distance between the first elements of \code{x} and \code{y}, the second,
#' etc. If \code{FALSE}, return the dense matrix with all pairwise distances.
#' @param which Character vector. For geodetic coordinates one of
#' \code{Harvesine} or \code{Vicenty}. For points in OS British National Grid
#' coordinates it defaults to \code{BNG}.
#' @param grid.distance Logical variable. Currently only used for BNG
#' coordinates. If TRUE it returns the true (geodesic) distance.
#' @param iterations Numeric variable. Maximum number of iterations used in the
#' Vicenty method.
#' @details
#' TODO. fro grid.distance explain how corrrection factors work
#' @return
#' If \code{by.element} is \code{FALSE} \code{sgs_distance} returns a dense
#' numeric matrix of dimension length(x) by length(y). Otherwise it returns a
#' numeric vector of length \code{x} or \code{y}, the shorter one being
#' recycled. Distances involving empty geometries are \code{NA}.
#' @examples
#' #TODO
#' @export
sgs_distance <- function (x, y, by.element=FALSE,
  which = ifelse(isTRUE(x$epsg==27700 || x$epsg==7405), "BNG", "Harvesine"),
  grid.distance = ifelse(isTRUE(x$epsg==27700 || x$epsg==7405), TRUE, FALSE),
  iterations = 20L)
    UseMethod("sgs_distance")

#' @export
sgs_distance.sgs_points <- function(x, y, by.element=FALSE,
  which = ifelse(isTRUE(x$epsg==27700 || x$epsg==7405), "BNG", "Harvesine"),
  grid.distance = ifelse(isTRUE(x$epsg==27700 || x$epsg==7405), TRUE, FALSE),
  iterations = 20L) {

  if (missing(y))
    y <- x

  if (x$epsg != y$epsg)
    stop("All points must have the same EPSG code")

  if (isTRUE(x$epsg %in% c(4936, 3035, 4978, 3857)))
    stop("This function doesn't support the input's EPSG")

  default.simpson <- 20 #20 km
  coords <- c("x", "y")
  if(isTRUE(x$epsg==27700 || x$epsg==7405)) {

    p1 <- as.matrix(x[, coords, drop=TRUE])
    p2 <- as.matrix(y[, coords, drop=TRUE])

    if (by.element) {
      bng.distance(p1, p2, grid.distance, default.simpson)
    } else {
      rows.p1 <- nrow(p1)
      rows.p2 <- nrow(p2)

      # Kronecker product of our matrices with vectors of 1's:
      # rep(1, nTimes) %x% mt would be 'similar' to rep(mt, times = nTimes)
      # mt %x% rep(1, nTimes) would be 'similar' to rep(mt, each = nTimes)
      m1 <- rep(1, rows.p2) %x% p1
      m2 <- p2 %x% rep(1, rows.p1)

      matrix(bng.distance(m1, m2, grid.distance, default.simpson),
             rows.p1, rows.p2)
    }

  } else {

    p1 <- as.matrix(x[, coords, drop=TRUE] / RAD.TO.GRAD)
    p2 <- as.matrix(y[, coords, drop=TRUE] / RAD.TO.GRAD)

    if (by.element) {

      if (which == "Harvesine") {
        great.circle.harvesine(p1, p2)
      } else if (which == "Vicenty") {
        vicenty.ellipsoid(p1, p2, x$datum, iterations)
      }

    } else {
      rows.p1 <- nrow(p1)
      rows.p2 <- nrow(p2)

      m1 <- rep(1, rows.p2) %x% p1
      m2 <- p2 %x% rep(1, rows.p1)

      if (which == "Harvesine") {
        matrix(great.circle.harvesine(m1, m2), rows.p1, rows.p2)
      } else if (which == "Vicenty") {
        matrix(vicenty.ellipsoid(m1, m2, x$datum, iterations), rows.p1, rows.p2)
      }
    }

  }

}

# parametres:
# dist.simpson: with distances (in km) greater than those will apply simpson rule when caclulating true (geodesic) distances
bng.distance <- function(p1, p2, grid.distance = TRUE, dist.simpson = 20) {

  E1 <- p1[, 1]; E2 <- p2[, 1]
  N1 <- p1[, 2]; N2 <- p2[, 2]
  Em <- E1 + (E2 - E1) / 2 # E at midpoint
  Nm <- N1 + (N2 - N1) / 2 # N at midpoint

  dE <- E2 - E1
  dN <- N2 - N1
  s <- sqrt(dE * dE + dN * dN)

  if (grid.distance)
    return(round(s, 3)) # round to mm

  if (any(s >= (dist.simpson * 1000))) {
    Ft <- local.scale.factor(c(E1, E2, Em), c(N1, N2, Nm))
    len.E1 <- length(E1) # E1, E2, Em have the same length
    F1 <- Ft[(1:len.E1)]
    F2 <- Ft[length(Ft) - (len.E1:0)]
    Fm <- Ft[((len.E1 + 1):(len.E1 * 2))]
    F <- (F1 + 4 * Fm + F2) / 6
  } else {
    #F for mid point only
    F <- local.scale.factor(Em, Nm)
  }

  round(s / F, 3) # S (true distance)

}

local.scale.factor <- function(E, N) {

  # ellipsoid parameters
  params <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid=="Airy1830",
                             c("a","b","e2")]
  a <- params$a
  b <- params$b
  e2 <- params$e2
  F0 <- 0.9996012717 #Central meridian scale factor
  aF0 <- a * F0
  bF0 <- b * F0
  n <- (a-b) / (a+b)
  N0 <- -100000; E0 <- 400000 # True origin
  phi0 <- 49 / RAD.TO.GRAD

  # Initial latitude φ'
  dN <- N - N0
  phi <- phi0 + dN/aF0

  M <- NA
  phi.plus <- NA
  phi.minus <- NA
  repeat {
    phi.minus <- phi - phi0
    phi.plus <- phi + phi0

    M <- bF0 * (
      (1 + n * (1 + 5/4 * n * (1 + n))) * phi.minus
      - 3 * n * (1 + n * (1 + 7 / 8 * n)) * sin(phi.minus) * cos(phi.plus)
      + (15 / 8 * n * (n * (1 + n))) * sin(2 * phi.minus) * cos(2 * phi.plus)
      - 35 / 24 * n^3 * sin(3 * phi.minus) * cos(3 * phi.plus)
    ) # meridional arc

    if ( max(abs(dN - M)) < 0.00001 ) { break } # ie until < 0.01mm
    phi <- phi + (dN - M) / aF0
  }

  cos.phi <- cos(phi)
  sin.phi <- sin(phi)

  splat <- 1 - e2 * sin.phi * sin.phi
  sqrtsplat <- sqrt(splat)

  # radius of curvature at latitude φ perpendicular to a meridian
  nu <- aF0 / sqrtsplat
  # radius of curvature of a meridian at latitude φ
  rho <- aF0 * (1 - e2) / (splat * sqrtsplat)
  # East - west component of the deviation of the vertical, squared
  eta2 <- nu / rho - 1

  XXI <- 1 / (2 * rho * nu)
  XXII <- (1 + 4 * eta2 * eta2) / (24 * rho * rho * nu * nu)

  dE <- E - E0
  dE2 <- dE * dE
  F <- F0 * (1 + dE2 * XXI + dE2 * dE2 * XXII)

}


# Default R as defined by the International Union of Geodesy and Geophysics
# Harvesine error up to 0.5%
# p1 & p2 in rad
great.circle.harvesine <- function(p1, p2, R=6371008) {

  hav.dlat <- sin((p2[, 2] - p1[, 2]) / 2)
  hav.dlon <- sin((p2[, 1] - p1[, 1]) / 2)
  h <- hav.dlat * hav.dlat + cos(p1[, 2]) * cos(p2[, 2]) * hav.dlon * hav.dlon

  # https://en.wikipedia.org/wiki/Haversine_formula:
  # When using these formulae, one must ensure that h does not exceed 1 due to
  # a floating point error (d is only real for 0 ≤ h ≤ 1). h only approaches 1
  # for antipodal points (on opposite sides of the sphere)—in this region,
  # relatively large numerical errors tend to arise in the formula when finite
  # precision is used.
  h <- pmin(h, 1)
  d <- 2 * R * asin(sqrt(h))
  round(d, 3) #round to mm (problaby shouldn't expect accuracy greater than m)

}
#example: antipodal points:
#p1 <- sgs_points(list(-177.5,-5.5),epsg=4326)
#p2 <- sgs_points(list(2.5,5.5),epsg=4326)
#res <- great.circle.harvesine(p1,p2)
#res: 20015112

#p1 <- sgs_points(list(0,0),epsg=4326)
#p2 <- sgs_points(list(90,90),epsg=4326)
#res <- great.circle.harvesine(p1,p2)
#res 10007556

# Vicenty (inverse) iterative method to computes the geographical distance
# between two given points.
# It is accurate to within 0.5mm on the Earth ellipsoid.
# https://en.wikipedia.org/wiki/Vincenty's_formulae
# p1 & p2 in rad
vicenty.ellipsoid <- function(p1, p2, datum, iterations = 20L) {

  #check first for NA points and co-incident points?

  # initialise distances
  s <- rep(NA, nrow(p1))

  # ellipsoid parameters
  ellipsoid <- lonlat.datum[lonlat.datum$datum==datum, "ellipsoid"]
  params <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid,
                             c("a","b","f")]
  a2 <- params$a * params$a
  b <- params$b
  b2 <- b * b
  f <- 1 / params$f

  one.f <- 1 - f
  dlon <- (p2[, 1] - p1[, 1])     # difference of longitudes
  U1 <- atan(one.f * tan(p1[, 2])) # reduced latitude
  U2 <- atan(one.f * tan(p2[, 2]))

  sin.U1 <- sin(U1)
  sin.U2 <- sin(U2)
  sin.U1.U2 <- sin.U1 * sin.U2
  cos.U1 <- cos(U1)
  cos.U2 <- cos(U2)
  cos.U1.U2 <- cos.U1 * cos.U2

  lambda <- dlon
  repeat {
    sin.lambda <- sin(lambda)
    cos.lambda <- cos(lambda)
    sin.sigma.1 <- cos.U2 * sin.lambda
    sin.sigma.2 <- cos.U1 * sin.U2 - sin.U1 * cos.U2 * cos.lambda
    sin.sigma <- sqrt(sin.sigma.1 * sin.sigma.1 + sin.sigma.2 * sin.sigma.2)
    cos.sigma <- sin.U1.U2 + cos.U1.U2 * cos.lambda
    sigma <- atan2(sin.sigma, cos.sigma)

    sin.alpha <- cos.U1.U2 * sin.lambda / sin.sigma
    cos2.alpha <- 1 - sin.alpha * sin.alpha
    cos.2sigma.m <- cos.sigma - 2 * sin.U1.U2 / cos2.alpha
    cos.2sigma.m[is.nan(cos.2sigma.m)] <- 0  # cos2.alpha = 0 (equatorial line)
    cos2.2sigma.m <- cos.2sigma.m * cos.2sigma.m

    C <- f / 16 * cos2.alpha * (4 + f * (4 - 3 * cos2.alpha))
    lambda.tmp <- lambda
    lambda <- dlon + (1 - C) * f * sin.alpha *
      (sigma + C * sin.sigma * (cos.2sigma.m + C * cos.sigma *
                                  (-1 + 2 * cos2.2sigma.m)))

    iterations <- iterations - 1L
    if ( max(abs(lambda - lambda.tmp)) <= 1e-12 || iterations == 0L) {
      break # ie until <= 0.06mm
    }
  }
  if (iterations == 0L)
    return(s)

  u2 <- cos2.alpha * (a2 - b2) / b2
  A <- 1 + u2 / 16384 * (4096 + u2 * (-768 + u2 * (320 - 175 * u2)))
  B <- u2 / 1024 * (256 + u2 * (-128 + u2 *(74 - 47 * u2)))
  delta.sigma <- B * sin.sigma * (cos.2sigma.m +
                    B/4 * (cos.sigma * (-1 + 2 * cos2.2sigma.m) -
                    B/6 * cos.2sigma.m * (-3 + 4 * sin.sigma * sin.sigma) *
                      (-3 + 4 * cos2.2sigma.m)))

  s <- unname(b * A * (sigma - delta.sigma)) #round to mm
  round(s, 3) #round to mm

}
