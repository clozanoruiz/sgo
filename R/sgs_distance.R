#' @encoding UTF-8
#' @title Calculate distances between a set of points
#'
#' @description
#' Calculates the distance between OS National Grid Reference points or using
#' the Harvesine or Vicenty formulae for points with angular coordinates.
#'
#' @name sgs_distance
#' @usage sgs_distance(x, y, by.element = FALSE,
#'   which = ifelse(isTRUE(x$epsg==27700 || x$epsg==7405), "BNG", "Harvesine"))
#' @param x A \code{sgs_points} object describing a set of points in a geodetic
#' coordinate system.
#' @param y A \code{sgs_points} object, defaults to \code{x}.
#' @param by.element Logical variable. If \code{TRUE}, return a vector with
#' distance between the first elements of \code{x} and \code{y}, the second,
#' etc. If \code{FALSE}, return the dense matrix with all pairwise distances.
#' @param which Character vector. For geodetic coordinates one of
#' \code{Harvesine} or \code{Vicenty}. For points in OS British National Grid
#' coordinates it defaults to \code{BNG}.
#' @details
#' TODO.
#' @return
#' If \code{by.element} is \code{FALSE} \code{sgs_distance} returns a dense
#' numeric matrix of dimension length(x) by length(y). Otherwise it returns a
#' numeric vector of length \code{x} or \code{y}, the shorter one being
#' recycled. Distances involving empty geometries are \code{NA}.
#' @examples
#' #TODO
#' @export
sgs_distance <- function (x, y, by.element=FALSE,
  which = ifelse(isTRUE(x$epsg==27700 || x$epsg==7405), "BNG", "Harvesine"))
    UseMethod("sgs_distance")

#' @export
sgs_distance.sgs_points <- function(x, y, by.element=FALSE,
  which = ifelse(isTRUE(x$epsg==27700 || x$epsg==7405), "BNG", "Harvesine")) {

  #TODO
  #algorithms
  #great_circle harvesine
  #geodesic vicenty ellipsoidal
  #bng

  if (missing(y))
    y <- x

  if (x$epsg != y$epsg)
    stop("All points must have the same EPSG code")

  if(isTRUE(x$epsg==27700 || x$epsg==7405)) {
    #BNG
  } else {
    #harvesine, vicenty(vicenty can use any ellipsoid so accept 4258,4937;4326,4979;4277) iterations as a parameter
    coords <- c("x", "y")
    p1 <- as.matrix(x[, coords, drop=TRUE] /
                      57.29577951308232087679815481410517)
    p2 <- as.matrix(y[, coords, drop=TRUE] /
                      57.29577951308232087679815481410517)

    if (by.element) {

      if (which == "Harvesine") {
        great.circle.harvesine(p1, p2)
      } else if (which == "Vicenty") {
        vicenty.ellipsoid(p1, p2, x$datum, iterations)
      }

    } else {
      rows.p1 <- nrow(p1)
      rows.p2 <- nrow(p2)
      # Kronecker product of mathe matrices with vectors of 1's:
      # rep(1, nTimes) %x% mt would be 'similar' to rep(mt, times = nTimes)
      # mt %x% rep(1, nTimes) would be 'similar' to rep(mt, each = nTimes)
      m1 <- rep(1, rows.p2) %x% p1
      m2 <- p2 %x% rep(1, rows.p1)


      if (which == "Harvesine") {
        matrix(great.circle.harvesine(m1, m2), rows.p1, rows.p2)
      } else if (which == "Vicenty") {
        vicenty.ellipsoid(p1, p2, x$datum, iterations)
        matrix(vicenty.ellipsoid(m1, m2, x$datum, iterations), rows.p1, rows.p2)
      }
    }

  }

}

# Default R as defined by the International Union of Geodesy and Geophysics
# Harvesine error up to 0.5%
# p1 & p2 in rad
great.circle.harvesine <- function(p1, p2, R=6371008) {

  hav.dlat <- sin((p2[, 2] - p1[, 2]) / 2L)
  hav.dlon <- sin((p2[, 1] - p1[, 1]) / 2L)
  h <- hav.dlat * hav.dlat + cos(p1[, 2]) * cos(p2[, 2]) * hav.dlon * hav.dlon

  # https://en.wikipedia.org/wiki/Haversine_formula:
  # When using these formulae, one must ensure that h does not exceed 1 due to
  # a floating point error (d is only real for 0 ≤ h ≤ 1). h only approaches 1
  # for antipodal points (on opposite sides of the sphere)—in this region,
  # relatively large numerical errors tend to arise in the formula when finite
  # precision is used.
  h <- pmin(h, 1L)
  d <- 2L * R * asin(sqrt(h))
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

  # ellipsooid parameters
  ellipsoid <- lonlat.datum[lonlat.datum$datum==datum, "ellipsoid"]
  params <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid,
                             c("a","b","f")]
  a2 <- params$a * params$a
  b <- params$b
  b2 <- b * b
  f <- 1L / params$f

  one.f <- 1L - f
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
    cos2.alpha <- 1L - sin.alpha * sin.alpha
    cos.2sigma.m <- cos.sigma - 2 * sin.U1.U2 / cos2.alpha
    cos.2sigma.m[is.nan(cos.2sigma.m)] <- 0  # cos2.alpha = 0 (equatorial line)
    cos2.2sigma.m <- cos.2sigma.m * cos.2sigma.m

    C <- f / 16L * cos2.alpha * (4L + f * (4L - 3L * cos2.alpha))
    lambda.tmp <- lambda
    lambda <- dlon + (1L - C) * f * sin.alpha *
      (sigma + C * sin.sigma * (cos.2sigma.m + C * cos.sigma *
                                  (-1L + 2L * cos2.2sigma.m)))

    iterations <- iterations - 1L
    if ( max(abs(lambda - lambda.tmp)) <= 1e-12 || iterations == 0) {
      break # ie until <= 0.06mm
    }
  }
  if (iterations == 0)
    return(s)

  u2 <- cos2.alpha * (a2 - b2) / b2
  A <- 1L + u2 / 16384L * (4096L + u2 * (-768L + u2 * (320L - 175L * u2)))
  B <- u2 / 1024 * (256L + u2 * (-128L + u2 *(74L - 47L * u2)))
  delta.sigma <- B * sin.sigma * (cos.2sigma.m +
                    B/4L * (cos.sigma * (-1L + 2L * cos2.2sigma.m) -
                    B/6L * cos.2sigma.m * (-3L + 4L * sin.sigma * sin.sigma) *
                      (-3L + 4L * cos2.2sigma.m)))

  s <- unname(round(b * A * (sigma - delta.sigma), 3)) #round to mm
  s

}

#Areas:
#https://support.esri.com/en/technical-article/000006109
#https://en.wikipedia.org/wiki/Shoelace_formula
#https://www.johndcook.com/blog/2018/09/26/polygon-area/
#https://github.com/rspatial/raster/blob/19db58371dc37562a74f23830174da47c1e8b9b4/src/area.cpp#L58
#https://rdrr.io/cran/pracma/src/R/polyarea.R
