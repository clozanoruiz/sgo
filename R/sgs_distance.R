#' @encoding UTF-8
#' @title Calculate distance(s) between points
#'
#' @description
#' Calculates the distance between OS National Grid Reference points or using
#' the Harvesine or Vicenty formulae for points with angular coordinates.
#'
#' @name sgs_distance
#' @usage sgs_distance(x, y, by.element = FALSE,
#'   which = ifelse(isTRUE(x$epsg==27700 || x$epsg==7405), "BNG", "Vicenty"),
#'   grid.true.distance = ifelse(isTRUE(x$epsg==27700 || x$epsg==7405),
#'   TRUE, FALSE), iterations = 100L)
#' @param x A \code{sgs_points} object describing a set of points in a geodetic
#' coordinate system.
#' @param y A \code{sgs_points} object, defaults to \code{x}.
#' @param by.element Logical variable. If \code{TRUE}, return a vector with
#' distance between the first elements of \code{x} and \code{y}, the second,
#' etc. If \code{FALSE}, return the dense matrix with all pairwise distances.
#' @param which Character vector. For geodetic coordinates one of
#' \code{Harvesine} or \code{Vicenty}. It defaults to \code{BNG} for points in
#' 'OS British National Grid' coordinates.
#' @param grid.true.distance Logical variable. Currently only used for BNG
#' coordinates. If \code{TRUE} it returns the true (geodesic) distance.
#' @param iterations Numeric variable. Maximum number of iterations used in the
#' Vicenty method.
#' @details
#' This function can use two different methods when working with geodetic
#' coordinates: When \code{which = "Vicenty"} the Vincenty's formula is used to
#' calculate the geodesics (distance) on an ellipsoid to an accuracy of up to
#' a millimetre. If such accuracy is not needed, \code{which} can also
#' accept the string "Harvesine" which calculates great-circle distance between
#' two points on a sphere. Harvesines are faster to compute than the Vicenty
#' dsitances but can result in an error of up to 0.5\%.
#'
#' When working with planar coordinates the Local Scale Factor is the scale
#' distortion inherent in the map projection at a point. When
#' \code{grid.true.distance} is \code{FALSE} the Euclidean distance in the
#' plane is calculated. When it is \code{TRUE} the function computes a line
#' scale factor using Simpson's Rule to achieve greater accuracy and
#' approximate the distance to the true geodesic distance.
#' @return
#' If \code{by.element} is \code{FALSE} \code{sgs_distance} returns a dense
#' numeric matrix of dimension length(x) by length(y). Otherwise it returns a
#' numeric vector of length \code{x} or \code{y}, the shorter one being
#' recycled. Distances involving empty geometries are \code{NA}.
#' All distances are returned in metres.
#' @references
#' Thaddeus Vincenty, 1975. \emph{Direct and Inverse Solutions of Geodesics on
#' the Ellipsoid with application of nested equations}. Survey Review, 23:176,
#' 88-93, DOI: 10.1179/sre.1975.23.176.88
#' @examples
#' p1 <- sgs_points(list(-3.9369, 56.1165), epsg=4326)
#' lon <- c(-4.25181,-3.18827)
#' lat <- c(55.86424, 55.95325)
#' pts <- sgs_points(list(longitude=lon, latitude=lat), epsg=4326)
#' p1.to.pts <- sgs_distance(p1, pts, by.element = TRUE)
#'
#' ## Perimeter of a polygon defined as a series of ordered points:
#' lon <- c(-6.43698696, -6.43166843, -6.42706831, -6.42102546,
#' -6.42248238, -6.42639092, -6.42998435, -6.43321409)
#' lat <- c(58.21740316, 58.21930597, 58.22014035, 58.22034112,
#' 58.21849188, 58.21853606, 58.21824033, 58.21748949)
#' pol <- sgs_points(list(lon, lat), epsg=4326)
#' # Create a copy of the polygon with its coordinates shifted one
#' # position so that we can calculate easily the distance between vertices
#' coords <- sgs_coordinates(pol)
#' pol.shift.one <- sgs_points(rbind(coords[-1, ], coords[1, ]), epsg=pol$epsg)
#' perimeter <- sum(sgs_distance(pol, pol.shift.one, by.element=TRUE))
#' @export
sgs_distance <- function (x, y, by.element=FALSE,
  which = ifelse(isTRUE(x$epsg==27700 || x$epsg==7405), "BNG", "Vicenty"),
  grid.true.distance = ifelse(isTRUE(x$epsg==27700 || x$epsg==7405),
                              TRUE, FALSE), iterations = 100L)
    UseMethod("sgs_distance")

#' @export
sgs_distance.sgs_points <- function(x, y, by.element=FALSE,
  which = ifelse(isTRUE(x$epsg==27700 || x$epsg==7405), "BNG", "Vicenty"),
  grid.true.distance = ifelse(isTRUE(x$epsg==27700 || x$epsg==7405),
                              TRUE, FALSE), iterations = 100L) {

  if (missing(y))
    y <- x

  if (x$epsg != y$epsg)
    stop("All points must have the same EPSG code")

  if (isTRUE(x$epsg %in% c(4936, 3035, 4978, 3857)))
    stop("This function doesn't support the input's EPSG")

  default.simpson <- 20 #20 km
  coords <- .sgs_points.2d.coords
  if(isTRUE(x$epsg==27700 || x$epsg==7405)) {

    p1 <- matrix(unlist(x[coords], use.names = FALSE), ncol = 2, byrow = FALSE)
    p2 <- matrix(unlist(y[coords], use.names = FALSE), ncol = 2, byrow = FALSE)

    if (by.element) {
      .bng.distance(p1, p2, grid.true.distance, default.simpson)
    } else {
      rows.p1 <- nrow(p1)
      rows.p2 <- nrow(p2)

      # Kronecker product of our matrices with vectors of 1's:
      # rep(1, nTimes) %x% mt would be 'similar' to rep(mt, times = nTimes)
      # mt %x% rep(1, nTimes) would be 'similar' to rep(mt, each = nTimes)
      m1 <- rep(1, rows.p2) %x% p1
      m2 <- p2 %x% rep(1, rows.p1)

      matrix(.bng.distance(m1, m2, grid.true.distance, default.simpson),
             rows.p1, rows.p2)
    }

  } else {

    p1 <- matrix(unlist(x[coords], use.names = FALSE),
                 ncol = 2, byrow = FALSE) / RAD.TO.GRAD
    p2 <- matrix(unlist(y[coords], use.names = FALSE),
             ncol = 2, byrow = FALSE) / RAD.TO.GRAD

    if (by.element) {

      if (which == "Harvesine") {
        .great.circle.harvesine(p1, p2)
      } else if (which == "Vicenty") {
        .inverse.vicenty.ellipsoid(p1, p2, x$datum, iterations)$distance
      }

    } else {
      rows.p1 <- nrow(p1)
      rows.p2 <- nrow(p2)

      m1 <- rep(1, rows.p2) %x% p1
      m2 <- p2 %x% rep(1, rows.p1)

      if (which == "Harvesine") {
        matrix(.great.circle.harvesine(m1, m2), rows.p1, rows.p2)
      } else if (which == "Vicenty") {
        matrix(.inverse.vicenty.ellipsoid(m1, m2, x$datum, iterations)$distance,
               rows.p1, rows.p2)
      }
    }

  }

}

#' @noRd
#' @param p1 A matrix of coordinates
#' @param p2 A matrix of coordinates
#' @param grid.true.distance Logical value
#' @param dist.simpson: distance (in km) greater than this will apply Simpson's
#' Rule when calculating true (geodesic) distances
.bng.distance <- function(p1, p2, grid.true.distance = TRUE,
                          dist.simpson = 20) {

  E1 <- p1[, 1]; E2 <- p2[, 1]
  N1 <- p1[, 2]; N2 <- p2[, 2]

  dE <- E2 - E1
  dN <- N2 - N1
  s <- sqrt(dE * dE + dN * dN)

  if (!grid.true.distance)
    return(round(s, 3)) # round to mm

  Em <- E1 + (E2 - E1) / 2 # E at midpoint
  Nm <- N1 + (N2 - N1) / 2 # N at midpoint
  if (any(s >= (dist.simpson * 1000))) {
    Ft <- .local.scale.factor(c(E1, E2, Em), c(N1, N2, Nm))
    len.E1 <- length(E1) # E1, E2, Em have the same length
    F1 <- Ft[(1:len.E1)]
    F2 <- Ft[length(Ft) - (len.E1:0)]
    Fm <- Ft[((len.E1 + 1):(len.E1 * 2))]
    F <- (F1 + 4 * Fm + F2) / 6
  } else {
    #F for mid point only
    F <- .local.scale.factor(Em, Nm)
  }

  round(s / F, 3) # S (true distance)

}

#' @noRd
#' @param E Vector of Easting coordinates
#' @param N Vector of Northing coordinates
.local.scale.factor <- function(E, N) {

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


#' @noRd
#' @param p1 A matrix of coordinates in radians
#' @param p2 A matrix of coordinates in radians
#' @param R Default defined by the International Union of Geodesy and Geophysics
.great.circle.harvesine <- function(p1, p2, R=6371008) {

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
#TODO in tests: antipodal points:
#p1 <- sgs_points(list(-177.5,-5.5),epsg=4326)
#p2 <- sgs_points(list(2.5,5.5),epsg=4326)
#res <- .great.circle.harvesine(p1,p2)
#res: 20015112

#p1 <- sgs_points(list(0,0),epsg=4326)
#p2 <- sgs_points(list(90,90),epsg=4326)
#res <- .great.circle.harvesine(p1,p2)
#res 10007556


# Vicenty (inverse) iterative method to compute the geographical distance
# between two given points.
#' @noRd
#' @param p1 A matrix of coordinates in radians
#' @param p2 A matrix of coordinates in radians
#' @param datum A string containing "OSGB36", "WGS84" or "ETRS89"
#' @param iterations Scalar value. Number of iterations to reach convergence
.inverse.vicenty.ellipsoid <- function(p1, p2, datum, iterations = 100L) {

  # ellipsoid parameters
  ellipsoid <- lonlat.datum[lonlat.datum$datum==datum, "ellipsoid"]
  params <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid,
                             c("a","b","f")]
  a2 <- params$a * params$a
  b <- params$b
  b2 <- b * b
  f <- 1 / params$f

  one.f <- 1 - f
  dlon <- (p2[, 1] - p1[, 1])    # difference of longitudes
  tan.U1 <- one.f * tan(p1[, 2]) # tan(reduced latitude)
  tan.U2 <- one.f * tan(p2[, 2])

  cos.U1 <- 1 / sqrt(1 + tan.U1 * tan.U1)
  cos.U2 <- 1 / sqrt(1 + tan.U2 * tan.U2)
  sin.U1 <- tan.U1 * cos.U1
  sin.U2 <- tan.U2 * cos.U2
  sin.U1.U2 <- sin.U1 * sin.U2

  cos.U1.U2 <- cos.U1 * cos.U2

  lambda <- dlon
  no.convergence <- FALSE
  repeat {
    sin.lambda <- sin(lambda)
    cos.lambda <- cos(lambda)
    sin.sigma.1 <- cos.U2 * sin.lambda
    sin.sigma.2 <- cos.U1 * sin.U2 - sin.U1 * cos.U2 * cos.lambda
    sin2.sigma <- sin.sigma.1 * sin.sigma.1 + sin.sigma.2 * sin.sigma.2
    sin.sigma <- sqrt(sin2.sigma)
    cos.sigma <- sin.U1.U2 + cos.U1.U2 * cos.lambda
    sigma <- atan2(sin.sigma, cos.sigma)

    sin.alpha <- cos.U1.U2 * sin.lambda / sin.sigma
    coincident.points <- is.nan(sin.alpha) # or antipodal (from sin.sigma=0)
    sin.alpha[coincident.points] <- 0
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
  if (iterations == 0L) {
    no.convergence <- abs(lambda - lambda.tmp) > 1e-12
  }

  u2 <- cos2.alpha * (a2 - b2) / b2
  A <- 1 + u2 / 16384 * (4096 + u2 * (-768 + u2 * (320 - 175 * u2)))
  B <- u2 / 1024 * (256 + u2 * (-128 + u2 *(74 - 47 * u2)))
  delta.sigma <- B * sin.sigma * (cos.2sigma.m +
                    B/4 * (cos.sigma * (-1 + 2 * cos2.2sigma.m) -
                    B/6 * cos.2sigma.m * (-3 + 4 * sin.sigma * sin.sigma) *
                      (-3 + 4 * cos2.2sigma.m)))

  # alpha1, alpha2: azimuths of the geodesic, clockwise from north
  # alpha2 in the direction p1 p2 produced
  alpha1 <- atan2(cos.U2 * sin.lambda,
                  cos.U1 * sin.U2 - sin.U1 * cos.U2 * cos.lambda)
  alpha2 <- atan2(cos.U1 * sin.lambda,
                  -sin.U1 * cos.U2 + cos.U1 * sin.U2 * cos.lambda)

  # From Chris Veness (https://www.movable-type.co.uk):
  # Special handling of exactly antipodal points where sin2.sigma = 0
  # (due to discontinuity atan2(0, 0) = 0 but atan2(NaN, 0) = PI/2 / 90°) - in
  # which case bearing is always meridional, due north (or due south!)
  alpha1[coincident.points] <- 0
  alpha2[coincident.points] <- PI

  s <- round(unname(b * A * (sigma - delta.sigma)), 3) #round to mm

  if (any(no.convergence)) {
    s[no.convergence] <- NaN
    alpha1[no.convergence] <- NaN
    alpha2[no.convergence] <- NaN
    warning("Vicenty formula failed to converge. Check your results.")
  }

  list(distance=s, initial.bearing=alpha1, final.bearing=alpha2)

}
#examples or better in tests TODO:
#nearly antipodal points may need a higher number of iterations to converge
#new.zealand <- sgs_points(list(174.35, -35.76),epsg=4326)
#gibraltar <- sgs_points(list(-5.35, 36.13),epsg=4326)
#sgs_distance(new.zealand, gibraltar, which="Vicenty", iterations=300)
#TODO: test how it behaves with coincident points


# Vicenty (direct) iterative method to compute a destination point from a given
# point and an initial bearing (both in radians). 's' (distance) in m.
#' @noRd
#' @param p1 A matrix of coordinates in radians
#' @param s A numeric vector. Distances (to compute the destination point)
#' @param alpha1 A numeric vector. Initial bearing in radians
#' @param datum A string containing "OSGB36", "WGS84" or "ETRS89"
#' @param iterations Scalar value. Number of iterations to reach convergence
.direct.vicenty.ellipsoid <- function(p1, s, alpha1, datum, iterations = 100L) {

  # ellipsoid parameters
  ellipsoid <- lonlat.datum[lonlat.datum$datum==datum, "ellipsoid"]
  params <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid,
                             c("a","b","f")]

  a2 <- params$a * params$a
  b <- params$b
  b2 <- b * b
  f <- 1 / params$f
  one.f <- 1 - f

  tan.U1 <- one.f * tan(p1[, 2])
  cos.U1 <- 1 / sqrt(1 + tan.U1 * tan.U1)
  sin.U1 <- tan.U1 * cos.U1

  sin.alpha1 <- sin(alpha1)
  cos.alpha1 <- cos(alpha1)
  sigma1 <- atan2(tan.U1, cos.alpha1)
  sin.alpha <- cos.U1 * sin.alpha1
  cos2.alpha <- 1 - sin.alpha * sin.alpha

  u2 <- cos2.alpha * (a2 - b2) / b2
  A <- 1 + u2 / 16384 * (4096 + u2 * (-768 + u2 * (320 - 175 * u2)))
  B <- u2 / 1024 * (256 + u2 * (-128 + u2 *(74 - 47 * u2)))

  sigma <- s / (b * A)
  no.convergence <- FALSE
  repeat {
    cos.2sigma.m <- cos(2 * sigma1 + sigma)
    cos2.2sigma.m <- cos.2sigma.m * cos.2sigma.m
    sin.sigma <- sin(sigma)
    cos.sigma <- cos(sigma)

    delta.sigma <- B * sin.sigma * (cos.2sigma.m + B/4 *
                     (cos.sigma * (-1 + 2 * cos2.2sigma.m) -
                        B/6 * cos.2sigma.m * (-3 + 4 * sin.sigma * sin.sigma) *
                        (-3 + 4 * cos2.2sigma.m)))
    sigma.tmp <- sigma
    sigma <- s / (b * A) + delta.sigma

    iterations <- iterations - 1L
    if ( max(abs(sigma - sigma.tmp)) <= 1e-12 || iterations == 0L) {
      break
    }
  }
  if (iterations == 0L) {
    no.convergence <- abs(sigma - sigma.tmp) > 1e-12
  }

  x <- sin.U1 * sin.sigma - cos.U1 * cos.sigma * cos.alpha1
  phi2 <- atan2(sin.U1 * cos.sigma + cos.U1 * sin.sigma * cos.alpha1,
                one.f * sqrt(sin.alpha * sin.alpha + x * x))
  lambda <- atan2(sin.sigma * sin.alpha1,
                  cos.U1 * cos.sigma - sin.U1 * sin.sigma * cos.alpha1)
  C <- f / 16 * cos2.alpha * (4 + f * (4 - 3 * cos2.alpha))
  L <- lambda - (1 - C) * f * sin.alpha *
    (sigma + C * sin.sigma * (cos.2sigma.m + C * cos.sigma *
                                (-1 + 2 * cos2.2sigma.m)))
  lambda2 <- p1[, 1] + L
  alpha2 <- atan2(sin.alpha, -x)
  p2 <- cbind(x=lambda2, y=phi2)

  if (any(no.convergence)) {
    p2[no.convergence, ]<- NaN
    alpha2[no.convergence] <- NaN
    warning("Vicenty formula failed to converge. Check your results.")
  }

  p2.is.p1 <- (s == 0)
  if (any(p2.is.p1)) {
    p2[p2.is.p1, ] <- p1[p2.is.p1, ]
    alpha2[p2.is.p1] <- NA
  }

  list(lon=lambda2, lat=phi2, final.bearing=alpha2)

}
# TODO: test how it behaves with s = 0 (and alpha1=0 or 90degrees?).
