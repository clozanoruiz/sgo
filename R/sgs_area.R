#' @encoding UTF-8
#' @title Calculate area from an ordered set of points
#'
#' @description
#' Calculates the planar area for a set of points defined in the OS NGR or
#' ETRS89-LAEA. The geodetic area is calculated when entered points are
#' expressed in angular coordinates.
#'
#' @name sgs_area
#' @usage sgs_area(x, ...)
#' @param x A \code{sgs_points} object describing a set of points (clockwise, countercockwise?)
#' @param ... Currently ignored
#' @details
#' Calculate areas using the Gauss's area formula (https://en.wikipedia.org/wiki/Shoelace_formula)
#' @return
#' Value of the are in squared metres.
#' @references
#' Sandi Berk & Miran Ferlan, 2018. \emph{Accurate area determination in the
#' cadaster: case study of Slovenia}. Cartography and Geographic Information
#' Science, 45:1, 1-17. DOI: 10.1080/15230406.2016.1217789
#'
#' Snyder, J.P. 1987. \emph{Map Projections — A Working Manual}. US Geological
#' Survey Professional Paper, no. 1395. Washington, DC: US Government Printing
#' Office. DOI: 10.3133/pp1395
#' @examples
#' #TODO
#' @export
sgs_area <- function (x, ...)
  UseMethod("sgs_area")

#' @export
sgs_area.sgs_points <- function(x, ...) {

  if (isTRUE(x$epsg %in% c(4936, 4978, 3857)))
    stop("This function doesn't support the input's EPSG")

  coords <- c("x", "y")

  if(isTRUE(epsgs[epsgs$epsg == x$epsg, "type"] == "PCS")) {

    # Planar area (27700, 7405, 3035)
    planar.area(as.matrix(x[, coords, drop=TRUE]))

  } else {

    # Geodetic area
    # 1- transform to BNG (which is conformal: keeps angles - and shapes)
    # we could also just use a mercator transformation
    x.bng <- sgs_lonlat_bng(x, OSTN=TRUE, ODN.datum=FALSE)

    # 2- calculate centroid from BNG points and convert back to lonlat
    c <- moment.centroid(as.matrix(x.bng[, coords, drop=TRUE]))
    c <- sgs_bng_lonlat(sgs_points(as.data.frame(c), coords=coords,
                                   epsg = x.bng$epsg), to=x$epsg, OSTN=TRUE)

    # 3- area calculation using a region-adapted equal area projection as
    # described in Berk and Ferlan, 2018. Albers Equal-Area Conic projection

    # ellipsoid parameters
    ellipsoid <- lonlat.datum[lonlat.datum$datum==x$datum, "ellipsoid"]
    params <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid,
                               c("a","e2")]
    a <- params$a
    e2 <- params$e2
    e <- sqrt(e2)

    lambda <- x$x / RAD.TO.GRAD
    phi <- x$y / RAD.TO.GRAD
    lambda.c <- c$x / RAD.TO.GRAD
    phi.c <- c$y / RAD.TO.GRAD

    sin.phi <- sin(phi)
    sin.phi.c <- sin(phi.c)
    cos.phi.c <- cos(phi.c)
    sin2.phi <- sin.phi * sin.phi
    splat <- 1 - e2 * sin2.phi
    splat.c <- 1 - e2 * sin.phi.c * sin.phi.c

    # Follow methodology from Berk and Ferlan, 2018: Here the standard parallel
    # and the projection origin are tied to the moment centroid (phi0, lambda0).
    # Since only one standard parallel (which has same φ as the centroid here)
    # is used, then n in q.phi (Snyder 1987) is sin(phi0)
    q.phi <- (1 - e2) * (sin.phi / splat - 1/(2 * e) *
                           log((1 - e * sin.phi) / (1 + e * sin.phi)))
    q.phi.c <- (1 - e2) * (sin.phi.c / splat.c - 1/(2 * e) *
                             log((1 - e * sin.phi.c) / (1 + e * sin.phi.c)))

    theta <- sin.phi.c * (lambda - lambda.c)
    C <- cos.phi.c * cos.phi.c / splat.c + sin.phi.c * q.phi.c
    rho <- a * sqrt(C - q.phi * sin.phi.c) / sin.phi.c
    rho.c <- a * sqrt(C - q.phi.c * sin.phi.c) / sin.phi.c

    e <- rho * sin(theta)
    n <- rho.c - rho * cos(theta)

    area <- planar.area(cbind(e, n))

  }

}

#p: matrix of points
planar.area <- function(p) {

  # Translate to 0,0 to minimise losing floating point precision
  p[, 1] <- p[, 1] - min(p[, 1])
  p[, 2] <- p[, 2] - min(p[, 2])

  # Gauss formula
  # Ap = 1/2 * abs(sum(xi * yi+1 - xi+1 * yi))

  # Create the 'i+1' set of terms:
  p.plus.one <- rbind(p[-1, ], p[1, ])

  # (xi * yi+1 - xi+1 * yi)
  term <- p[, 1] * p.plus.one[, 2] - p.plus.one[, 1] * p[, 2]

  area <- 0.5 * abs(sum(term))
  round(area, 3)

}

#p: matrix of points
moment.centroid <- function (p) {

  # Translate to 0,0 to minimise losing floating point precision
  min.x <- min(p[, 1])
  min.y <- min(p[, 2])
  p[, 1] <- p[, 1] - min.x
  p[, 2] <- p[, 2] - min.y

  # Create the 'i+1' set of terms:
  p.plus.one <- rbind(p[-1, ], p[1, ])

  # (xi * yi+1 - xi+1 * yi)
  term <- p[, 1] * p.plus.one[, 2] - p.plus.one[, 1] * p[, 2]

  area.div <- 3 * abs(sum(term)) # 3 -> 0.5 * 6

  x <- abs(sum((p[, 1] + p.plus.one[, 1]) * term))
  y <- abs(sum((p[, 2] + p.plus.one[, 2]) * term))

  centroid <- cbind(x,y) / area.div

  centroid[, 1] <- centroid[, 1] + min.x
  centroid[, 2] <- centroid[, 2] + min.y
  round(centroid, 3)

}
