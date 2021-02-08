#' @encoding UTF-8
#' @title Calculate area from an ordered set of points
#'
#' @description
#' Calculates the planar area for a set of points defined in the OS BNG or
#' ETRS89-LAEA. An accurate approximation of the geodetic area is calculated
#' when entered points are expressed in angular coordinates.
#'
#' @name sgs_area
#' @usage sgs_area(x, interpolate = NULL, ...)
#' @param x A \code{sgs_points} object describing an ordered set of points.
#' @param interpolate Numeric variable. If not \code{NULL}, defines the maximum
#' distance in metres between adjacent coordinates. It is only used with
#' angular coordinates.
#' @param ... Currently ignored
#' @details
#' Calculate areas using the Gauss's area formula
#' (https://en.wikipedia.org/wiki/Shoelace_formula).
#'
#' When using angular coordinates the function performs an approximation of the
#' geodetic area following the methodology discussed in Berk & Ferlan (2018)
#' where the area on the ellipsoid is determined by using a region-adapted
#' equal-area projection (Albers Equal-Area Conic) with one standard parallel.
#' The standard parallel and the projection origin are tied to the moment
#' centroid of the polygon.
#'
#' To reduce the error introduced by boundary simplification and provide an
#' even more accurate area computation for angular coordinates, the boundary
#' segments can be divided by interpolating vertices on the projected geodesic.
#' For instance, if \code{interpolate = 500} then any segment between adjacent
#' coordinates whose length is greater then \code{interpolate} will be split in
#' parts no greater than \code{500 m} and new vertices will be calculated.
#' @return
#' Value of the area in squared metres round up to the first decimal.
#' @references
#' Sandi Berk & Miran Ferlan, 2018. \emph{Accurate area determination in the
#' cadaster: case study of Slovenia}. Cartography and Geographic Information
#' Science, 45:1, 1-17. DOI: 10.1080/15230406.2016.1217789
#'
#' Snyder, J.P. 1987. \emph{Map Projections — A Working Manual}. US Geological
#' Survey Professional Paper, no. 1395. Washington, DC: US Government Printing
#' Office. DOI: 10.3133/pp1395
#' @examples
#' lon <- c(-6.43698696, -6.43166843, -6.42706831, -6.42102546,
#' -6.42248238, -6.42639092, -6.42998435, -6.43321409)
#' lat <- c(58.21740316, 58.21930597, 58.22014035, 58.22034112,
#' 58.21849188, 58.21853606, 58.21824033, 58.21748949)
#' A <- sgs_area(sgs_points(list(lon, lat), epsg=4326))
#' @export
sgs_area <- function (x, interpolate = NULL, ...)
  UseMethod("sgs_area")

#' @export
sgs_area.sgs_points <- function(x, interpolate = NULL, ...) {

  if (isTRUE(x$epsg %in% c(4936, 4978, 3857)))
    stop("This function doesn't support the input's EPSG")

  coords <- c("x", "y")

  if(isTRUE(epsgs[epsgs$epsg == x$epsg, "type"] == "PCS")) {

    # Planar area (27700, 7405, 3035)
    planar.area(matrix(unlist(x[coords], use.names = FALSE), ncol = 2,
                       byrow = FALSE))

  } else {

    # Geodetic area
    # 1- transform to BNG (which is conformal: keeps angles - and shapes)
    # we could also just use a mercator transformation
    x.bng <- sgs_lonlat_bng(x, OSTN=TRUE, ODN.datum=FALSE)

    # 2- calculate centroid from BNG points and convert back to lonlat
    mc <- unname(moment.centroid(matrix(unlist(x.bng[coords], use.names=FALSE),
                                        ncol = 2, byrow = FALSE)))
    mc <- lapply(seq_len(ncol(mc)), function(i) mc[, i])
    names(mc) <- coords
    mc <- structure(c(mc, epsg = x.bng$epsg, datum = x.bng$datum,
                      dimension = x.bng$dimension), class = "sgs_points")
    mc <- sgs_bng_lonlat(mc, to=x$epsg, OSTN=TRUE)

    # 3- if we need to interpolate
    if (is.numeric(interpolate)) {

      mat.x.grad <- matrix(unlist(x[coords], use.names=FALSE),
             ncol = 2, byrow = FALSE)
      mat.x <- mat.x.grad / RAD.TO.GRAD
      x.shift.one <- rbind(mat.x[-1, ], mat.x[1, ])

      # calculate distances and bearings
      vicenty <- inverse.vicenty.ellipsoid(mat.x, x.shift.one, x$datum,
                                           iterations=300L)

      # work only with those coordinates whose distance exceeds our threshold
      need.int <- which(vicenty$distance > interpolate)
      intp1 <- mat.x[need.int, ]
      alpha1 <- vicenty$initial.bearing[need.int]

      # the first results of seq is 0. Don't want it [-1].
      segments <- mapply(function(x,y,by) seq(x,y,by)[-1],
                         0, vicenty$distance[need.int], interpolate,
                         SIMPLIFY = FALSE)

      # call direct.vicenty.ellipsoid to calculate inter locations
      num.segments <- lengths(segments)
      xy <- intp1[rep(seq_len(nrow(intp1)), num.segments), ]
      d.vicenty <- direct.vicenty.ellipsoid(p1 = xy, s = unlist(segments),
                                            alpha1 = rep(alpha1, num.segments),
                                            datum = x$datum, iterations=100L)

      inter.locations <- cbind(x = d.vicenty$lon, y = d.vicenty$lat) *
        RAD.TO.GRAD
      inter.locations <- cbind(inter.locations,
                               p=rep(need.int, times=num.segments))

      # insert the new locations within the existing locations
      lst.x.grad <- lapply(seq_len(nrow(mat.x.grad)),
                           function(i) mat.x.grad[i, ])
      lst.x.grad[need.int]  <- lapply(need.int, function(i) {
        rbind(lst.x.grad[[i]],
              inter.locations[inter.locations[, "p"]==i, c(1,2)])
        })

      res.matrix <- do.call(rbind, lst.x.grad)
      res.lst <- lapply(seq_len(ncol(res.matrix)), function(i) res.matrix[, i])
      names(res.lst) <- coords
      x <- structure(c(res.lst, epsg = x$epsg, datum = x$datum,
                       dimension = x$dimension), class = "sgs_points")

    }

    # 4- area calculation using a region-adapted equal area projection as
    # described in Berk and Ferlan, 2018. Albers Equal-Area Conic projection
    geod.area(x, mc)

  }

}


#' @noRd
#' @param p A matrix of coordinates
planar.area <- function(p) {

  # Translate to 0,0 to minimise losing floating point precision
  p[, 1] <- p[, 1] - min(p[, 1])
  p[, 2] <- p[, 2] - min(p[, 2])

  # Gauss formula
  # Ap = 1/2 * abs(sum(xi * yi+1 - xi+1 * yi))

  # Create the 'i+1' set of terms:
  p.shift.one <- rbind(p[-1, ], p[1, ])

  # (xi * yi+1 - xi+1 * yi)
  term <- p[, 1] * p.shift.one[, 2] - p.shift.one[, 1] * p[, 2]

  area <- 0.5 * abs(sum(term))
  round(area, 1)

}

#' @noRd
#' @param p A matrix of coordinates
moment.centroid <- function (p) {

  # Translate to 0,0 to minimise losing floating point precision
  min.x <- min(p[, 1])
  min.y <- min(p[, 2])
  p[, 1] <- p[, 1] - min.x
  p[, 2] <- p[, 2] - min.y

  # Create the 'i+1' set of terms:
  p.shift.one <- rbind(p[-1, ], p[1, ])

  # (xi * yi+1 - xi+1 * yi)
  term <- p[, 1] * p.shift.one[, 2] - p.shift.one[, 1] * p[, 2]

  area.div <- 3 * abs(sum(term)) # 3 -> 0.5 * 6

  x <- abs(sum((p[, 1] + p.shift.one[, 1]) * term))
  y <- abs(sum((p[, 2] + p.shift.one[, 2]) * term))

  centroid <- cbind(x,y) / area.div

  centroid[, 1] <- centroid[, 1] + min.x
  centroid[, 2] <- centroid[, 2] + min.y
  round(centroid, 3)

}

#' @noRd
#' @param p An sgs_points object containing a set of ordered angular coordinates
#' @param c An sgs_points object containing the coordinates of the centroid
geod.area <- function(p, c) {

  ellipsoid <- lonlat.datum[lonlat.datum$datum==p$datum, "ellipsoid"]
  params <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid,
                             c("a","e2")]
  a <- params$a
  e2 <- params$e2
  e <- sqrt(e2)

  lambda <- p$x / RAD.TO.GRAD
  phi <- p$y / RAD.TO.GRAD
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

  planar.area(cbind(e, n))

}
