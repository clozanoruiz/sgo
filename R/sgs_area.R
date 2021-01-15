#' @encoding UTF-8
#' @title Calculate areas from a set of points
#'
#' @description
#' Calculates the planar area for a set of points defined in the OS NGR or
#' ETRS89-LAEA. The geodetic area is calculated when the entered points are
#' expressed in angular coordinates.
#'
#' @name sgs_area
#' @usage sgs_area(x, ...)
#' @param x A \code{sgs_points} object describing a set of points (clockwise, countercockwise?)
#' @param ... Currently ignored
#' @details
#' TODO.
#' @return
#' Value of the are in squared metres.
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
    #1-transform to BNG
    #2-calculate planar.area and centroid from BNG points
    #3-trnasform the centroid coordinates back to lonlat
    #4-continue with area calculation
  }

}

#p: matrix of points
planar.area <- function(p) {

  # Translate to 0,0 to minimise losing floating point precision
  p.x <- p[, 1] - min(p[, 1])
  p.y <- p[, 2] - min(p[, 2])

  n <- nrow(p)

  # Gauss formula
  area <- 0
  # Loop through vertices 2 to n-1
  for (i in 2:(n-1)) {
    diff <- p.y[i + 1] - p.y[i - 1]
    area <- area + p.x[i] * diff
  }

  # Vertices 1 and n
  diff <- p.y[2] - p.y[n]
  area <- area + p.x[1] * diff

  diff <- p.y[1] - p.y[n-1]
  area <- area + p.x[n] * diff

  area <- 0.5 * abs(area)

}

moment.centroid <- function () {
  #TODO
}

#Areas:
# compute planar area for BNG and LAEA projections (shoelace) - although BNG would be 'incorrect'
# compute geodetic area from lon/lat cordinates projecting to equal-area mapping (pdf's)
#translate points by substraction MinX and MinY (https://www.johndcook.com/blog/2018/09/26/polygon-area/)
#https://support.esri.com/en/technical-article/000006109
#https://en.wikipedia.org/wiki/Shoelace_formula

#https://github.com/rspatial/raster/blob/19db58371dc37562a74f23830174da47c1e8b9b4/src/area.cpp#L58
#https://rdrr.io/cran/pracma/src/R/polyarea.R
