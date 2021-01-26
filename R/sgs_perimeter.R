#' @encoding UTF-8
#' @title Calculate area from an ordered set of points
#'
#' @description
#' Calculates the planar area for a set of points defined in the OS BNG or
#' ETRS89-LAEA. An approximation of the geodetic area is calculated when
#' entered points are expressed in angular coordinates.
#'
#' @name sgs_perimeter
#' @usage sgs_perimeter(x,
#'   grid.true.distance = ifelse(isTRUE(x$epsg==27700 || x$epsg==7405),
#'   TRUE, FALSE))
#' @param x A \code{sgs_points} object describing an ordered set of points. It
#' also accepts a list of sgs_points objects each of them describing a
#' different polygon (area).
#' @param grid.true.distance TODO. BNG only
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
#' even more accurate area computation, the boundary segments can be divided by
#' interpolating vertices on the projected geodesic.
#' @return
#' When entering a single \code{sgs_points} object it returns the value of the
#' area in squared metres. For a list of \code{sgs_points} objects, it will
#' return a list of areas in squared metres.
#' @references
#' Sandi Berk & Miran Ferlan, 2018. \emph{Accurate area determination in the
#' cadaster: case study of Slovenia}. Cartography and Geographic Information
#' Science, 45:1, 1-17. DOI: 10.1080/15230406.2016.1217789
#'
#' Snyder, J.P. 1987. \emph{Map Projections — A Working Manual}. US Geological
#' Survey Professional Paper, no. 1395. Washington, DC: US Government Printing
#' Office. DOI: 10.3133/pp1395
#' @examples
#' #TODO. PERHAPS MOVE PERIMETER FUNCTIONALITY TO SGS_AREA... and add a parameter to sgs_area,
#' to return or not a perimeter along with the area? Additionally, the calculation of perimeter is trivial
#' and could be just put in an example of sgs_distance.
#' @export
sgs_perimeter <- function (x,
  grid.true.distance = ifelse(isTRUE(x$epsg==27700 || x$epsg==7405),
                              TRUE, FALSE))
  UseMethod("sgs_perimeter")

#' @export
sgs_perimeter.sgs_points <- function(x,
  grid.true.distance = ifelse(isTRUE(x$epsg==27700 || x$epsg==7405),
                              TRUE, FALSE)) {

  if (isTRUE(x$epsg %in% c(4936, 3035, 4978, 3857)))
    stop("This function doesn't support the input's EPSG")

  p <- sgs_coordinates(x)
  p.shift.one <- rbind(p[-1, ], p[1, ])

  if(isTRUE(x$epsg==27700 || x$epsg==7405)) {

    sum(sgs_distance(x, sgs_points(p.shift.one, epsg=x$epsg),
                     by.element=TRUE, grid.true.distance))

  } else {

    sum(sgs_distance(x, sgs_points(p.shift.one, epsg=x$epsg),
                     by.element=TRUE, which="Vicenty"))

  }

}
