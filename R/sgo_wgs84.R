#' @encoding UTF-8
#' @title WGS84 to Easting Northing (Pseudo - Mercator)
#'
#' @description
#' Converts WGS84 coordinates to Easting/Northing (Pseudo-Mercator)
#'
#' @name sgo_wgs84_en
#' @usage sgo_wgs84_en(x, to = 3857)
#' @param x A \code{sgo_points} object describing a set of points in the
#' geodetic coordinate system EPSG=4326 or 4979.
#' @param to Numeric. Sets the \code{epsg} code of the destination Geodetic
#' Coordinate System. 3857 (WGS84) by default. And currently doesn't support any
#' other.
#' @details
#' This routine also accepts source data expressed in ETRS89 coordinates
#' (EPSG=4258 or 4937) as it is considered the difference between those two GCS
#' is far less than the accuracy available when working with Pseudo-Mercator
#' coordinates.
#'
#' The results can be used in maps where Pseudo-Mercator coordinates are needed.
#' Usually, those include Google, Bing, OpenStreetMap and several other webmap
#' applications.
#' @return
#' An object of class \code{sgo_points} whose coordinates are defined as
#' Easting/Northing.
#' @references IOGP Publication 373-7-2 - Geomatics Guidance Note number 7,
#' part 2 (October 2020). https://epsg.org/guidance-notes.html
#' @seealso \code{\link{sgo_points}}, \code{\link{sgo_en_wgs84}}.
#' @examples
#' p <- sgo_points(list(-3.9369, 56.1165), epsg=4326)
#' res <- sgo_wgs84_en(p)
#' @export
sgo_wgs84_en <- function(x, to=3857) UseMethod("sgo_wgs84_en")

#' @export
sgo_wgs84_en.sgo_points <- function(x, to=3857) {

  if (!x$epsg %in% c(4326, 4979, 4258, 4937))
    stop("This routine only supports WGS84 or ETRS89 polar entries")

  if(to != 3857)
    stop("This routine only supports converting to EPSG:3857 (Pseudo-Mercator)")

  if (x$dimension == "XY") {
    core.cols <- .sgo_points.2d.core
  } else {
    core.cols <- .sgo_points.3d.core
  }

  additional.elements <- !names(x) %in% core.cols
  num.elements <- sum(additional.elements, na.rm=TRUE)

  phi <- x$y / RAD.TO.DEG
  lambda <- x$x / RAD.TO.DEG

  ellipsoid <- lonlat.datum[lonlat.datum$datum==x$datum, "ellipsoid"]

  a <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid, "a"]
  FE <- 0; FN <- 0  # False Easting, Northing
  lambda0 <- 0      # True origin

  #e <- round(FE + a * (lambda - lambda0), 2) #round to cm
  #n <- round(FN + a * log(tan(pi/4 + phi/2)), 2)
  e <- FE + a * (lambda - lambda0)
  n <- FN + a * log(tan(pi/4 + phi/2))

  # Return values
  en <- list(x=e, y=n)
  if (num.elements > 0)
    en <- c(en, x[additional.elements])

  structure(c(en, epsg = to, datum = .epsgs[.epsgs$epsg == to, "datum"],
              dimension = "XY"),
            class = "sgo_points")

}


#' @encoding UTF-8
#' @title Pseudo - Mercator to WGS84 Longitude/Latitude
#'
#' @description
#' Converts Pseudo - Mercator coordinates to WGS84 (EPSG=4326)
#'
#' @name sgo_en_wgs84
#' @usage sgo_en_wgs84(x, to = 4326)
#' @param x A \code{sgo_points} object describing a set of points in the
#' geodetic coordinate system EPSG=3857.
#' @param to Numeric. Sets the \code{epsg} code of the destination Geodetic
#' Coordinate System. 4326 (WGS84) by default. And currently doesn't support any
#' other.
#' @details
#' Currently converts ONLY from EPSG 3857 to 4326 (Longitude/Latitude).
#' @return
#' An object of class \code{sgo_points} whose coordinates are defined as
#' Longitude/Latitude.
#' @references IOGP Publication 373-7-2 - Geomatics Guidance Note number 7,
#' part 2 (October 2020). https://epsg.org/guidance-notes.html
#' @seealso \code{\link{sgo_points}}, \code{\link{sgo_wgs84_en}}.
#' @examples
#' p <- sgo_points(list(-11169055.58, 2810000.00), epsg=3857)
#' res <- sgo_en_wgs84(p)
#' @export
sgo_en_wgs84 <- function(x, to=4326) UseMethod("sgo_en_wgs84")

#' @export
sgo_en_wgs84.sgo_points <- function(x, to=4326) {

  if (x$epsg != 3857) stop("This routine only supports EPSG:3857 entries")

  if(to != 4326)
    stop("This routine only supports converting to EPSG:4326")

  core.cols <- .sgo_points.2d.core

  additional.elements <- !names(x) %in% core.cols
  num.elements <- sum(additional.elements, na.rm=TRUE)

  ellipsoid <- lonlat.datum[lonlat.datum$datum==x$datum, "ellipsoid"]

  a <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid, "a"]
  FE <- 0; FN <- 0  # False Easting, Northing
  lambda0 <- 0      # True origin

  E <- x$x
  N <- x$y

  D <- (FN - N) / a
  phi <- (pi/2) - 2 * atan(exp(D))
  lambda <- ((E - FE)/a) + lambda0

  # Return
  xy <- list(x=lambda * RAD.TO.DEG, y=phi * RAD.TO.DEG)
  if (num.elements > 0)
    xy <- c(xy, x[additional.elements])

  structure(c(xy, epsg=to, datum=.epsgs[.epsgs$epsg==to, "datum"],
              dimension="XY"),
            class="sgo_points")

}
