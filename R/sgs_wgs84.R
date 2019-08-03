#' @encoding UTF-8
#' @title WGS84 to Easting Northing (Pseudo - Mercator)
#'
#' @description
#' Converts WGS84 coordinates to Easting/Northing (Pseudo-Mercator)
#'
#' @name sgs_wgs84_en
#' @usage sgs_wgs84_en(x, to = 3857)
#' @param x A \code{sgs_points} object describing a set of points in the
#' geodetic coordinate system EPSG=4326.
#' @param to Numeric. Sets the \code{epsg} code of the destination Geodetic
#' Coordinate System. 3857 (WGS84) by default. And currently doesn't support any
#' other.
#' @details
#' This routine also accepts source data expressed in ETRS89 coordinates
#' (EPSG=4258) as it is considered the difference between those two GCS is far
#' less than the accuracy available when working with Pseudo-Mercator
#' coordinates.
#'
#' The results can be used in maps where Pseudo-Mercator coordinates are needed.
#' Usually, those include Google, Bing, OpenStreetMap and several other webmap
#' applications including the MapChart in Spotfire.
#' @return
#' An object of class \code{sgs_points} whose coordinates are defined as
#' Easting/Northing.
#' @references OGP Publication 373-7-2 - Geomatics Guidance Note number 7, part
#' 2 (September 2016) \url{http://www.epsg.org/Portals/0/373-07-2.pdf}
#' @seealso \code{\link{sgs_points}}, \code{\link{sgs_en_wgs84}}.
#' @examples
#' p <- sgs_points(list(-3.9369, 56.1165), epsg=4326)
#' res <- sgs_wgs84_en(p)
#' @export
sgs_wgs84_en <- function(x, to=3857) UseMethod("sgs_wgs84_en")

#' @export
sgs_wgs84_en.sgs_points <- function(x, to=3857) {

  if (!x$epsg %in% c(4326, 4258))
    stop("This routine only supports EPSG:4326 WGS84 (or EPSG:4258) entries.")

  if(to != 3857)
    stop("This routine only supports converting to EPSG:3857 (Pseudo-Mercator)")

  core.cols <- sgs_points.core
  #coord.system <- epsgs[epsgs[, "epsg"]==x$epsg, "type"]
  #core.cols <- sgs_points.core[!sgs_points.core %in% c("easting", "northing")]

  additional.elements <- !names(x) %in% core.cols
  num.elements <- sum(additional.elements, na.rm=TRUE)

  phi <- x$y / 57.29577951308232087679815481410517
  lambda <- x$x / 57.29577951308232087679815481410517

  ellipsoid <- lonlat.datum[lonlat.datum$datum==x$datum, "ellipsoid"]

  a <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid, "a"]
  FE <- 0L; FN <- 0L # False Easting, Northing
  lambda0 <- 0L      # True origin

  e <- FE + a * (lambda - lambda0)
  n <- FN + a * log(tan(pi/4L + phi/2L))
  # Round up to 0.001 m
  e <- round(e, 3)
  n <- round(n, 3)

  # Return values
  en <- list(x=e, y=n)
  if (num.elements > 0) en <- c(x[additional.elements], en)

  sgs_points(en, coords=c("x", "y"), epsg=to)

}


#' @encoding UTF-8
#' @title Pseudo - Mercator to WGS84 Longitude/Latitude
#'
#' @description
#' Converts Pseudo - Mercator coordinates to WGS84 (EPSG=4326)
#'
#' @name sgs_en_wgs84
#' @usage sgs_en_wgs84(x, to = 4326)
#' @param x A \code{sgs_points} object describing a set of points in the
#' geodetic coordinate system EPSG=3857.
#' @param to Numeric. Sets the \code{epsg} code of the destination Geodetic
#' Coordinate System. 4326 (WGS84) by default. And currently doesn't support any
#' other.
#' @details
#' Currently converts ONLY from EPSG 3857 to 4326 (Longitude/Latitude).
#' @return
#' An object of class \code{sgs_points} whose coordinates are defined as
#' Longitude/Latitude.
#' @references OGP Publication 373-7-2 - Geomatics Guidance Note number 7, part
#' 2 (September 2016) \url{http://www.epsg.org/Portals/0/373-07-2.pdf}
#' @seealso \code{\link{sgs_points}}, \code{\link{sgs_wgs84_en}}.
#' @examples
#' p <- sgs_points(list(-11169055.58, 2810000.00), epsg=3857)
#' res <- sgs_en_wgs84(p)
#' @export
sgs_en_wgs84 <- function(x, to=4326) UseMethod("sgs_en_wgs84")

#' @export
sgs_en_wgs84.sgs_points <- function(x, to=4326) {

  if (x$epsg != 3857) stop("This routine only supports EPSG:3857 WGS84 entries")

  if(to != 4326)
    stop("This routine only supports converting to EPSG:4326 lon/lat.")

  core.cols <- sgs_points.core
 #core.cols <- sgs_points.core[!sgs_points.core %in% c("x", "y")]

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

  # Round and Return
  new.x <- sgs_points(
    list(x=round(lambda * 57.29577951308232087679815481410517, 8),
         y=round(phi * 57.29577951308232087679815481410517, 8)),
		 coords=c("x", "y"),
		 epsg=to)
  if (num.elements > 0) new.x <- c(x[additional.elements], new.x)
  new.x

}
