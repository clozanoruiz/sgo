#' @encoding UTF-8
#' @title Object containing 2D point coordinates
#'
#' @description
#' 2D coordinates (and other atributes) of a point or collection of points
#'
#' @name sgs_points
#' @usage sgs_points(x, coords=NULL, epsg = NULL)
#' @param x A list or dataframe with at least 2 columns of either easting/northing
#' or latitude/longitude coordinates per row - depending on \code{epsg}. \strong{Please
#' note} that the order is important: lon/lat or northing/easting will produce
#' wrong results.
#'
#' It also accepts objects of class \code{sf POINT/MULTIPOINT} or
#' \code{sfc POINT/MULTIPOINT}.
#' @param coords A vector with the names of the columns containing the easting/northing
#' (or latitude/longitude) coordinates.
#' @param epsg Specifies the EPSG code of coordinates to store. It can take any
#' of the following values: \code{4326}, \code{3857}, \code{4277}, \code{27700} or \code{4258}.
#' @details
#' This object stores 2D point coordinates and any other column-list of attributes
#' related to each point. Currently it only supports 5 \code{epsg}s
#' for coordinates:
#' \itemize{
#' \item\code{4326}: WGS84, geodetic coordinate system. The 2 columns in \code{x} be defined
#' as Latitude and Longitude values respectively. The defined datum for this set of
#' coordinates is WGS84 (https://epsg.io/4326)
#'
#' \item\code{3857}: WGS 84 / Pseudo-Mercator, projected coordinate system. The 2 columns
#' in \code{x} must be defined as Easting and Northing values respectively. The defined
#' datum for this set of coordinates is WGS84 (https://epsg.io/3857)
#'
#' \item\code{4277}: OSGB36, geodetic coordinate system. The 2 columns in \code{x}
#' must be defined as Latitude and Longitude values respectively. The defined datum for this
#' set of coordinates is OSGB 1936 (https://epsg.io/4277). \emph{Coordinates defined
#' like this should only be used for historical reasons.}
#'
#' \item\code{27700}: British National Grid, projected coordinate system. The 2 columns
#' in \code{x} must be defined as Easting and Northing values respectively. The defined
#' datum for this set of coordinates is OSGB 1936 (https://epsg.io/27700).
#'
#' \item\code{4258}: ETRS89, geodetic coordinate system. The 2 columns in \code{x}
#' must be defined as Latitude and Longitude values respectively. The defined
#' datum for this set of coordinates is ETRS89 (https://epsg.io/4258).
#' }
#' @return
#' An object of class \code{sgs_points}.
#' @seealso \code{\link{sgs_polygons}}, \code{\link{sgs_points_xy}},
#' \code{\link{sgs_points_sf}}, \code{\link{sgs_set_gcs}}.
#' @examples
#' # lists:
#' p1 <- sgs_points(list(56.1165, -3.9369), epsg=4326)
#' lat <- c(55.86424, 55.95325)
#' lon <- c(-4.25181,-3.18827)
#' p2 <- sgs_points(list(latitude=lat, longitude=lon), epsg=4326)
#' # dataframe:
#' lt <- c(57.47777, 57.14965)
#' ln <- c(-4.22472, -2.09908)
#' n <- c("Inverness", "Aberdeen")
#' df <- data.frame(n, lt, ln, stringsAsFactors = FALSE)
#' p3 <- sgs_points(df, coords=c("lt", "ln"), epsg=4326)
#'
#' \dontrun{
#' library(maps)
#' map('world', regions=('uk'))
#' points(x=p1$longitude, y=p1$latitude, pch=0, col="green")
#' points(x=p2$lon, y=p2$lat, pch=0, col="blue")
#' points(x=p3$lon, y=p3$lat, pch=0, col="red")
#' }
#' @export
sgs_points <- function (x, coords=NULL, epsg=NULL) UseMethod("sgs_points")

#' @export
sgs_points.list <- function (x, coords=NULL, epsg=NULL) {

  # Checks
  len <- length(x)
  if (len < 2) stop("This method accepts lists with at least 2 columns")

  if(is.null(epsg) || (!epsg %in% epsgs[, "epsg"])) {
    stop("The epsg parameter must be entered as 4326, 3857, 4277, 27700 or 4258")
  }

  if (len == 2 && is.null(names(x)) ) {
    coords <- coordinates.names[1, ] #just because
    names(x) <- coords
  }

  if(is.null(coords)) {
    lnames <- tolower(names(x))
    known.coords <- t(apply(coordinates.names, 1, function(n) n %in% lnames))
    if (any(known.coords)) {
      coords <- names(x)[lnames %in% coordinates.names[known.coords]]
    } else { stop("Must specify the name of columns containing coordinates using the 'coords' paramater") }
  }

  if(!is.numeric(x[[coords[1]]]) | !is.numeric(x[[coords[2]]]))
    stop("All coordinates must be numeric")

  other.columns <- x[!(names(x) %in% coords)]
  if (length(other.columns)==0) other.columns <- NULL
  points <- switch(as.character(epsg),
                   "4326" = structure(c(other.columns,
                                        list(latitude = x[[coords[1]]], longitude = x[[coords[2]]],
                                             epsg=epsg, datum="WGS84")),
                                      class="sgs_points"),
                   "3857" = structure(c(other.columns,
                                         list(easting = x[[coords[1]]], northing = x[[coords[2]]],
                                              epsg=epsg, datum="WGS84")),
                                       class="sgs_points"),
                   "4277" = structure(c(other.columns,
                                        list(latitude = x[[coords[1]]], longitude = x[[coords[2]]],
                                             epsg=epsg, datum="OSGB36")),
                                      class="sgs_points"),
                   "27700" = structure(c(other.columns,
                                        list(easting = x[[coords[1]]], northing = x[[coords[2]]],
                                             epsg=epsg, datum="OSGB36")),
                                      class="sgs_points"),
                   "4258" = structure(c(other.columns,
                                         list(latitude = x[[coords[1]]], longitude = x[[coords[2]]],
                                              epsg=epsg, datum="ETRS89")),
                                       class="sgs_points")
  )

}

#' @export
sgs_points.data.frame <- function (x, coords=NULL, epsg=NULL) {

  # Checks
  cols <- ncol(x)
  if (cols < 2) stop("This method accepts dataframes with at least 2 columns")

  if(is.null(epsg) || (!epsg %in% epsgs[, "epsg"])) {
    stop("The epsg parameter must be entered as 4326, 3857, 4277, 27700 or 4258")
  }

  if (cols == 2) {
    coords <- coordinates.names[1, ] #just because
    names(x) <- coords #when there are only 2 cols, which names we apply shouldn't matter
  }

  sgs_points(as.list(x), coords=coords, epsg=epsg)

}

#' @export
sgs_points.sf <- function (x, coords=NULL, epsg=NULL) {

  epsg <- sf_checks(x, epsg)

  coordinates <- sf::st_coordinates(x)
  coords <- colnames(coordinates)
  rownames(coordinates) <- NULL

  sf::st_geometry(x) <- NULL
  sgs_points(as.list(cbind(x, coordinates)), coords=coords, epsg=epsg)

}

#' @export
sgs_points.sfc <- function (x, coords=NULL, epsg=NULL) {

  epsg <- sf_checks(x, epsg)

  coordinates <- sf::st_coordinates(x)
  coords <- colnames(coordinates)
  rownames(coordinates) <- NULL

  lst <- list(coordinates[, 1], coordinates[, 2])
  names(lst) <- coords
  sgs_points(lst, coords=coords, epsg=epsg)

}


#' @encoding UTF-8
#' @title Convert sgs_points objects to sf objects
#'
#' @description
#' Converts \code{sgs_points} objects to instances of class \code{sf}. \pkg{sf}
#' is a package that allows working with spatial vector data.
#'
#' The reverse conversion (from \code{sf} to \code{sgs_points}) is done
#' automatically by the \code{\link{sgs_points}} contructor.
#'
#' @name sgs_points_sf
#' @usage sgs_points_sf(x)
#' @param x An instance of \code{sgs_points}.
#' @return
#' An object of class \code{sf}.
#' @examples
#' p <- sgs_points(list(57.47777, -4.22472), epsg=4326)
#' sf.p <- sgs_points_sf(p)
#' sf::st_crs(sf.p)
#' @export
sgs_points_sf <- function (x) UseMethod("sgs_points_sf")

#' @export
sgs_points_sf.sgs_points <-function(x) {

  x <- unclass(x)
  names <- names(x)
  columns <- !names %in% c("epsg", "datum")

  # Projection type
  coord.system <- epsgs[epsgs[, "epsg"]==x$epsg, "type"]

  if(coord.system == "GCS") {
    coords <- c("longitude", "latitude")
    len.coords <- length(x$latitude)
  } else {
    coords <- c("easting", "northing")
    len.coords <- length(x$easting)
  }

  # Ensure all extra columns have the same numbers of elements as coordinates
  # This means there won't be 'recycling' of elements
  x[columns] <- lapply(x[columns], function(col, len) {
                                    length(col) <- len; col}, len=len.coords)

  sf::st_as_sf(data.frame(x[columns], stringsAsFactors = FALSE),
               coords=coords, crs =x$epsg, dim="XY")

}

#' @encoding UTF-8
#' @title Extracts coordinates from an \code{sgs_points} object
#'
#' @description
#' Extract the coordinates of an \code{sgs_points} object expressed as a matrix.
#'
#' @name sgs_points_xy
#' @usage sgs_points_xy(x)
#' @param x An instance of \code{sgs_points}.
#' @return
#' A matrix with 2 named columns.
#' @examples
#' p <- sgs_points(list(57.47777, -4.22472), epsg=4326)
#' coords <- sgs_points_xy(p)
#' @export
sgs_points_xy <- function (x) UseMethod("sgs_points_xy")

#' @export
sgs_points_xy.sgs_points <-function(x) {

  # Projection type
  coord.system <- epsgs[epsgs[, "epsg"]==x$epsg, "type"]

  coords <- if(coord.system == "GCS") {
    c("latitude", "longitude")
  } else {
    c("easting", "northing")
  }

  xy <- matrix(unlist(x[coords]), ncol=2)
  colnames(xy) <- coords

  xy

}

# Extending '[' function to support sgs_points:
# Attributes are usually lost when subsetting lists, therefore we need to
# extend a subsetting operator that will keep all attributes of our object.
`[.sgs_points` <- function(x, i, ...) {

  if (epsgs[epsgs[, "epsg"]==x$epsg, "type"] == "GCS") {
    core.cols <- sgs_points.core[!sgs_points.core %in% c("easting", "northing")]
  } else {
    core.cols <- sgs_points.core[!sgs_points.core %in% c("latitude", "longitude")]
  }

  r <- NextMethod("[")
  if ( all(core.cols %in% names(r)) ) class(r) <- "sgs_points"
  #if (!inherits(r, "sgs_points")) class(r) <- "sgs_points"
  r

}

# TODO print!
print.sgs_points <- function(o) {
  cat(o$epsg, "\n")
  cat(o$datum, "\n")
}

# sgs_equal.sgs_points
# BETTER DO THIS WITH SF (AND MOSTLY EVERY OTHER GEO FUNCTION)!!!
