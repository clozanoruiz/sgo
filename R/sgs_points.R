#' @encoding UTF-8
#' @title Object containing 2D point coordinates
#'
#' @description
#' 2D coordinates (and other attributes) of a point or collection of points
#'
#' @name sgs_points
#' @usage sgs_points(x, coords=NULL, epsg = NULL)
#' @param x A list or dataframe with at least 2 columns of either
#' easting/northing or longitude/latitude coordinates per row - depending on
#' \code{epsg}. \strong{Please note} that the order is important: lat/lon or
#' northing/easting will produce wrong results.
#'
#' It also accepts objects of class \code{sf POINT/MULTIPOINT} or
#' \code{sfc POINT/MULTIPOINT}.
#' @param coords A vector with the names of the columns containing the
#' easting/northing (or longitude/latitude) coordinates.
#' @param epsg Specifies the EPSG code of coordinates to store. It can take any
#' of the following values: \code{4326}, \code{3857}, \code{4277}, \code{27700}
#' or \code{4258}.
#' @details
#' This object stores 2D point coordinates and any other column-list of
#' attributes related to each point. Note that additional column-lists will be
#' expanded with \code{NA} values if they contain less elements than coordinates.
#' Currently it only supports 5 \code{epsg}s for coordinates:
#' \itemize{
#' \item\code{4326}: WGS84, geodetic coordinate system. The 2 columns in
#' \code{x} be defined as Longitude and Latitude values respectively. The
#' defined datum for this set of coordinates is WGS84 (https://epsg.io/4326)
#'
#' \item\code{3857}: WGS 84 / Pseudo-Mercator, projected coordinate system. The
#' 2 columns in \code{x} must be defined as Easting and Northing values
#' respectively. The defined datum for this set of coordinates is WGS84
#' (https://epsg.io/3857)
#'
#' \item\code{4277}: OSGB36, geodetic coordinate system. The 2 columns in
#' \code{x} must be defined as Longitude and Latitude values respectively. The
#' defined datum for this set of coordinates is OSGB 1936
#' (https://epsg.io/4277). \emph{Coordinates defined like this should only be
#' used for historical reasons.}
#'
#' \item\code{27700}: British National Grid, projected coordinate system. The 2
#' columns in \code{x} must be defined as Easting and Northing values
#' respectively. The defined datum for this set of coordinates is OSGB 1936
#' (https://epsg.io/27700).
#'
#' \item\code{4258}: ETRS89, geodetic coordinate system. The 2 columns in
#' \code{x} must be defined as Longitude and Latitude values respectively. The
#' defined datum for this set of coordinates is ETRS89 (https://epsg.io/4258).
#' }
#' @return
#' An object of class \code{sgs_points}.
#' @seealso \code{\link{sgs_polygons}}, \code{\link{sgs_points_xy}},
#' \code{\link{sgs_points_sf}}, \code{\link{sgs_set_gcs}}.
#' @examples
#' # lists:
#' p1 <- sgs_points(list(-3.9369, 56.1165), epsg=4326)
#' lon <- c(-4.25181,-3.18827)
#' lat <- c(55.86424, 55.95325)
#' p2 <- sgs_points(list(longitude=lon, latitude=lat), epsg=4326)
#' #p3 will expand the list 'desc' up to 2 elements with NA's
#  p3 <- sgs_points(list(longitude=lon, latitude=lat, desc="c1"), epsg=4326)
#' # dataframe:
#' ln <- c(-4.22472, -2.09908)
#' lt <- c(57.47777, 57.14965)
#' n <- c("Inverness", "Aberdeen")
#' df <- data.frame(n, ln, lt, stringsAsFactors = FALSE)
#' p4 <- sgs_points(df, coords=c("ln", "lt"), epsg=4326)
#'
#' \dontrun{
#' library(maps)
#' map('world', regions=('uk'))
#' points(x=p1$longitude, y=p1$latitude, pch=0, col="green")
#' points(x=p2$lon, y=p2$lat, pch=0, col="blue")
#' points(x=p4$lon, y=p4$lat, pch=0, col="red")
#' }
#' @export
sgs_points <- function (x, coords=NULL, epsg=NULL) UseMethod("sgs_points")

#' @export
sgs_points.list <- function (x, coords=NULL, epsg=NULL) {

  # Checks
  len <- length(x)
  if (len < 2) stop("This method accepts lists with at least 2 columns")

  if(is.null(epsg) || (!epsg %in% epsgs[, "epsg"])) {
    stop("'epsg' must be entered as 4326, 3857, 4277, 27700 or 4258")
  }

  if (len == 2 && is.null(names(x)) ) {
    coords <- coordinates.names[1, ] #just because
    names(x) <- coords
  }

  if(is.null(coords)) {
    lnames <- tolower(names(x))
    known.coords <- t(apply(coordinates.names, 1, function(n) n %in% lnames))
    if (any(known.coords) && sum(known.coords, na.rm = TRUE) == 2) {
      coords <- names(x)[lnames %in% coordinates.names[known.coords]]
    } else {
      stop("Must specify the coordinates columns using the 'coords' parameter")
    }
  }

  if(!is.numeric(x[[coords[1]]]) | !is.numeric(x[[coords[2]]]))
    stop("All coordinates must be numeric")

  other.columns <- x[!(names(x) %in% coords)]

  if (length(other.columns)==0) {
    other.columns <- NULL
  } else {
    # if other.columns contains the column names 'x' or 'y' we will remove them
    # and warn the user about it.
    old.length <- length(other.columns)
    other.columns <- other.columns[!(names(other.columns) %in% c("x", "y"))]
    if (length(other.columns) != old.length) {
      warning("Any column from input data named 'x' or 'y' has been removed")
    }

    # all additional columns will expanded to contain the same number of
    # elements as coordinates in the object
    max.len <- length(x[[coords[1]]])
    expand.columns <- lapply(Filter(function(x) length(x) < max.len,
                                    other.columns), `length<-`, max.len)
    if (length(expand.columns)==0) expand.columns <- NULL
    other.columns <- c(Filter(function(x) length(x) >= max.len, other.columns),
                       expand.columns)
  }

  points <- switch(as.character(epsg),
                   "4326" = structure(c(other.columns,
                                        list(x = x[[coords[1]]],
                                             y = x[[coords[2]]],
                                             epsg=epsg, datum="WGS84")),
                                      class="sgs_points"),
                   "3857" = structure(c(other.columns,
                                         list(x = x[[coords[1]]],
                                              y = x[[coords[2]]],
                                              epsg=epsg, datum="WGS84")),
                                       class="sgs_points"),
                   "4277" = structure(c(other.columns,
                                        list(x = x[[coords[1]]],
                                             y = x[[coords[2]]],
                                             epsg=epsg, datum="OSGB36")),
                                      class="sgs_points"),
                   "27700" = structure(c(other.columns,
                                        list(x = x[[coords[1]]],
                                             y = x[[coords[2]]],
                                             epsg=epsg, datum="OSGB36")),
                                      class="sgs_points"),
                   "4258" = structure(c(other.columns,
                                         list(x = x[[coords[1]]],
                                              y = x[[coords[2]]],
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
    stop("'epsg' must be entered as 4326, 3857, 4277, 27700 or 4258")
  }

  # the names we apply don't matter for the rest of the function
  # when there are only 2 columns
  if (cols == 2) {
    coords <- coordinates.names[1, ] #just because
    names(x) <- coords
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
#' automatically by the \code{\link{sgs_points}} constructor.
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

  len.coords <- length(x$x)

  # Ensure all extra columns have the same numbers of elements as coordinates
  # This means there won't be 'recycling' of elements
  x[columns] <- lapply(x[columns], function(col, len) {
                                    length(col) <- len; col}, len=len.coords)

  sf::st_as_sf(data.frame(x[columns], stringsAsFactors = FALSE),
               coords=c("x", "y"), crs =x$epsg, dim="XY")

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

  xy <- matrix(unlist(x[c("x", "y")]), ncol=2)
  colnames(xy) <- coords

  xy

}

# TODO export
# Extending '[' function to support sgs_points:
# Attributes are usually lost when subsetting lists, therefore we need to
# extend a subsetting operator that will keep all attributes of our object.
`[.sgs_points` <- function(x, i, ...) {

  core.cols <- sgs_points.core[!sgs_points.core %in% c("x", "y")]

  r <- NextMethod("[")
  if ( all(core.cols %in% names(r)) ) class(r) <- "sgs_points"
  #if (!inherits(r, "sgs_points")) class(r) <- "sgs_points"
  r

}

#TODO export
#c.sgs_points <- function(..., recursive = FALSE) {
c.sgs_points <- function(...) {

  dots <- list(...)
  classes <- sapply(dots, class)

  if (all(classes == "sgs_points")) { #merge them

    if (length(unique(sapply(dots, `[`, "epsg"))) == 1) { #all equal
      coords <- do.call(mapply,
                        c(FUN=c, lapply(dots, `[`, c("longitude", "latitude"))))
      coords <- split(coords, col(coords))

      other.columns <- unlist(sapply(dots, function(x, n) {x[!names(x) %in% n]},
                                     n=sgs_points.core), recursive=FALSE)

      r <- structure(c(other.columns,
                       list(coords, epsg=dots[[1]]$epsg, datum=dots[[1]]$datum)),
                     class = "sgs_points")
    } else { #different EPSG's we cannot merge
      stop("Objects with different EPSG codes cannot be combined")
    }

  } else { #only the first one can be sgs_points
    if (classes[1] == "sgs_points" && all(classes[-1] != "sgs_points")) {
      r <- NextMethod("c")
      class(r) <- "sgs_points"
    } else {
      stop("Only the first object can be of type sgs_points")
    }
  }

  attr(r, "coords") <- attr(dots[[1]], "coords")
  r

}

#TODO improve print! and export
print.sgs_points <- function(x) {
  #ADD attributes like sgs_x and sgs_y to sgspoints so we save all those coordinate checkins:
  #like: attr(p1, "sgs_x") <-"latitude" (or easting)

  #len.coords <- length(x$x)
  if (length(x$x) > 6) {
    "showing only the first elements... or just add  an ellipsis after the 6 coordinates"
  }

  cat("EPSG:", x$epsg, "\n",
      x$x, x$y)

}
