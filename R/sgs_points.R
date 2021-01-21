#' @encoding UTF-8
#' @title Object containing 2D or 3D point coordinates
#'
#' @description
#' 2D or 3D coordinates (and other attributes) of a point or collection of
#' points
#'
#' @name sgs_points
#' @usage sgs_points(x, coords = NULL, epsg = NULL)
#' @param x A matrix, list or dataframe with at least 2 columns of either
#' easting/northing or longitude/latitude coordinates per row. A third column
#' with height values is optional.
#' \strong{Please note} that the order is important when \code{x} has only 2 or
#' 3 columns and \code{coords} is not informed: lat/lon or northing/easting
#' (and height) will produce wrong results.
#'
#' @param coords A vector with the names of the two or three columns containing
#' the X (easting or longitude), Y (northing or latitude) and optionally Z
#' (ellipsoid or orthometric height) coordinates.
#' @param epsg Specifies the EPSG code of coordinates to store. It can take any
#' of the following values:
#' \itemize{
#' \item when working with (2D/3D) ETRS89 Datum: \code{4258}, \code{4937},
#' \code{4936}, \code{3035}
#' \item when working with (2D/3D) WGS84 Datum:\code{4326}, \code{4979},
#' \code{4978}
#' \item when working with (2D/3D) OSGB36 Datum:\code{4277}, \code{27700},
#' \code{7405}
#' \item WGS84/Pseudo-Mercator (Google Maps, OpenStreetMap, etc.): \code{3857}
#' }
#' @details
#' This object stores 2D or 3D point coordinates and any other column-list of
#' attributes related to each point. Note that additional column-lists will be
#' expanded with \code{NA} values if they contain less elements than
#' coordinates. Currently it only supports the following \code{epsg}s:
#' \itemize{
#' \item\code{4258}: ETRS89, geodetic coordinate system. The columns in
#' \code{x} must be defined as Longitude and Latitude (\code{sgs} also accepts
#' a third column for ellipsoid heights). The defined datum for this set of
#' coordinates is ETRS89 (https://epsg.io/4258).
#'
#' \item\code{4937}: ETRS89, geodetic coordinate system. The columns in
#' \code{x} must be defined as Longitude, Latitude and Ellipsoid Heights
#' respectively. The defined datum for this set of coordinates is ETRS89
#' (https://epsg.io/4937).
#'
#' \item\code{4936}: ETRS89, geodetic coordinate system. The columns in
#' \code{x} must be defined as cartesian coordinates x, y and z. The defined
#' datum for this set of coordinates is ETRS89 (https://epsg.io/4936).
#'
#' \item\code{3035}: ETRS-LAEA, projected coordinate system. The
#' columns in \code{x} must be defined as Easting and Northing. The defined
#' datum for this set of coordinates is WGS84 (https://epsg.io/3035)
#'
#' \item\code{4326}: WGS84, geodetic coordinate system. The columns in \code{x}
#' must be defined as Longitude and Latitude (\code{sgs} also accepts a
#' third column for ellipsoid heights). The defined datum for this set of
#' coordinates is WGS84 (https://epsg.io/4326).
#'
#' \item\code{4979}: WGS84, geodetic coordinate system. The columns in \code{x}
#' must be defined as Longitude, Latitude and Ellipsoid Height respectively.
#' The defined datum for this set of coordinates is WGS84 (https://epsg.io/4979)
#'
#' \item\code{4978}: WGS84, geodetic coordinate system. The columns in \code{x}
#' must be defined as cartesian coordinates x, y and z. The
#' defined datum for this set of coordinates is WGS84 (https://epsg.io/4978)
#'
#' \item\code{4277}: OSGB36, geodetic coordinate system. The 2 columns in
#' \code{x} must be defined as Longitude and Latitude values respectively. The
#' defined datum for this set of coordinates is OSGB 1936
#' (https://epsg.io/4277). \emph{Coordinates defined like this should only be
#' used for historical reasons.} \strong{Height values will be discarded when
#' working with this coordinate system.}
#'
#' \item\code{27700}: British National Grid, projected coordinate system. The
#' columns in \code{x} must be defined as Easting and Northing (\code{sgs} also
#' accepts a third column for orthometric heights). The defined datum for this
#' set of coordinates is OSGB 1936 (https://epsg.io/27700).
#'
#' \item\code{7405}: British National Grid, projected coordinate system. The
#' columns in \code{x} must be defined as Easting, Northing and ODN Orthometric
#' height respectively (\code{sgs} accepts heights from other datums like
#' Orkney, Lerwick, Stornoway, Douglas, St.Marys and 'Newlyn offshore').
#' The defined datum for this set of coordinates is OSGB 1936
#' (https://epsg.io/7405).
#'
#' \item\code{3857}: WGS 84 / Pseudo-Mercator, projected coordinate system. The
#' columns in \code{x} must be defined as Easting and Northing. The defined
#' datum for this set of coordinates is WGS84 (https://epsg.io/3857)
#' }
#'
#' @return
#' An object of class \code{sgs_points}.
#' @seealso \code{\link{sgs_coordinates}}, \code{\link{sgs_transform}}.
#' @examples
#' # lists:
#' p1 <- sgs_points(list(-3.9369, 56.1165), epsg=4326)
#' lon <- c(-4.25181,-3.18827)
#' lat <- c(55.86424, 55.95325)
#' p2 <- sgs_points(list(longitude=lon, latitude=lat), epsg=4326)
#' #p3 will fill up the list 'desc' up to 2 elements with NA's:
#' p3 <- sgs_points(list(longitude=lon, latitude=lat, desc="c1"),
#'                  coords=c("longitude", "latitude"), epsg=4326)
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
sgs_points <- function (x, coords=NULL, epsg=NULL)
  UseMethod("sgs_points")

#' @export
sgs_points.list <- function (x, coords=NULL, epsg=NULL) {

  # Checks
  len <- length(x)
  if (len < 2) stop("This method accepts lists with at least 2 columns")

  if(is.null(epsg) || (!epsg %in% epsgs[, "epsg"])) {
    stop("'epsg' must be entered as one of the accepted numbers")
  }

  if (len < 4 && is.null(names(x)) ) {
    coords <- coordinates.names[1, 1:len] #c(x,y,(z)) just because
    names(x) <- coords
  }

  if(is.null(coords)) {
    lnames <- tolower(names(x))
    known.coords <- t(apply(coordinates.names, 1, function(n) n %in% lnames))
    if (any(known.coords) && sum(known.coords, na.rm = TRUE) == len) {
      coords <- names(x)[lnames %in% coordinates.names[known.coords]]
    } else {
      stop("Must specify the coordinates columns using the 'coords' parameter")
    }
  }

  num.coords <- length(coords)

  #check those epsgs that ONLY admit 2 and have 3, and viceversa
  if ((epsg %in% epsgs[epsgs$dimension=="XY", "epsg"] && num.coords > 2) ||
      (epsg %in% epsgs[epsgs$dimension=="XYZ", "epsg"] && num.coords < 3)) {
    stop("Wrong number of coordinates for the The specified 'epsg'")
  }

  for (i in 1:num.coords) {
    if (!is.numeric(x[[coords[i]]]) )
      stop("All coordinates must be numeric")
  }

  dimension <- if(num.coords == 2) "XY" else "XYZ"

  # correct 3D EPSG 4258, 4326, 27700 if needed
  if (epsg %in% epsgs[epsgs$dimension=="XY/Z", "epsg"] && dimension =="XYZ") {
    if (epsg == 4258) {
      epsg <- 4937
    } else if (epsg == 4326) {
      epsg <- 4979
    } else if (epsg == 27700) {
      epsg <- 7405
    }
  }


  if (dimension == "XY") {
    point.coords <- list(x = x[[coords[1]]],
                         y = x[[coords[2]]])
  } else {
    point.coords <- list(x = x[[coords[1]]],
                         y = x[[coords[2]]],
                         z = x[[coords[3]]])
  }

  other.columns <- x[!(names(x) %in% coords)]

  if (length(other.columns)==0) {
    other.columns <- NULL
  } else {
    # if other.columns contains the column names 'x', 'y' or 'z' we will remove
    # them and warn the user about it.
    old.length <- length(other.columns)
    cols.to.check <- if(dimension == "XY") c("x", "y") else c("x", "y", "z")
    other.columns <- other.columns[!(names(other.columns) %in% cols.to.check)]
    if (length(other.columns) != old.length) {
      warning(paste0("Any column from input data named ",
                     paste(cols.to.check, collapse = ", "),
                     " has been removed"))
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

  points <- structure(c(other.columns, point.coords,
                        epsg=epsg,
                        datum=epsgs[epsgs$epsg==epsg, "datum"],
                        dimension=dimension),
                      class="sgs_points")

}

#' @export
sgs_points.data.frame <- function (x, coords=NULL, epsg=NULL) {

  # Checks
  cols <- ncol(x)
  if (cols < 2) stop("This method accepts dataframes with at least 2 columns")

  if(is.null(epsg) || (!epsg %in% epsgs[, "epsg"])) {
    stop("'epsg' must be entered as one of the accepted numbers")
  }

  # the names we apply don't matter for the rest of the function
  # when there are only 2 columns
  if (cols == 2) {
    coords <- coordinates.names[1, 1:cols] #just because
    names(x) <- coords
  }

  sgs_points(as.list(x), coords=coords, epsg=epsg)

}

#' @export
sgs_points.matrix <- function (x, coords=NULL, epsg=NULL) {

  # Checks
  cols <- ncol(x)
  if (cols < 2) stop("This method accepts matrices with at least 2 columns")

  if(is.null(epsg) || (!epsg %in% epsgs[, "epsg"])) {
    stop("'epsg' must be entered as one of the accepted numbers")
  }

  # the names we apply don't matter for the rest of the function
  # when there are only 2 columns
  if (cols == 2) {
    coords <- coordinates.names[1, 1:cols] #just because
    colnames(x) <- coords
  }

  lst <- split(x, rep(seq_len(ncol(x)), each = nrow(x)))
  names(lst) <- colnames(x)
  sgs_points(lst, coords=coords, epsg=epsg)

}

#' @encoding UTF-8
#' @title Extracts coordinates from an \code{sgs_points} object
#'
#' @description
#' Extract the coordinates of an \code{sgs_points} object expressed as a matrix.
#'
#' @name sgs_points_xy-deprecated
#' @usage sgs_points_xy(x)
#' @param x An instance of \code{sgs_points}.
#' @return
#' A matrix with 2 named columns.
#' @seealso \code{\link{sgs-deprecated}}
#' @keywords internal
#' @examples
#' \dontrun{
#' p <- sgs_points(list(57.47777, -4.22472), epsg=4326)
#' coords <- sgs_points_xy(p)
#' }
NULL

#' @rdname sgs-deprecated
#' @section \code{sgs_points_xy}:
#' For \code{sgs_points_xy}, use \code{\link{sgs_coordinates}}.
#'
#' @export
sgs_points_xy <- function (x) UseMethod("sgs_points_xy")

#' @export
sgs_points_xy.sgs_points <-function(x) {
  .Deprecated("sgs_coordinates")

  coords <- c("x", "y")

  xy <- matrix(unlist(x[, c("x", "y"), drop=TRUE]), ncol=2)
  colnames(xy) <- coords

  xy

}

#' @encoding UTF-8
#' @title Extracts coordinates from an \code{sgs_points} object
#'
#' @description
#' Extract the coordinates of an \code{sgs_points} object expressed as a matrix.
#'
#' @name sgs_coordinates
#' @usage sgs_coordinates(x)
#' @param x An instance of \code{sgs_points}.
#' @return
#' A matrix with 2 or 3 named columns.
#' @examples
#' p <- sgs_points(list(57.47777, -4.22472), epsg=4326)
#' coords <- sgs_coordinates(p)
#'
#' @export
sgs_coordinates <- function (x) UseMethod("sgs_coordinates")

#' @export
sgs_coordinates.sgs_points <- function(x) {

  coords <- if (x$dimension == "XY") c("x", "y") else c("x", "y", "z")

  xyz <- matrix(unlist(x[, coords, drop=TRUE]), ncol=length(coords))
  colnames(xyz) <- coords

  xyz

}

# TODO
# Extending '[' function to support sgs_points:
#' @name sgs_points
#' @param i Record selection, see \link[base]{Extract}
#' @param j Variable selection, see \link[base]{Extract}
#' @param drop Logical variable, default \code{FALSE}. If \code{TRUE} it will
#' drop the \code{sgs_points} class of the object.
#' @param ... Not currently used
#' @details \code{[.sgs_points} will return a \code{sgs_points} object or a
#' \code{data.frame} if any of the coordinates columns is dropped ; \code{...}
#' arguments are not currently used.
# @examples
# g = st_sfc(st_point(1:2), st_point(3:4))
# s = st_sf(a=3:4, g)
# s[1,]
# class(s[1,])
# s[,1]
# class(s[,1])
# s[,2]
# class(s[,2])
# g = st_sf(a=2:3, g)
# pol = st_sfc(st_polygon(list(cbind(c(0,3,3,0,0),c(0,0,3,3,0)))))
# h = st_sf(r = 5, pol)
# g[h,]
#' @export
`[.sgs_points` <- function(x, i, j, drop=FALSE, ...) {

  coords <- if (x$dimension == "XY") c("x", "y") else c("x", "y", "z")
  other.cols <- names(x)[!names(x) %in% sgs_points.core]
  selected.columns <- c(coords, other.cols)
  epsg <- x$epsg
  class(x) <- NULL

  if (!missing(i)) {
    x <- lapply(x[selected.columns], function(p) p[i])
  }

  if (!missing(j)) {
    # remove any reference to "epsg", "datum", "dimension"
    if (is.character(j)) {
      j <- setdiff(j, setdiff(sgs_points.core, c("x", "y", "z")))
    }
    x <- x[j]
    selected.columns <- names(x[j])
  }

  if (all(coords %in% selected.columns) && !drop) {
    if(is.matrix(x)){
      x <- split(x, rep(seq_len(ncol(x)), each = nrow(x))) #to list
      names(x) <- selected.columns
    } else {
      names(x) <- selected.columns
      x <- as.list(x)
    }
    x <- sgs_points(x, coords=coords, epsg=epsg)
  } else {
    x <- data.frame(x, stringsAsFactors = FALSE)
  }

  x

}

#TODO export
# does this actually make sense?? other.columns should be the same? and the number of points? etc
#c.sgs_points <- function(..., recursive = FALSE) {
c.sgs_points <- function(...) {

  dots <- list(...)
  classes <- vapply(dots, class, character(1))

  if (all(classes == "sgs_points")) { #merge them

    #all equal
    if (length(unique(lapply(dots, `[`, "epsg"))) == 1) {

      coord.cols <- if (dots[[1]]$dimension == "XY") c("x", "y")
                      else c("x", "y", "z")
      coords <- do.call(mapply, c(FUN=c, lapply(dots, `[`, coord.cols)))
      coords <- split(coords, col(coords))

      other.columns <- unlist(sapply(dots, function(x, n) {x[!names(x) %in% n]}, #try to substitute this sapply with vapply (probably not posible as every list would have different size) or lapply: is lapply safer than sapply?
                                     n=sgs_points.core), recursive=FALSE)

      r <- structure(c(other.columns,
                       list(coords, epsg=dots[[1]]$epsg,
                            datum=dots[[1]]$datum,
                            dimension=dots[[1]]$dimension)),
                     class = "sgs_points")

    } else { #different EPSG's, we cannot merge

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
#If you’re implementing more complicated print() methods, it’s a better idea to
#implement format() methods that return a string, and then implement
#print.class <- function(x, ...) cat(format(x, ...), "\n". This makes for methods
#that are much easier to compose, because the side-effects are isolated to a single place.
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

#TODO
#create a: is.XXX <- function(x) inherits(x, "XXX") to check if an objects is myclass
#implement: length, [<-, [[, [[<-, c. (If [ is implemented rev, head, and tail should all work).
#implement Summary?
#implement cbind, rbind

#work with EPSGs
#4277 (only 2D), 27700, 7405 (assume it works for ODN heights, so show datum flag in output!) -> OSGB36
#4258, 4937, 4936 (assume h=0 when XY only) -> ETRS89
#4326, 4979, 4978 (assume h=0 when XY only) -> WGS84
#3857 -> Pseudo-Mercator
