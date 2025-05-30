#' @encoding UTF-8
#' @title Object containing 2D or 3D point coordinates
#'
#' @description
#' 2D or 3D coordinates (and other attributes) of a point or collection of
#' points
#'
#' @name sgo_points
#' @usage sgo_points(x, coords = NULL, epsg = NULL)
#' @param x A matrix, list or dataframe with at least 2 columns of either
#' easting/northing or longitude/latitude coordinates per row. A column with
#' height values is optional.
#' \strong{Please note} that the order is important when \code{x} has only 2 or
#' 3 columns and \code{coords} is not informed: lat/lon or northing/easting
#' (and height) will produce wrong results.
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
#' coordinates.
#' Currently it only supports the following \code{epsg}s:
#' \itemize{
#' \item\code{4258}: ETRS89, geodetic coordinate system. The columns in
#' \code{x} must be defined as Longitude and Latitude (\code{sgo} also accepts
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
#' datum for this set of coordinates is ETRS89 (https://epsg.io/3035)
#'
#' \item\code{4326}: WGS84, geodetic coordinate system. The columns in \code{x}
#' must be defined as Longitude and Latitude (\code{sgo} also accepts a
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
#' used for historical reasons and to convert only to or from BNG coordinates.}
#' \strong{Height values will be discarded when working with this coordinate
#' system.}
#'
#' \item\code{27700}: British National Grid, projected coordinate system. The
#' columns in \code{x} must be defined as Easting and Northing (\code{sgo} also
#' accepts a third column for orthometric heights). The defined datum for this
#' set of coordinates is OSGB 1936 (https://epsg.io/27700).
#'
#' \item\code{7405}: British National Grid, projected coordinate system. The
#' columns in \code{x} must be defined as Easting, Northing and ODN Orthometric
#' height respectively (\code{sgo} accepts heights from other datums like
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
#' An object of class \code{sgo_points}. This object is a actually a list with
#' class \code{sgo_points} and at least 5 elements (or 6 elements if it is 3D):
#' \itemize{
#' \item\code{x}: A numeric vector containing easting or longitude coordinates.
#' \item\code{y}: A numeric vector with northing or latitude coordintes.
#' \item\code{z}: A numeric vector with height values when the object is 3D.
#' \item\code{epsg}: A scalar value with the EPSG code of the current
#' Geographic Coordinate System (GCS).
#' \item\code{datum}: A string describing the geodetic datum that defines the
#' GCS of the object. Currently can take the values "OSGB36", "WGS84" or
#' "ETRS89"
#' \item\code{dimension}: A string describing whether the object is 2D or 3D.
#' It can take the values "XY" or "XYZ".
#' }
#' @seealso \code{\link{sgo_coordinates}}, \code{\link{sgo_transform}}.
#' @examples
#' # lists:
#' p1 <- sgo_points(list(-3.9369, 56.1165), epsg=4326)
#' lon <- c(-4.25181,-3.18827)
#' lat <- c(55.86424, 55.95325)
#' p2 <- sgo_points(list(longitude=lon, latitude=lat), epsg=4326)
#' #p3 will fill up the list 'desc' with NA's to have the same number of
#' #elements as coordinates in the list:
#' p3 <- sgo_points(list(longitude=lon, latitude=lat, desc="c1"),
#'                  coords=c("longitude", "latitude"), epsg=4326)
#' # dataframe:
#' ln <- c(-4.22472, -2.09908)
#' lt <- c(57.47777, 57.14965)
#' n <- c("Inverness", "Aberdeen")
#' df <- data.frame(n, ln, lt, stringsAsFactors = FALSE)
#' p4 <- sgo_points(df, coords=c("ln", "lt"), epsg=4326)
#'
#' # plotting on a map:
#' if (require(maps)) {
#'   map('world', regions=('uk'), xlim=c(-9, 0), ylim=c(54.5, 60.9))
#'   points(x=p1$x, y=p1$y, pch=0, col="green") #Stirling
#'   points(p4, pch=0, col="red")
#'   text(p4, labels=p4$n, pos=1, cex=0.9)
#' }
#' @export
sgo_points <- function (x, coords=NULL, epsg=NULL)
  UseMethod("sgo_points")

#' @export
sgo_points.list <- function (x, coords=NULL, epsg=NULL) {

  # Checks
  len <- length(x)
  if (len < 2) stop("This method accepts lists with at least 2 elements")

  if (is.null(epsg) || (!epsg %in% .epsgs$epsg)) {
    stop("'epsg' must be entered as one of the accepted numbers")
  }

  # don't try to guess too much the coords...
  if (is.null(coords)) {
    if ((epsg %in% .epsgs[.epsgs$dimension %in% c("XY", "XY/Z"), "epsg"]
         && len == 2) ||
        (epsg %in% .epsgs[.epsgs$dimension == "XYZ", "epsg"]
         && len == 3)){
      coords <- .coordinates.names[1, 1:len] #c(x,y,(z)) just because
      names(x) <- coords
    }
  }

  x.names <- names(x)
  if (is.null(x.names) || any(x.names == "")) {
    stop("All elements in 'x' must be named")
  }

  if (!is.null(coords) && !all(coords %in% x.names)) {
    stop("'x' must include all the coordinates defined in 'coords'")
  }

  if (is.null(coords)) {
    stop("Must specify the coordinate columns using the 'coords' parameter")
  }

  num.coords <- length(coords)

  # check those epsgs that need 3 coordinates and there are only 2 defined
  if (epsg %in% .epsgs[.epsgs$dimension=="XYZ", "epsg"] && num.coords < 3) {
    stop("Wrong number of coordinates for the the specified 'epsg'")
  }

  for (i in 1:num.coords) {
    if (!is.numeric(x[[coords[i]]]) )
      stop("All coordinates must be numeric")
  }

  dimension <- if(num.coords == 2) "XY" else "XYZ"

  # correct 3D EPSG 4258, 4326, 27700 if needed
  if (epsg %in% .epsgs[.epsgs$dimension=="XY/Z", "epsg"] && dimension =="XYZ") {
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

  other.columns <- x[!(x.names %in% coords)]

  if (length(other.columns)==0) {
    other.columns <- NULL
  } else {
    # if other.columns contains column names from sgo_points.core, rename them
    # and warn the user about it.

    if(dimension == "XY") {
      cols.to.check <- .sgo_points.2d.core
    } else  {
      cols.to.check <- .sgo_points.3d.core
    }

    to.rename <- names(other.columns) %in% cols.to.check
    if (any(to.rename)) {
      o.names <- names(other.columns)[to.rename]
      names(other.columns)[to.rename] <- paste0(o.names, ".1")
      warning(paste0("The column(s) from input data named ",
                     paste(o.names, collapse = ", "),
                     " has(have) been renamed appending the suffix '.1'"))
    }


    # all additional columns will be expanded to contain the same number of
    # elements as coordinates in the object
    max.len <- length(x[[coords[1]]])
    other.columns <- lapply(other.columns,
                            function(x) { length(x) <- max.len; x })
  }

  structure(c(point.coords, other.columns, epsg=epsg,
              datum=.epsgs[.epsgs$epsg==epsg, "datum"],
              dimension=dimension),
            class="sgo_points")

}

#' @export
sgo_points.data.frame <- function (x, coords=NULL, epsg=NULL) {

  # Checks
  cols <- ncol(x)
  if (cols < 2) stop("This method accepts dataframes with at least 2 columns")

  if(is.null(epsg) || (!epsg %in% .epsgs$epsg)) {
    stop("'epsg' must be entered as one of the accepted numbers")
  }

  # the names we apply don't matter for the rest of the function
  # when there are only 2 columns
  if (cols == 2) {
    coords <- .coordinates.names[1, 1:cols] #just because
    names(x) <- coords
  }

  sgo_points(as.list(x), coords=coords, epsg=epsg)

}

#' @export
sgo_points.matrix <- function (x, coords=NULL, epsg=NULL) {

  # Checks
  cols <- ncol(x)
  if (cols < 2) stop("This method accepts matrices with at least 2 columns")

  if(is.null(epsg) || (!epsg %in% .epsgs$epsg)) {
    stop("'epsg' must be entered as one of the accepted numbers")
  }

  # the names we apply don't matter for the rest of the function
  # when there are only 2 columns
  if (cols == 2) {
    coords <- .coordinates.names[1, 1:cols] #just because
    colnames(x) <- coords
  }

  lst <- lapply(seq_len(ncol(x)), function(i) x[, i])
  names(lst) <- colnames(x)
  sgo_points(lst, coords = coords, epsg = epsg)

}

#' @encoding UTF-8
#' @title Extracts coordinates from an \code{sgo_points} object
#'
#' @description
#' Extract the coordinates of an \code{sgo_points} object expressed as a matrix.
#'
#' @name sgo_coordinates
#' @usage sgo_coordinates(x, names.xyz = NULL, as.latlon = FALSE,
#' ll.format=NULL)
#' @param x An instance of \code{sgo_points}.
#' @param names.xyz Character vector. New names for the columns x, y and
#' possibly z of the object \code{x}.
#' @param as.latlon Logical variable. When \code{x} is defined in a geodetic
#' coordinate system as lon/lat and this parameter is set to \code{TRUE} then
#' it returns the coordinates ordered as lat/lon.
#' @param ll.format Character variable. Applies a format to the returned
#' coordinates when \code{x} is defined in a geodetic coordinate system. As of
#' now it only accepts \code{DMS}, which will return strings of
#' coordinates formatted as degrees, minutes and seconds (certain accuracy will
#' be lost because seconds are rounded to the second decimal).
#' @return
#' A matrix with 2 or 3 named columns.
#' @examples
#' p <- sgo_points(list(57.47777, -4.22472), epsg=4326)
#' coords <- sgo_coordinates(p)
#'
#' @export
sgo_coordinates <- function (x, names.xyz=NULL, as.latlon=FALSE, ll.format=NULL)
  UseMethod("sgo_coordinates")

#' @export
sgo_coordinates.sgo_points <- function(x, names.xyz=NULL, as.latlon=FALSE,
                                       ll.format=NULL) {

  if(x$dimension == "XY") {
    coords <- .sgo_points.2d.coords
    cols <- 2
  } else  {
    coords <- .sgo_points.3d.coords
    cols <- 3
  }

  is.ll <- .epsgs[.epsgs$epsg == x$epsg, "format"] == "ll"

  # set the new names for the coordinates columns
  if (!is.null(names.xyz)) {
    if (length(names.xyz) > length(coords)) {
      dim.names <- replace(coords, values = names.xyz[1:length(coords)])
    } else {
      dim.names <- replace(coords, 1:length(names.xyz), names.xyz)
    }
  } else {
    dim.names <- coords
  }

  if (is.ll) {
    if (as.latlon) {
      # change order of coords and dim.names
      coords <- c(coords[2], setdiff(coords, coords[2]))
      dim.names <- c(dim.names[2], setdiff(dim.names, dim.names[2]))
    }
    vec.coords <- unlist(x[coords], use.names = FALSE)
    len.vec <- length(vec.coords)
    if (!is.null(ll.format)) {
      if(ll.format == "DMS") {
        if (cols == 3) {
          vec.coords[1:(len.vec/3*2)] <- .dd.to.dms(vec.coords[1:(len.vec/3*2)],
                                                    as.latlon, 2)
        } else {
          vec.coords <- .dd.to.dms(vec.coords, as.latlon, 2)
        }
      }
    }
  } else {
    vec.coords <- unlist(x[coords], use.names = FALSE)
  }

  matrix(vec.coords, ncol = cols, byrow = FALSE,
         dimnames = list(NULL, dim.names))

}


#' @rdname sgo_points
#' @param ... Further arguments passed to or from other methods,
#' see \link{print}, \link{as.data.frame} or \link{as.list} .
#' @param n Maximum number of features to print.
#' @export
print.sgo_points <- function(x, ..., n = 6L) {

  len <- length(x$x)
  if (n >= len) {
    msg <- ""
    n <- len
  } else {
    msg <- paste("\nFirst", n, ifelse(n == 1L, "feature:", "features:"))
  }

  # print coordinates always first
  x.2d <- x$dimension == "XY"
  if (x.2d) {
    coords <- .sgo_points.2d.coords
    print.cols <- c(coords, setdiff(names(x), .sgo_points.2d.core))
  } else {
    coords <- .sgo_points.3d.coords
    print.cols <- c(coords, setdiff(names(x), .sgo_points.3d.core))
  }

  num.fields <- length(print.cols) - ifelse(x.2d, 2L, 3L)
  and <- paste("and", num.fields, ifelse(num.fields == 1L, "field", "fields"))
  cat("An sgo object with", len, ifelse(len == 1L,
                                      "feature (point)", "features (points)"),
      if (num.fields == 0L) NULL else and,
      "\ndimension:", x$dimension,
      "\nEPSG:     ", x$epsg,
      msg, "\n")

  print.data.frame(as.data.frame(lapply(x[print.cols],
                                        function(l) l[1:n])), ...)
  invisible(x)

}


#' @rdname sgo_points
#' @param row.names	NULL or a character vector giving the row names for the
#' data frame. Missing values are not allowed.
#' @param optional Logical. See \link{as.data.frame}
#' @export
as.data.frame.sgo_points <- function(x,
                                     row.names = NULL, optional = FALSE, ...) {

  col.names <- setdiff(names(x), .sgo_points.attr)
  as.data.frame.list(x[col.names], row.names = row.names, optional = optional,
                     col.names = col.names)

}


#' @rdname sgo_points
#' @export
as.list.sgo_points <- function(x, ...) {

  x[setdiff(names(x), .sgo_points.attr)]

}


# dd.coords is a vector of coordinates
# as.latlon is a logical value.
# num.decimals is a hscalar value.
.dd.to.dms <- function(coords, as.latlon, num.decimals = 0) {

  # a typical tolerance: tol = sqrt(.Machine$double.eps)
  tol <- 60

  len <- length(coords)
  signs <- coords < 0
  if (as.latlon) {
    letters <- c(ifelse(signs[1:(len/2)], "S", "N"),
                 ifelse(signs[(len/2+1):len], "W", "E"))
  } else {
    letters <- c(ifelse(signs[1:(len/2)], "W", "E"),
                 ifelse(signs[(len/2+1):len], "S", "N"))
  }

  coords <- abs(coords)
  d <- trunc(coords)
  ms <- (coords - d) * 60
  m <- trunc(ms)
  s <- round((ms - m) * 60, num.decimals)

  above.tol <- s != tol
  s <- ifelse(above.tol, s, 0)
  m <- ifelse(above.tol, m, m + 1)
  keep.min <- as.integer(m) < 60L
  m <- ifelse(keep.min, m, 0)
  d <- ifelse(keep.min, d, d + 1)

  sprintf("%d%s %d%s %.*f%s %s", d, "\U00B0", m, "\U2032",
          num.decimals, s, "\U2033", letters)
  #sprintf("%d%s %d%s %.*f%s %s", d, "\U00B0", m, "\U2032",
  #        num.decimals, trunc(s * 10^num.decimals) / 10^num.decimals, "\U2033",
  #        letters)
  # Not using num.decimals:
  #sprintf("%d%s %d%s %.*f%s %s", d, "\U00B0", m, "\U2032",
  #        num.decimals, trunc(s), "\U2033", letters)

}
