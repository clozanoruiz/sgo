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
#' An object of class \code{sgs_points}. This object is a actually a list with
#' class \code{sgs_points} and 5 or 6 elements:
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
#' @seealso \code{\link{sgs_coordinates}}, \code{\link{sgs_transform}}.
#' @examples
#' # lists:
#' p1 <- sgs_points(list(-3.9369, 56.1165), epsg=4326)
#' lon <- c(-4.25181,-3.18827)
#' lat <- c(55.86424, 55.95325)
#' p2 <- sgs_points(list(longitude=lon, latitude=lat), epsg=4326)
#' #p3 will fill up the list 'desc' with NA's to have the same number of
#' #elements as coordinates in the list:
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
#' map('world', regions=('uk'), xlim=c(-9, 0), ylim=c(54.5, 60.9))
#' points(x=p1$x, y=p1$y, pch=0, col="green") #Stirling
#' points(p4, pch=0, col="red")
#' text(p4, labels=p4$n, pos=1, cex=0.9)
#' }
#' @export
sgs_points <- function (x, coords=NULL, epsg=NULL)
  UseMethod("sgs_points")

#' @export
sgs_points.list <- function (x, coords=NULL, epsg=NULL) {

  # Checks
  len <- length(x)
  if (len < 2) stop("This method accepts lists with at least 2 columns")

  if(is.null(epsg) || (!epsg %in% .epsgs$epsg)) {
    stop("'epsg' must be entered as one of the accepted numbers")
  }

  if (len < 4 && is.null(names(x)) ) {
    coords <- .coordinates.names[1, 1:len] #c(x,y,(z)) just because
    names(x) <- coords
  }

  if(is.null(coords)) {
    lnames <- tolower(names(x))
    known.coords <- .coordinates.names[match(lnames, .coordinates.names)]
    known.coords <- known.coords[!is.na(known.coords)]
    len.known <- length(known.coords)
    if (len.known < 4 && any(apply(.coordinates.names, 1,
                                   function(n,x) all(n[1:len.known]==x),
                                   x=known.coords))) {
      coords <- names(x)[lnames %in% known.coords]
    } else {
      stop("Must specify the coordinate columns using the 'coords' parameter")
    }
  }

  num.coords <- length(coords)

  #check those epsgs that ONLY admit 2 and have 3, and viceversa
  if ((epsg %in% .epsgs[.epsgs$dimension=="XY", "epsg"] && num.coords > 2) ||
      (epsg %in% .epsgs[.epsgs$dimension=="XYZ", "epsg"] && num.coords < 3)) {
    stop("Wrong number of coordinates for the The specified 'epsg'")
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

  other.columns <- x[!(names(x) %in% coords)]

  if (length(other.columns)==0) {
    other.columns <- NULL
  } else {
    # if other.columns contains column names from sgs_points.core, rename them
    # and warn the user about it.

    if(dimension == "XY") {
      cols.to.check <- .sgs_points.2d.core
    } else  {
      cols.to.check <- .sgs_points.3d.core
    }

    to.rename <- names(other.columns) %in% cols.to.check
    names(other.columns)[to.rename] <- paste0(
      names(other.columns)[to.rename], ".2")

    if (any(to.rename))
      warning(paste0("All columns from input data named ",
                     paste(cols.to.check, collapse = ", "),
                     " have been renamed appending the suffix '.2'"))

    # all additional columns will be expanded to contain the same number of
    # elements as coordinates in the object
    max.len <- length(x[[coords[1]]])
    other.columns <- lapply(other.columns,
                            function(x) { length(x) <- max.len; x })
  }

  structure(c(point.coords, other.columns, epsg=epsg,
              datum=.epsgs[.epsgs$epsg==epsg, "datum"],
              dimension=dimension),
            class="sgs_points")

}

#' @export
sgs_points.data.frame <- function (x, coords=NULL, epsg=NULL) {

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

  sgs_points(as.list(x), coords=coords, epsg=epsg)

}

#' @export
sgs_points.matrix <- function (x, coords=NULL, epsg=NULL) {

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
  sgs_points(lst, coords = coords, epsg = epsg)

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

  coords <- .sgs_points.2d.coords
  matrix(unlist(x[coords], use.names = FALSE), ncol = 2, byrow = FALSE,
         dimnames = list(NULL, coords))

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

  if(x$dimension == "XY") {
    coords <- .sgs_points.2d.coords
    cols <- 2
  } else  {
    coords <- .sgs_points.3d.coords
    cols <- 3
  }
  matrix(unlist(x[coords], use.names = FALSE), ncol = cols, byrow = FALSE,
         dimnames = list(NULL, coords))

}

#TODO (docs?)
#' @export
as.data.frame.sgs_points <- function(x, row.names = NULL, optional = FALSE,
      ..., cut.names = FALSE, col.names = names(x), fix.empty.names = TRUE,
      stringsAsFactors = default.stringsAsFactors()) {

  class(x) <- setdiff(class(x), "sgs_points")
  col.names <- setdiff(col.names, .sgs_points.attr)

  as.data.frame.list(x[col.names], row.names = row.names, optional = optional,
                     ..., cut.names = cut.names, col.names = col.names,
                     fix.empty.names = fix.empty.names,
                     stringsAsFactors = stringsAsFactors)

}


#TODO (docs?)
#' @export
print.sgs_points <- function(x, ..., n = 6L) {

  len <- length(x$x)
  if (n >= len) {
    msg <- ""
    n <- len
  } else {
    msg <- paste("\nFirst", n, "features:")
  }

  # print coordinates always first
  x.2d <- x$dimension == "XY"
  if (x.2d) {
    coords <- .sgs_points.2d.coords
    print.cols <- c(coords, setdiff(names(x), .sgs_points.2d.core))
  } else {
    coords <- .sgs_points.3d.coords
    print.cols <- c(coords, setdiff(names(x), .sgs_points.3d.core))
  }

  num.fields <- length(print.cols) - ifelse(x.2d, 2L, 3L)
  cat("An sgs object with", n, ifelse(n == 1L,
                                      "feature (point)", "features (points)"),
      "and", num.fields, ifelse(num.fields == 1L, "field", "fields"),
      "\ndimension:", x$dimension,
      "\nEPSG:     ", x$epsg,
      msg, "\n")

  print.data.frame(as.data.frame(lapply(x[print.cols], function(l) l[1:n])))
  #invisible(x)

}
