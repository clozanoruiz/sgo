#' sgs: SEPA's GIS In Spotfire (Spatial Analysis).
#'
#' The sgs package provides a set of functions to perform spatial or geographic
#' analysis using TERR and Spotfire.
#'
#' @encoding UTF-8
#' @section Object contructors:
#' \itemize{
#' \item\code{\link{sgs_points}}: 2D point coordinates
#' \item\code{\link{sgs_polygons}}: \code{sf} polygons.
#' }
#'
#' @section Transformation and Conversion functions:
#' Functions to provide coordinate transformations between:
#' \itemize{
#' \item\code{\link{sgs_bng_latlon}}: British National Grid (E/N) and Lat/Lon
#' \item\code{\link{sgs_latlon_bng}}: Lat/Lon and British National Grid (E/N)
#' \item\code{\link{sgs_bng_ngr}}: British National Grid (E/N) and National Grid References
#' \item\code{\link{sgs_ngr_bng}}: National Grid References and British National Grid (E/N)
#' \item\code{\link{sgs_wgs84_en}}: WGS84 Lat/Lon and Pseudo-Mercator (E/N)
#' \item\code{\link{sgs_en_wgs84}}: Pseudo-Mercator (E/N) and WGS84 Lat/Lon
#' \item\code{\link{sgs_transform}}: Wrapper for all the transformations above
#' }
#'
#' Functions to convert between data formats:
#' \itemize{
#' \item\code{\link{sgs_points_sf}}: Conversion from sgs_points to sf
#' }
#'
#' @section Disclaimer:
#' The OSTN15 transformation model is used in this package, and it is licensed under
#' the BSD Licence (http://opensource.org/licenses/bsd-license.php):\cr
#' \emph{© Copyright and database rights Ordnance Survey Limited 2016, © Crown copyright
#' and database rights Land & Property Services 2016 and/or © Ordnance Survey Ireland, 2016.
#' All rights reserved.}
#' @docType package
#' @name sgs
NULL


## COMMON HELPER OBJECTS AND FUNCTIONS ##

#check 'empty' values in a vector
is_nothing <- function(x) {
  is.null(x) | is.na(x) | is.nan(x) | x == ""
}

# list of EPSG codes and types #
epsgs <- data.frame(epsg=c(4326, 3857, 4277, 27700, 4258),
                    datum=c("WGS84", "WGS84", "OSGB36", "OSGB36", "ETRS89"),
                    type=c("GCS","PCS", "GCS","PCS", "GCS"),
                    stringsAsFactors = FALSE)

# Vector with all the main elements a sgs_points object contains
# But actually: Easting or Latitude; Northing or Longitude
sgs_points.core <- c("easting", "northing",
                     "latitude", "longitude",
                     "epsg", "datum")
coordinates.names <- cbind(x=c("x", "lat", "latitude", "easting"),
                           y=c("y", "lon","longitude", "northing"))


# Ellipsoid parameters; major axis (a), minor axis (b), and flattening (f) for each ellipsoid.
# For all practical applications GRS80 abd WGS84 ellipsoids are identical
latlon.ellipsoid <- data.frame(ellipsoid=c("WGS84", "Airy1830","GRS80"),
                               a=c(6378137.000, 6377563.396, 6378137.000),
                               b=c(6356752.31424518, 6356256.909, 6356752.314140),
                               f=c(298.257223563, 299.3249612665, 298.257222101),
                               e2=c(0.006694379990141316996137233540,
                                    0.0066705400741492318211148938735613129751683486352306,
                                    0.006694380022900787625359114703),
                               stringsAsFactors = FALSE)


# Datums, with associated ellipsoid, and Helmert transform parameters to convert from WGS84 into
# given datum.
# Note that precision of various datums will vary, and WGS84 (original) is not defined to be
# accurate to better than ±1 metre. No transformation should be assumed to be accurate to better
# than a meter; for many datums somewhat less (OSGB36 to WGS84 or ETRS89 the lost of accuracy can
# be up to 5m with single Helmert transformations.
# transforms: t in metres, s in ppm, r in arcseconds
latlon.datum <- data.frame(datum=c("OSGB36", "WGS84", "ETRS89"),
                           ellipsoid=c("Airy1830","WGS84", "GRS80"),
                           tx=c(-446.448, 0L, 0L),
                           ty=c(125.157, 0L, 0L),
                           tz=c(-542.060, 0L, 0L),
                           s=c(0.0000204894, 0L, 0L),
                           rx=c(-0.1502, 0L, 0L),
                           ry=c(-0.2470, 0L, 0L),
                           rz=c(-0.8421, 0L, 0L),
                           stringsAsFactors = FALSE)



#' @encoding UTF-8
#' @title Coordinates transformation of a set of points
#'
#' @description
#' Transforms the coordinate system of a set of points to any supported coordinate system.
#'
#' @name sgs_transform
#' @usage sgs_transform(x, to = NULL)
#' @param x A \code{sgs_points} object.
#' @param to Specifies the EPSG code to convert the coordinates to. Currently it
#' can take any of the following values: \code{4326}, \code{3857}, \code{4277},
#' \code{27700} or \code{4258}.
#' @details
#' This function is a wrapper of specific transformation functions (\code{\link{sgs_bng_latlon}},
#' \code{\link{sgs_en_wgs84}}, \code{\link{sgs_latlon_bng}}, \code{\link{sgs_wgs84_en}})
#' that transforms the coordinate system of a set of points to any of the supported coordinate systems.
#' @return
#' An object of class 'sgs_points'.
#' @seealso \code{\link{sgs_points}}, \code{\link{sgs_points_xy}},
#' \code{\link{sgs_set_gcs}}, \code{\link{sgs_bng_ngr}}.
#' @examples
#' lt <- c(57.47777, 57.14965)
#' ln <- c(-4.22472, -2.09908)
#' n <- c("Inverness", "Aberdeen")
#' df <- data.frame(n, lt, ln, stringsAsFactors = FALSE)
#' locations <- sgs_points(df, coords=c("lt", "ln"), epsg=4326)
#'
#' locations.bng <- sgs_transform(locations, to=27700)
#' locations.osgb36 <- sgs_transform(locations, to=4277)
#' locations.ngr <- sgs_bng_ngr(sgs_transform(locations, to=27700))
#' locations.wgs84EN <- sgs_transform(locations.bng, to=3857)
#' @export
sgs_transform <- function(x, to=NULL) UseMethod("sgs_transform")

#' @export
sgs_transform.sgs_points <- function(x, to=NULL) {

  if (is.null(to)) stop("Paramater 'to' must be specified")
  if (x$epsg==to) return(x)

  # Get the list of functions and their arguments that need to be run for this transformation
  FUN_list <- FUNCS[which(FUNCS$FROM==x$epsg), as.character(to)][[1]]
  ARGS_list <- ARGS[which(ARGS$FROM==x$epsg), as.character(to)][[1]]

  # Run functions sequentially over the input
  for (i in 1:length(FUN_list)) {
    x <- do.call(FUN_list[[i]], c(list(x), ARGS_list[[i]]))
  }

  x

}

#Dataframes of operations and their arguments
#from: 4326  to:    -, 3857, 4277, 27700, 4258
#from: 3857  to: 4326,    -, 4277, 27700, 42584
#from: 4277  to: 4326, 3857,    -, 27700, 4258
#from: 27700 to: 4326, 3857, 4277,     -, 4258
#from: 4258  to: 4326, 3857, 4277, 27700,    -

FROM <-          list(4326,                                 3857,                                               4277,                                               27700,                              4258)

FUN_TO_4326 <-   list(NULL,                                 list(sgs_en_wgs84),                                 list(sgs_latlon_bng, sgs_bng_latlon),               list(sgs_bng_latlon),               list(sgs_set_gcs))
FUN_TO_3857 <-   list(list(sgs_wgs84_en),                   NULL,                                               list(sgs_latlon_bng, sgs_bng_latlon, sgs_wgs84_en), list(sgs_bng_latlon, sgs_wgs84_en), list(sgs_set_gcs, sgs_wgs84_en))
FUN_TO_4277 <-   list(list(sgs_latlon_bng, sgs_bng_latlon), list(sgs_en_wgs84, sgs_latlon_bng, sgs_bng_latlon), NULL,                                               list(sgs_bng_latlon),               list(sgs_latlon_bng, sgs_bng_latlon))
FUN_TO_27700 <-  list(list(sgs_latlon_bng),                 list(sgs_en_wgs84, sgs_latlon_bng),                 list(sgs_latlon_bng),                               NULL,                               list(sgs_latlon_bng))
FUN_TO_4258 <-   list(list(sgs_set_gcs),                    list(sgs_en_wgs84, sgs_set_gcs),                    list(sgs_latlon_bng, sgs_bng_latlon),               list(sgs_bng_latlon),               NULL)

ARGS_TO_4326 <-  list(NULL,                                 list(4326),                                         list(list(TRUE), list(4326, TRUE)),                 list(4326, TRUE),                   list(4326))
ARGS_TO_3857 <-  list(list(3857),                           NULL,                                               list(list(TRUE), list(4326, TRUE), list(3857)),     list(list(4326, TRUE), list(3857)), list(list(4326), list(3857)))
ARGS_TO_4277 <-  list(list(list(TRUE),list(4277, TRUE)),    list(list(4326), list(TRUE), list(4277, TRUE)),     NULL,                                               list(4277, TRUE),                   list(list(TRUE), list(4277, TRUE)))
ARGS_TO_27700 <- list(list(TRUE),                           list(list(4326), list(TRUE)),                       list(TRUE),                                         NULL,                               list(TRUE))
ARGS_TO_4258 <-  list(list(4258),                           list(list(4326), list(4258)),                       list(list(TRUE), list(4258, TRUE)),                 list(4258, TRUE),                   NULL)

# Two dataframes containing those funtions and arguments respectively
FUNCS <- as.data.frame(cbind("FROM"=FROM,
                             "4326"=FUN_TO_4326,
                             "3857"=FUN_TO_3857,
                             "4277"=FUN_TO_4277,
                             "27700"=FUN_TO_27700,
                             "4258"=FUN_TO_4258
                      ))

ARGS <- as.data.frame(cbind("FROM"=FROM,
                            "4326"=ARGS_TO_4326,
                            "3857"=ARGS_TO_3857,
                            "4277"=ARGS_TO_4277,
                            "27700"=ARGS_TO_27700,
                            "4258"=ARGS_TO_4258
                      ))

#' @encoding UTF-8
#' @title TODO
#'
#' @description
#' TODO.
#'
#' @name sgs_equal
#' @usage sgs_equal(x, y, tolerance = NULL, core.only=TRUE)
#' @param x A \code{sgs_points} object.
#' @param y A \code{sgs_points} object.
#' @param tolerance TODO. 2-elemntes vector to set coordinates differencfe tolerance
#' @param core.only TODO. if evaluate only core columns of sgs_points
#' @details
#' TODO.
#' @return
#' TODO. TRUE or a vector of mode 'character' describing the differences between x and y.
#' @export
sgs_equal <- function(x, y, tolerance=NULL, core.only=TRUE) UseMethod("sgs_equal")

#' @export
sgs_equal.sgs_points <- function(x, y, tolerance=NULL, core.only=TRUE) {

  if (class(x) != "sgs_points" && class(y) != "sgs_points")
    stop("Both x and y must be objects of class 'sgs_points'")

  if (x$epsg != y$epsg || x$datum != y$datum)
    return ("Different coordinate systems")

  # Projection type
  coord.system <- epsgs[epsgs[, "epsg"]==x$epsg, "type"]

  if (coord.system == "GCS") {
    core.cols <- sgs_points.core[!sgs_points.core %in% c("easting", "northing")]
    if(length(x$latitude) != length(y$latitude))
      return ("Different number of points")
    default.tolerance <- c(1/1100000000, 1/650000000) #lat/lon
  } else { #PCS
    core.cols <- sgs_points.core[!sgs_points.core %in% c("latitude", "longitude")]
    if(length(x$easting) != length(y$easting))
      return ("Different number of points")
    default.tolerance <- c(0.00001, 0.00001) #easting/northing
  }

  if (core.only) {
    x <- x[core.cols]
  }

  if (!core.only && (length(x) != length(y) || !identical(sort(names(x)), sort(names(y)))))
    return ("They have different columns")

  #use sgs_points_xy to compare all coordinates
  #if !core.only compare also the additional columns values

}
