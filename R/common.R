## This modelue contains 'common' and helper functions


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


# Helper function. Extract last n characters from a string
substr.r <- function(x, n){
  substr(x, nchar(x) - n+1, nchar(x))
}
