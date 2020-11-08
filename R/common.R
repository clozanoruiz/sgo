## This modelue contains 'common' and helper functions


#check 'empty' values in a vector
is_nothing <- function(x) {
  is.null(x) | is.na(x) | is.nan(x) | x == ""
}


# list of EPSG codes and types #,
# we could admit some epsg which should be XY as XYZ (eg. 4258, 4326, 27700)
# format is either ll (lon/lat), c (cartesians), en (easting/northing)
epsgs <- data.frame(epsg=c(4258, 4937, 4936, 3035,
                           4326, 4979, 4978, 3857,
                           4277, 27700, 7405),
                    datum=c("ETRS89", "ETRS89", "ETRS89", "ETRS89",
                            "WGS84", "WGS84", "WGS84", "WGS84",
                            "OSGB36", "OSGB36", "OSGB36"),
                    type=c("GCS", "GCS", "GCS", "PCS",
                           "GCS", "GCS", "GCS", "PCS",
                           "GCS", "PCS", "PCS"),
                    dimension=c("XY/Z", "XYZ", "XYZ", "XY",
                                "XY/Z", "XYZ", "XYZ", "XY",
                                "XY", "XY/Z", "XYZ"),
                    format=c("ll", "ll", "c", "en",
                             "ll", "ll", "c", "en",
                             "ll", "en", "en"),
                    stringsAsFactors = FALSE)


# Vector with all the main elements a sgs_points object contains
sgs_points.core <- c("x", "y", "z", "epsg", "datum", "dimension")

coordinates.names <- cbind(x=c("x", "lon", "longitude", "easting"),
                           y=c("y", "lat","latitude", "northing"),
                           z=c("z", "H", "h","height"))


# Ellipsoid parameters; major axis (a), minor axis (b), and flattening (f)
# for each ellipsoid. For all practical applications GRS80 abd WGS84 ellipsoids
# are identical
lonlat.ellipsoid <- data.frame(
  ellipsoid=c("WGS84", "Airy1830",      "GRS80"),
  a=c(6378137.000,      6377563.396,    6378137.000),
  b=c(6356752.31424518, 6356256.909,    6356752.314140),
  f=c(298.257223563,    299.3249612665, 298.257222101),
  e2=c(0.006694379990141316996137233540,
       0.0066705400741492318211148938735613129751683486352306,
       0.006694380022900787625359114703),
  stringsAsFactors = FALSE)


# Datums, with associated ellipsoid, and Helmert transform parameters to convert
# from WGS84 into given datum.
# Note that precision of various datums will vary, and WGS84 (original) is not
# defined to be accurate to better than ±1 metre. No transformation should be
# assumed to be accurate to better than a meter; for many datums somewhat less
# (OSGB36 to WGS84 or ETRS89 the lost of accuracy can be up to 5m with single
# Helmert transformations.
# transforms: t in metres, s in ppm, r in arcseconds
lonlat.datum <- data.frame(datum=c("OSGB36", "WGS84", "ETRS89"),
                           ellipsoid=c("Airy1830","WGS84", "GRS80"),
                           tx=c(-446.448, 0L, 0L),
                           ty=c(125.157, 0L, 0L),
                           tz=c(-542.060, 0L, 0L),
                           s=c(0.0000204894, 0L, 0L),
                           rx=c(-0.1502, 0L, 0L),
                           ry=c(-0.2470, 0L, 0L),
                           rz=c(-0.8421, 0L, 0L),
                           stringsAsFactors = FALSE)


# Datum flag references. The Geoid datum flag is a number representing the local
# height datum or area of applicability of the transformation
datum.flags <- data.frame(
  geoid.datum.flag=c(1, 2, 3, 4, 6, 7, 15, 16),
  datum.name=c("Newlyn", "St Marys", "Douglas02", "Stornoway15", "Lerwick",
               "Newlyn (Orkney)", "Newlyn Offshore",
               "Outside trnasformation area"),
  region=c("UK mainland", "Scilly Isles", "Isle of Man", "Outer Hebrides",
           "Shetland Isles", "Orkney Isles",
           "Offshore (from 2km offshore up to transformation boundary)",
           "Outside transformation area"))


# Helper function. Extract last n characters from a string
substr.r <- function(x, n){
  substr(x, nchar(x) - n+1, nchar(x))
}
