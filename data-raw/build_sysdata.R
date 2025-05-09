# 180/π
# https://keisan.casio.com/calculator (34 digits precision)
RAD.TO.DEG <- 57.29577951308232087679815481410517
PI <- 3.141592653589793238462643383279503


#Lookup table table to decode NGR letters
.ngr.LUT <- data.frame(letter=c("A", "B", "C", "D", "E", "F", "G", "H", "J",
                                "K", "L", "M", "N", "O", "P", "Q", "R", "S",
                                "T", "U", "V", "W", "X", "Y", "Z"),
                       num=   c(0, 1, 2, 3, 4, 5, 6, 7, 8,
                                9, 10, 11, 12, 13, 14, 15, 16, 17,
                                18, 19, 20, 21, 22, 23, 24),
                       stringsAsFactors = FALSE)


# list of EPSG codes and types #,
# we could admit some epsg which should be XY as XYZ (eg. 4258, 4326, 27700)
# format is either ll (lon/lat), c (cartesians), en (easting/northing)
.epsgs <- data.frame(epsg=c(4258, 4937, 4936, 3035,
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
.sgo_points.attr <- c("epsg", "datum", "dimension")
.sgo_points.2d.coords <- c("x", "y")
.sgo_points.3d.coords <- c("x", "y", "z")
.sgo_points.2d.core <- c(.sgo_points.2d.coords, .sgo_points.attr)
.sgo_points.3d.core <- c(.sgo_points.3d.coords, .sgo_points.attr)

.coordinates.names <- cbind(x=c("x", "lon", "longitude", "e", "easting"),
                            y=c("y", "lat", "latitude","n", "northing"),
                            z=c("z", "h", "height", "h", "height"))


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
                           tx=c(-446.448, 0, 0),
                           ty=c(125.157, 0, 0),
                           tz=c(-542.060, 0, 0),
                           s=c(0.0000204894, 0, 0),
                           rx=c(-0.1502, 0, 0),
                           ry=c(-0.2470, 0, 0),
                           rz=c(-0.8421, 0, 0),
                           stringsAsFactors = FALSE)


# Datum flag references. The Geoid datum flag is a number representing the local
# height datum or area of applicability of the transformation
datum.flags <- data.frame(
  geoid.datum.flag=c(1, 2, 3, 4, 6, 7, 15, 16),
  datum.name=c("Newlyn", "St Marys", "Douglas02", "Stornoway15", "Lerwick",
               "Newlyn (Orkney)", "Newlyn Offshore",
               "Outside transformation area"),
  region=c("UK mainland", "Scilly Isles", "Isle of Man", "Outer Hebrides",
           "Shetland Isles", "Orkney Isles",
           "Offshore (from 2km offshore up to transformation boundary)",
           "Outside transformation area"),
  height.datum=c("Newlyn, UK mainland", "St Marys, Scilly Isles",
                 "Douglas02, Isle of Man", "Stornoway15, Outer Hebrides",
                 "Lerwick, Shetland Isles",
                 "Newlyn (Orkney), Orkney Isles",
                 paste("Newlyn Offshore,",
                  "Offshore (from 2km offshore up to transformation boundary)"),
                 "Outside transformation area"), stringsAsFactors = FALSE)


# Get the OSTN15 for developers from Ordnance Survey
# https://www.ordnancesurvey.co.uk/business-and-government/help-and-support/navigation-technology/os-net/formats-for-developers.html

# OSTN15 coverage is extended from grid point (0,0) to (700000,1250000),
# although the accuracy far off shore should not be relied on more than
# about +/- 5m.

# The table of data supplied by the Ordnance Survey contains 876951 rows with
# entries for each km intersection between (0,0) and (700000, 1250000).

# Make sure this file exists in the folder (currently 7z-compressed in ./dev)
file <- "./data-raw/OSTN15_OSGM15_DataFile.txt"
OSTN15 <- read.csv(file)

.ostn.shifts <- data.matrix(OSTN15[c("ETRS89_OSGB36_EShift",
                                     "ETRS89_OSGB36_NShift",
                                     "ETRS89_ODN_HeightShift",
                                     "Height_Datum_Flag")])
colnames(.ostn.shifts) <- c("e", "n", "g", "f")
.ROWS.OSTN.SHIFTS <- nrow(.ostn.shifts)


#SAVE EVERYTHING IN sysdata.rda
save(RAD.TO.DEG, PI, .ngr.LUT, .epsgs, .sgo_points.attr, .sgo_points.2d.coords,
     .sgo_points.3d.coords, .sgo_points.2d.core, .sgo_points.3d.core,
     .coordinates.names, lonlat.ellipsoid, lonlat.datum, datum.flags,
     .ostn.shifts, .ROWS.OSTN.SHIFTS, file = "./R/sysdata.rda",
     version = 3, compress = "xz")
#load(file = "./R/sysdata.rda")
