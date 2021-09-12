library(sgs)

cols <- c("x", "y", "epsg", "datum")

test_that("sgs_points constructors", {
  #lists
  p1 <- sgs_points(list(56.1165, -3.9369), epsg=4326)
  expect_true(all(cols %in% names(p1)) && class(p1) == "sgs_points")

  lon <- c(-4.25181,-3.18827)
  lat <- c(55.86424, 55.95325)
  p2 <- sgs_points(list(longitude=lon, latitude=lat), epsg=4326)
  expect_true(all(cols %in% names(p2)) && class(p2) == "sgs_points")

  p3 <- sgs_points(list(255005, 749423), epsg=27700)
  expect_true(all(cols %in% names(p3)) && class(p3) == "sgs_points")

  #data.frame
  p1b <- sgs_points(data.frame(-3.9369, 56.1165), epsg=4326)
  expect_true(all(cols %in% names(p1)) && class(p1) == "sgs_points")

  ln <- c(-4.22472, -2.09908)
  lt <- c(57.47777, 57.14965)
  n <- c("Inverness", "Aberdeen")
  df <- data.frame(n, ln, lt, stringsAsFactors = FALSE)
  p4 <- sgs_points(df, coords=c("ln", "lt"), epsg=4326)
  expect_true(all(cols %in% names(p4)) && class(p4) == "sgs_points")

  #matrix

})

test_that("Wrong inputa data", {
  n <- c("Inverness", "Aberdeen")
  ln <- c(-4.22472, -2.09908)
  lt <- c(57.47777, 57.14965)

  #lists
  expect_error(sgs_points(list(ln), epsg=4326),
               "method accepts lists with at least 2 elements")
  expect_error(sgs_points(list(-4.22472, 57.47777)),
               "'epsg' must be entered as one of the accepted numbers")
  expect_error(sgs_points(list(-4.22472, 57.47777), epsg=-1),
               "'epsg' must be entered as one of the accepted numbers")
  #sgs_points(list(c(1,3),ln,lt), epsg=4326) <- arreglar en el codigo cuando vienen sin nombre de columna!

  #data.frame
  df <- data.frame(ln, stringsAsFactors = FALSE)
  expect_error(sgs_points(df, epsg=4326),
               "method accepts dataframes with at least 2 columns")

  df <- data.frame(n, ln, lt, stringsAsFactors = FALSE)
  expect_error(sgs_points(df),
               "'epsg' must be entered as one of the accepted numbers")
  expect_error(sgs_points(df, epsg=-1),
               "'epsg' must be entered as one of the accepted numbers")

  df <- data.frame(n, ln, stringsAsFactors = FALSE)
  #sgs_points(df, epsg=4326) <- debe dar error

  #matrix
  m <- cbind(ln)
  #sgs_points(m, epsg=4326)
})

test_that("Print output", {
  #print all elements
  p <- sgs_points(list(56.1165, -3.9369), epsg=4326)

  expect_output(print(p), "An sgs object with 1 feature (point)", fixed = TRUE)
  expect_output(print(p), "dimension: XY", fixed = TRUE)
  expect_output(print(p), "EPSG:      4326", fixed = TRUE)

  #print n first elements only
  N <- c("Inverness", "Aberdeen")
  country <- c("Scotland", "Scotland")
  x <- c(3427907.0081, 3465674.1815)
  y <- c(-253216.7327, -127024.7800)
  z <- c(5354692.0241, 5334958.3584)
  df <- data.frame(N, country, x, y, z, stringsAsFactors = FALSE)
  p <- sgs_points(df, coords=c("x", "y", "z"), epsg=4978)

  expect_output(print(p, n=1),
                "An sgs object with 1 feature (point) and 2 fields ",
                fixed = TRUE)
  expect_output(print(p, n=1), "dimension: XYZ", fixed = TRUE)
  expect_output(print(p, n=1), "EPSG:      4978", fixed = TRUE)
  expect_output(print(p, n=1), "First 1 feature:",  fixed = TRUE)
})

#TODO
# as.data.frame, as.list, sgs_points_sfc, sgs_points_sf documented in a single help file called 'Coerce to other other types'!!

# 1) Test multiple functions (sgs_bng_laton, sgs_set_gcs_etc, points_xy) with those
# different types of sgs_points (dataframes, lists, single, etc.)

# 2) Confirm all of those keep the 'sgs_points' class and the names in the class
# are always at least the (5-6) core columns

# 3) Test all the extended operators '['. Check they do what is required from
# them and they keep all attributes/class of sgs (or basically test multiple ways of manipulating sgs objects (subset, merging, etc and see how it works))

# 4) test that the transformations, besides being correct keep all the additional
# columns (just a couple of them)


#test bng where the OSGM heights are outside of the transformation area (flag16)
#test cases when BNG is out of boundaries: ie. 1000000
#add these tests too: https://github.com/thruston/grid-banger/blob/master/test/test_some_more_places.py

#Write in documentation somewhre:
#**Accuracy**: Grid references rounded to whole metres will give lat/lon that
#are accurate to about 5 decimal places.  In the UK, 0.00001 of a degree of
#latitude is about 70cm, 0.00001 of a degree of longitude is about 1m.

#More NOTES:
#https://www.ordnancesurvey.co.uk/gps/transformation/ Ben Nevis: 216600,771200 (and try several heights)
#regarding distances:
#https://www.movable-type.co.uk/scripts/gis-faq-5.1.html

#Distances between points British National Grid (BNG): https://gis.stackexchange.com/a/324054
#and put examples like: shetland-jersey, thurso-carlyle, harris-aberdeen,edinburgh-london,belfats-london, etc.
#OS spreadhseet this calcualtions another OS pdfs:
#F = s(grid distance)/S(True distance)
#S=s/F
#Lines up to 10km it is considerered constant at that distance, so lines up to 20km use F from the mid point. For greater accuracy: F = 1/6(F1 + 4Fm + F2) where Fm is scale factor at mid point between P1 and P2
#http://fgg-web.fgg.uni-lj.si/~/mkuhar/Zalozba/TM_projection.pdf

#COMMENT WE CONSIDER WGS84 AND ETRS89 EQUIVALENT, BUT IT IS NOT TRUE, TALK ABOUT ETRS89 (etrF89) AND HOW IT IS DIVERGING...so if more accuracy is requiered, then they should transform between 4326 and etrs89 with other tool:
#From Transformations and OSGM015™ User guide (https://www.ordnancesurvey.co.uk/business-government/tools-support/os-net/for-developers) says:
#"...In Europe, ETRS89 is a precise version of the better known WGS84 reference system optimised for use in Europe; however, for most purposes it can be considered equivalent to WGS84.

#Specifically, the motion of the European continental plate is not apparent in ETRS89, which allows a fixed relationship to be established between this system and Ordnance Survey mapping coordinate systems.

#Additional precise versions of WGS84 are currently in use, notably ITRS (International Terrestrial Reference System); these are not equivalent to ETRS89. The difference between ITRS and ETRS89 is in the order of 0.25 m (in 1999), and growing by 0.025 m per year in UK and Ireland. This effect is only relevant in international scientific applications. For all navigation, mapping, GIS, and engineering applications within the tectonically stable parts of Europe (including UK and Ireland), the term ETRS89 should be taken as synonymous with WGS84."
#---
#e <- c(139533, 139859, 140135, 140491, 140392, 140163, 139950, 139755)
#n <- c(933991, 934182, 934257, 934256, 934056, 934076, 934057, 933986)

#bng - sgs   : 133733 (without last being the same as first) 133733 (first and last are the same)
#bng - raster: 133733


#lon = c(-6.43698696, -6.43166843, -6.42706831, -6.42102546, -6.42248238, -6.42639092, -6.42998435, -6.43321409)
#lat = c(58.21740316, 58.21930597, 58.22014035, 58.22034112, 58.21849188, 58.21853606, 58.21824033, 58.21748949)
#etrs89 - sgs:       133610.63 ("%.2f" and without last being the same as first) 133610.63 ("%.2f" last being same as first) (ok?)
#wgs84 - geosphere: 133610.64 ("%.2f" and without last being the same as first) 133610.64 ("%.2f" last being same as first)
#wgs84 - geodlib:   133610.6 (https://geographiclib.sourceforge.io/cgi-bin/Planimeter)
