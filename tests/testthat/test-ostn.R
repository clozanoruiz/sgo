context("Conversion from/to OS coordinates")
library(sgs)

# Degrees of longitude get significantly smaller in northern latitudes.
# In southern England one degree of longitude represents about 70 km, in
# northern Scotland it's less than 60 km.
# 0.1 d = 6-7 km; 0.01 d = 600-700m; 0.000001 = 6-7cm
# 1/650000000. Correctness threshold defined as a hundreth of  a milimeter.

# Earth semi-major axis at equator is a=6,378,137 meters.
# 2πa/360degrees ≈ 111 (or 110)km in a degree.
# Degrees of latitude get very slightly longer as you go further north but
# not by much.
# 0.1 d = 11 km; 0.01 d = 1.1km; 0.0000001 = 1cm
# 1/1100000000. Correctness threshold defined as a hundreth of  a milimeter.

# Heights from BNG to Lat/Lon are returned with 4 decimals. The round()
# function in R follows the  the IEC 60559 standard, which means that for
# rounding off a 5, the IEC 60559 standard (see also ‘IEEE 754’) is expected to
# be used, ‘go to the even digit’.
# A few of the testing heights that fall in that category wouldn't match to the
# 4th decimal, that's why we set the threshold as < 0.00015 (although the
# resulsts should be accurate up to the mm).

threshold    <- c(1/650000000, 1/1100000000)
threshold.3d <- c(1/650000000, 1/1100000000, 0.00015)

test_that("OSTN15_OSGM15_TestInput_ETRStoOSGB.txt", {

  file.in <- file.path(getwd(), "OSTN15_OSGM15_TestInput_ETRStoOSGB.txt")
  ostn.in <- read.csv(file.in, stringsAsFactors = FALSE)
  points.in <- sgs_points(ostn.in,
                          coords=c("ETRS.Longitude", "ETRS89.Latitude"),
                          epsg=4258)

  points.out <-
    as.data.frame(sgs_lonlat_bng(points.in, OSTN=TRUE)[c("PointID", "x", "y")])
  pointsT.out <-
    as.data.frame(sgs_transform(points.in, to=27700)[c("PointID", "x", "y")])

  file.out <- file.path(getwd(), "OSTN15_OSGM15_TestOutput_ETRStoOSGB.txt")
  ostn.out <- read.csv(file.out,
                       stringsAsFactors = FALSE)[
                         c("PointID", "OSGBEast", "OSGBNorth")]

  expect_true(all(points.out == ostn.out))
  expect_true(all(pointsT.out == ostn.out))

})

test_that("3D OSTN15_OSGM15_TestInput_ETRStoOSGB.txt", {

  file.in <- file.path(getwd(), "OSTN15_OSGM15_TestInput_ETRStoOSGB.txt")
  ostn.in <- read.csv(file.in, stringsAsFactors = FALSE)
  points.in <- sgs_points(ostn.in,
               coords=c("ETRS.Longitude", "ETRS89.Latitude", "ETRS.Height"),
               epsg=4937)

  pointsgf.out <- as.data.frame(sgs_lonlat_bng(points.in, to=7405, OSTN=TRUE,
                                               OD=TRUE),
                              stringsAsFactors = FALSE)
  points.out <- as.data.frame(sgs_lonlat_bng(points.in, to=7405, OSTN=TRUE),
                              stringsAsFactors = FALSE)
  pointsTgf.out <- as.data.frame(sgs_transform(points.in, to=7405, OSTN=TRUE,
                                               OD=TRUE),
                                 stringsAsFactors = FALSE)
  pointsT.out <- as.data.frame(sgs_transform(points.in, to=7405, OSTN=TRUE,
                                             OD=FALSE),
                               stringsAsFactors = FALSE)

  file.out <- file.path(getwd(), "OSTN15_OSGM15_TestOutput_ETRStoOSGB.txt")
  ostn.out <- read.csv(file.out,
                       stringsAsFactors = FALSE)[c("OSGBEast", "OSGBNorth",
                                                   "ODNHeight",
                                                   "OSGBDatumFlag", "PointID")]
  # convert flags to text
  ostn.out[, "OSGBDatumFlag"] <- datum.flags[match(ostn.out[, "OSGBDatumFlag"],
                                              datum.flags$geoid.datum.flag), 4]

  expect_true(all(pointsgf.out == ostn.out))
  expect_true(all(points.out == ostn.out[-4]))
  expect_true(all(pointsTgf.out == ostn.out))
  expect_true(all(pointsT.out == ostn.out[-4]))

})

test_that("OSTN15_OSGM15_TestInput_OSGBtoETRS.txt", {

  file.in <- file.path(getwd(), "OSTN15_OSGM15_TestInput_OSGBtoETRS.txt")
  ostn.in <- read.csv(file.in, stringsAsFactors = FALSE)
  points.in <- sgs_points(ostn.in,
                          coords=c("OSGB36.Eastings", "OSGB36.Northing"),
                          epsg=27700)

  points.out <- as.data.frame(sgs_bng_lonlat(points.in,
    to=4258, OSTN=TRUE)[c("PointID", "x", "y")])
  pointsT.out <-
    as.data.frame(sgs_transform(points.in, to=4258)[c("PointID", "x", "y")])

  file.out <- file.path(getwd(), "OSTN15_OSGM15_TestOutput_OSGBtoETRS.txt")
  ostn.out <- read.csv(file.out, stringsAsFactors = FALSE)
  ostn.out <- ostn.out[ostn.out$Iteration.No..RESULT == "RESULT",
                       c("PointID", "ETRSNorth.Long", "ETRSEast.Lat")]

  # We are checking the dataframe against a vector!! The default way would
  # reuse the threshold vector by column. By using sweep we compare each
  # element of the vector against each column.
  #expect_true(all(abs(points.out[, 2:3] - ostn.out[, 2:3]) < threshold))WRONG
  #expect_true(all(abs(pointsT.out[, 2:3] - ostn.out[, 2:3]) < threshold))WRONG
  expect_true(all(sweep(abs(points.out[, 2:3] - ostn.out[, 2:3]),
                        2, threshold, "<")))
  expect_true(all(sweep(abs(pointsT.out[, 2:3] - ostn.out[, 2:3]),
                        2, threshold, "<")))

})

test_that("3D OSTN15_OSGM15_TestInput_OSGBtoETRS.txt", {

  file.in <- file.path(getwd(), "OSTN15_OSGM15_TestInput_OSGBtoETRS.txt")
  ostn.in <- read.csv(file.in, stringsAsFactors = FALSE)
  points.in <- sgs_points(ostn.in,
                          coords=c("OSGB36.Eastings", "OSGB36.Northing",
                                   "Ortho.Height"),
                          epsg=7405)

  pointsgf.out <- as.data.frame(sgs_bng_lonlat(points.in, to=4937, OSTN=TRUE,
                                               OD=TRUE),
                              stringsAsFactors = FALSE)
  points.out <- as.data.frame(sgs_bng_lonlat(points.in, to=4937, OSTN=TRUE),
                              stringsAsFactors = FALSE)
  pointsTgf.out <- as.data.frame(sgs_transform(points.in, to=4937, OSTN=TRUE,
                                               OD=TRUE),
                                 stringsAsFactors = FALSE)
  pointsT.out <- as.data.frame(sgs_transform(points.in, to=4937,
                                             OD=FALSE),
                               stringsAsFactors = FALSE)

  file.out <- file.path(getwd(), "OSTN15_OSGM15_TestOutput_OSGBtoETRS.txt")
  ostn.out <- read.csv(file.out, stringsAsFactors = FALSE)
  ostn.out <- ostn.out[ostn.out$Iteration.No..RESULT == "RESULT",
                       c("ETRSNorth.Long", "ETRSEast.Lat",
                         "ETRSHeight","OSGBDatumFlag", "PointID")]

  # convert flags to text
  ostn.out[, "OSGBDatumFlag"] <- datum.flags[match(ostn.out[, "OSGBDatumFlag"],
                                              datum.flags$geoid.datum.flag), 4]

  #coordinates
  expect_true(all(sweep(abs(pointsgf.out[, 1:3] - ostn.out[, 1:3]),
                        2, threshold.3d, "<")))
  expect_true(all(sweep(abs(points.out[, 1:3] - ostn.out[, 1:3]),
                        2, threshold.3d, "<")))
  expect_true(all(sweep(abs(pointsTgf.out[, 1:3] - ostn.out[, 1:3]),
                        2, threshold.3d, "<")))
  expect_true(all(sweep(abs(pointsT.out[, 1:3] - ostn.out[, 1:3]),
                        2, threshold.3d, "<")))

  #gf
  expect_true(all(pointsgf.out[4] == ostn.out[4]))
  expect_true(all(pointsTgf.out[4] == ostn.out[4]))

})


#SAME TEST DATA IN A DIFFERENT FORMAT:
e <- c(170370.718, 250359.811, 449816.371, 438710.920, 292184.870, 639821.835,
       362269.991, 530624.974, 241124.584, 599445.590, 389544.190, 474335.969,
       562180.547, 454002.834, 357455.843, 247958.971, 247959.241, 331534.564,
       422242.186, 227778.330, 525745.670, 244780.636, 339921.145, 424639.355,
       256340.925, 319188.434, 167634.202, 397160.491, 267056.768,   9587.906,
        71713.131, 151968.652, 299721.891, 330398.323, 261596.778, 180862.461,
       421300.525, 440725.073, 395999.668)

out.e <- c(
  170370.718, 250359.811, 449816.371, 438710.920, 292184.870, 639821.835,
  362269.991, 530624.974, 241124.584, 599445.590, 389544.190, 474335.969,
  562180.547, 454002.834, 357455.843, 247958.971, 247959.241, 331534.564,
  422242.186, 227778.330, 525745.670, 244780.636, 339921.145, 424639.355,
  256340.925, 319188.434, 167634.202, 397160.491, 267056.768,   9587.909,
   71713.132, 151968.652, 299721.891, 330398.323, 261596.778, 180862.461,
  421300.525, 440725.073, 395999.668)

n <- c(11572.405,  62016.569,  75335.861, 114792.250, 168003.465, 169565.858,
      169978.690, 178388.464, 220332.641, 225722.826, 261912.153, 262047.755,
      319784.995, 340834.943, 383290.436, 393492.909, 393495.583, 431920.794,
      433818.701, 468847.388, 470703.214, 495254.887, 556034.761, 565012.703,
      664697.269, 670947.534, 797067.144, 805349.736, 846176.972, 899449.000,
      938516.405, 966483.780, 967202.992, 1017347.016, 1025447.602, 1029604.114,
      1072147.239, 1107878.448, 1138728.951)

out.n <- c(
  11572.405,  62016.569,  75335.861, 114792.250, 168003.465, 169565.858,
  169978.690, 178388.464, 220332.641, 225722.826, 261912.153, 262047.755,
  319784.995, 340834.943, 383290.436, 393492.909, 393495.583, 431920.794,
  433818.701, 468847.388, 470703.214, 495254.887, 556034.761, 565012.703,
  664697.269, 670947.534, 797067.144, 805349.736, 846176.972, 899448.996,
  938516.404, 966483.780, 967202.992,1017347.016,1025447.602,1029604.114,
  1072147.239, 1107878.448, 1138728.951)

lat <- c(49.96006137820, 50.43885825610, 50.57563665000 ,50.93127937910,
         51.40078220140, 51.37447025550, 51.42754743020, 51.48936564950,
         51.85890896400, 51.89436637350, 52.25529381630, 52.25160951230,
         52.75136687170, 52.96219109410, 53.34480280190, 53.41628516040,
         53.41630925420, 53.77911025760, 53.80021519630, 54.08666318080,
         54.11685144290, 54.32919541010, 54.89542340420, 54.97912273660,
         55.85399952950, 55.92478265510, 57.00606696050, 57.13902518960,
         57.48625000720, 57.81351838410, 58.21262247180, 58.51560361300,
         58.58120461280, 59.03743871190, 59.09335035320, 59.09671617400,
         59.53470794490, 59.85409913890, 60.13308091660)

lon <- c(-5.20304609998, -4.10864563561, -1.29782277240, -1.45051433700,
         -3.55128349240,  1.44454730409, -2.54407618349, -0.11992557180,
         -4.30852476960,  0.89724327012, -2.15458614387, -0.91248956970,
          0.40153547065, -1.19747655922, -2.64049320810, -4.28918069756,
         -4.28917792869, -3.04045490691, -1.66379168242, -4.63452168212,
         -0.07773133187, -4.38849118133, -2.93827741149, -1.61657685184,
         -4.29649016251, -3.29479219337, -5.82836691850, -2.04856030746,
         -4.21926398555, -8.57854456076, -7.59255560556, -6.26091455533,
         -3.72631022121, -3.21454001115, -4.41757674598, -5.82799339844,
         -1.62516966058, -1.27486910356, -2.07382822798)

test_that("OS E/N to Lon/Lat", {

  expect_true(all(abs(
    sgs_coordinates(sgs_bng_lonlat(sgs_points(
      list(91492.146, 11318.804), epsg=27700))) -
        c(-6.29977752014, 49.92226393730)) < threshold))

  #List of points
  array_of_xy <- sgs_coordinates(sgs_bng_lonlat(sgs_points(list(e, n),
                                                         epsg=27700)))
  array_of_substractions <- abs(array_of_xy - cbind(lon, lat))
  expect_true(all(sweep(array_of_substractions, 2, threshold, "<")))

})

test_that("Lon/Lat to OS E/N", {

  expect_true(all(abs(sgs_coordinates(sgs_lonlat_bng(sgs_points(
    list(-6.29977752014, 49.92226393730), epsg=4258))) -
      c(91492.146, 11318.804)) < 0.001))

  #List of points
  array_of_xy <- sgs_coordinates(sgs_lonlat_bng(sgs_points(list(lon, lat),
                                                         epsg=4258)))
  expect_true(all(array_of_xy == cbind(out.e, out.n)))

})

test_that("Out of range conditions and comparison with Helmert", {

  #Royal Observatory Greenwich
  gw <- c(538874.197, 177344.080)
  expect_true(all(sgs_coordinates(
    sgs_lonlat_bng(sgs_points(list(0, 51.4775), epsg=4277))) == gw))
  expect_true(all(sgs_coordinates(
    sgs_lonlat_bng(sgs_points(list(-0.0015938564, 51.4780161357),
                              epsg=4326))) == gw))
  expect_true(all(sgs_coordinates(
    sgs_lonlat_bng(sgs_points(list(-0.0015938564, 51.4780161357), epsg=4326),
                   OSTN = FALSE)) == c(538876, 177344)))

  #True origin of OSGB36
  expect_warning(OSTN.coords <- sgs_coordinates(
    sgs_lonlat_bng(sgs_points(list(-2, 49), epsg=4258))),
    "outside of the OSTN15 rectangle")
  expect_true(all(OSTN.coords == sgs_coordinates(sgs_lonlat_bng(
    sgs_points(list(-2, 49), epsg=4258), OSTN = FALSE))))

  #Outside OSTN15
  expect_warning(OSTN.coords <- sgs_coordinates(
    sgs_lonlat_bng(sgs_points(list(-10, 55), epsg=4258))),
    "outside of the OSTN15 rectangle")
  expect_true(all(OSTN.coords == sgs_coordinates(sgs_lonlat_bng(
    sgs_points(list(-10, 55), epsg=4258), OSTN = FALSE))))
  expect_warning(sgs_bng_lonlat(sgs_points(list(-111093, 596584), epsg=27700)),
    "outside of the OSTN15 rectangle") #BNG to Lon/Lat (-10, 55)

  #In the White Sea, NW Russia
  expect_warning(OSTN.coords <- sgs_coordinates(
    sgs_lonlat_bng(sgs_points(list(40, 66), epsg=4326))),
    "outside of the OSTN15 rectangle")
  expect_true(all(OSTN.coords == sgs_coordinates(sgs_lonlat_bng(
    sgs_points(list(40, 66), epsg=4326), OSTN = FALSE))))

  #True origin of OSGB36 in ETRS89 coordinates
  expect_equal(sprintf(c("%.6g", "%.7g"),
                       sgs_coordinates(sgs_bng_lonlat(
                         sgs_points(list(400096, -100086), epsg=27700),
                         OSTN = FALSE))), c("-2", "49"))

  # Outside OSTN15
  expect_equal(sprintf(c("%.6g", "%.7g"),
               sgs_coordinates(sgs_bng_lonlat(
                 sgs_points(list(133985, 604172), epsg=27700), OSTN = FALSE))),
               c("-6.18834", "55.25972"))
})

test_that("Boundaries of OSTN15 rectangle (edge and at sea)", {

  #Edge one (should give a warning, as the OSTN webpage)
  expect_warning(val <- sgs_coordinates(
    sgs_lonlat_bng(sgs_points(list(-7.5569, 49.76705), epsg=4258))),
    "outside of the OSTN15 rectangle")
  expect_true(all(val==c(21, 26)))

  expect_warning(val <- sprintf(c("%.5g", "%.7g"), sgs_coordinates(
    sgs_bng_lonlat(sgs_points(list(21, 26), epsg=27700)))),
    "outside of the OSTN15 rectangle")
  expect_equal(val, c("-7.5569", "49.76705"))

  #Off South Shields
  expect_true(all(sgs_coordinates(
    sgs_lonlat_bng(sgs_bng_lonlat(sgs_points(list(449960, 567710),
                                             epsg=27700)))) ==
      c(449960.000, 567710.000)))
  #Near Uist
  expect_true(all(sgs_coordinates(
    sgs_lonlat_bng(sgs_bng_lonlat(sgs_points(list(77360, 895710),
                                             epsg=27700)))) ==
      c(77360.001, 895709.999)))
  #Near Coll
  x <- c(109865, 109165)
  y <- c(764128, 763888)
  expect_true(all(sgs_coordinates(
    sgs_lonlat_bng(sgs_bng_lonlat(sgs_points(list(x, y), epsg=27700)))) ==
      cbind(c(109865.000, 109165.000), c(764128.000, 763888.000))))

  #Near Yell
  x <- c(458020, 449611, 456720, 452979, 447086)
  y <- c(1217306, 1215083, 1210574, 1203121, 1203452)
  res.x <- c(458020.000, 449611.000, 456720.000, 452979.000, 447086.000)
  res.y <- c(1217306.000, 1215083.000, 1210574.000, 1203121.000, 1203452.000)
  expect_true(all(sgs_coordinates(sgs_lonlat_bng(sgs_bng_lonlat(
    sgs_points(list(x, y), epsg=27700)))) == cbind(res.x, res.y)))

  #Off Scarborough
  x <- c(382514, 377516, 363754)
  y <- c(682908, 684564, 724465)
  res.x <- c(382514.000, 377516.000, 363754.000)
  res.y <- c(682908.000, 684564.000, 724465.000)
  expect_true(all(sgs_coordinates(sgs_lonlat_bng(sgs_bng_lonlat(
    sgs_points(list(x, y), epsg=27700)))) == cbind(res.x, res.y)))

  #In SW
  expect_true(all(sgs_coordinates(sgs_lonlat_bng(sgs_bng_lonlat(
    sgs_ngr_bng("SW 540 170")))) == c(154000.000, 17000.000)))
  expect_true(all(sgs_coordinates(sgs_lonlat_bng(sgs_bng_lonlat(
    sgs_ngr_bng("SW 910150")))) == c(191000.000, 15000.000)))

})

test_that("Input and internal conversions checks in BNG routines", {

  ## sgs_lonlat_bng

  expect_error(sgs_lonlat_bng(sgs_points(list(77360, 895710), epsg=27700)),
               "This routine only only accepts Geodetic Coordinate Systems")

  #Internal (3D) conversion WGS84 to ETRS89
  expect_true(sgs_bng_ngr(sgs_lonlat_bng(sgs_points(
    list(-5.003508, 56.79685, 1345), epsg=4979), to=7405),
    digits = 6) == "NN 166 712")

  #Warning from 2D to 3D
  #expect_warning(sgs_lonlat_bng(sgs_points(list(-5.003508, 56.79685),
  #                                         epsg=4326),to=7405),
  #               "Converted from 2D to 3D thus heights default to 0")


  ## sgs_bng_lonlat:

  expect_error(sgs_bng_lonlat(sgs_points(
    list(-5.003508, 56.79685, 1345), epsg=4979), to=4258),
  "This routine only supports BNG Easting and Northing entries")

  expect_error(sgs_bng_lonlat(sgs_points(list(651409.903, 313177.270),
                                         epsg=27700), to=3857),
               "This routine only supports converting to polar coordinates")

  #Warning from 2D to 3D
  #expect_warning(sgs_bng_lonlat(sgs_points(list(651409.903, 313177.270),
  #                                         epsg=27700), to=4937),
  #               "Converted from 2D to 3D thus heights default to 0")

 #ostn.at.shifts
 #all.is.na
})
