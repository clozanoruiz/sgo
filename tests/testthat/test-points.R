context("Routines related to the construction and transformation of points")
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
  #sf

  #sfc
})
#TODO
## ADD a warning when convering netween e/n to latlon and viceversa when there
# are points out of bounds of the grid. Perhaps add a parameter like in
# sgs_ngr_bng to list those out of bounds.
# 1) Test multiple functions (sgs_bng_laton, sgs_set_gcs_etc, points_xy) with those
# different types of sgs_points (dataframes, lists, single, etc.)
# 2) Confirm all of those keep the 'sgs_points' class and the names in the class
# are always (and only): x, y, epsg and datum
# 3) Test all the extended operators '['. Check they do what is required from
# them and they keep all attributes/class of sgs
# 4) test taht the transformations, besides being correct keep all the additional
# columns

#test all the options that use sf (like converting from sgs to sf!)

#confirm that I cannot improve accuracy/precision in test-trnasform to/from 4277

#test bng where the OSGM heights are outside of the transformationa rea (flag16)
#test cases when BNG is out of boundaries: ie. 1000000
#add these tests: https://github.com/thruston/grid-banger/blob/master/test/test_some_more_places.py

#Write in documentation somewhre:
#**Accuracy**: Grid references rounded to whole metres will give lat/lon that
#are accurate to about 5 decimal places.  In the UK, 0.00001 of a degree of
#latitude is about 70cm, 0.00001 of a degree of longitude is about 1m.
