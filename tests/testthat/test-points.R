context("Rotines related to the construction and transformation of points")
library(sgs)

cols <- c("latitude", "longitude", "epsg", "datum")
colsEN <- c("easting", "northing", "epsg", "datum")

test_that("sgs_points constructors", {
  #lists
  p1 <- sgs_points(list(56.1165, -3.9369), epsg=4326)
  expect_true(all(cols %in% names(p1)) && class(p1) == "sgs_points")

  lat <- c(55.86424, 55.95325)
  lon <- c(-4.25181,-3.18827)
  p2 <- sgs_points(list(latitude=lat, longitude=lon), epsg=4326)
  expect_true(all(cols %in% names(p2)) && class(p2) == "sgs_points")

  p3 <- sgs_points(list(255005, 749423), epsg=27700)
  expect_true(all(colsEN %in% names(p3)) && class(p3) == "sgs_points")

  #data.frame
  p1b <- sgs_points(data.frame(56.1165, -3.9369), epsg=4326)
  expect_true(all(cols %in% names(p1)) && class(p1) == "sgs_points")

  lt <- c(57.47777, 57.14965)
  ln <- c(-4.22472, -2.09908)
  n <- c("Inverness", "Aberdeen")
  df <- data.frame(n, lt, ln, stringsAsFactors = FALSE)
  p4 <- sgs_points(df, coords=c("lt", "ln"), epsg=4326)
  expect_true(all(cols %in% names(p4)) && class(p4) == "sgs_points")
  #sf

  #sfc
})
#TODO
# 1) Test multiple functions (sgs_bng_laton, sgs_set_gcs_etc) with those
# different types of sgs_points (dataframes, lists, single, etc.)
# 2) Confirm all of those keep the 'sgs_points' class and the names in the class
# are always (and only): lat/x, lon/y, epsg and datum
# 3) Test all the extended operators '['. Check they do what is required from
# them and they keep all attributes/class of sgs

#additional
# sgs_polygons should accept sgs_points. Conversely, sgs_pints should accept
# sgs_polygons

#test all the options that use sf (like converting from sgs to sf!!, and
# polygons)
