context("Planar, and geodetic area")
library(sgs)

# Comparing also against geographiclib (https://geographiclib.sourceforge.io/)
# https://geographiclib.sourceforge.io/cgi-bin/Planimeter

test_that("Geodetic area", {
  #2D EPSG:4326
  lon <- c(-6.43698696, -6.43166843, -6.42706831, -6.42102546,
           -6.42248238, -6.42639092, -6.42998435, -6.43321409)
  lat <- c(58.21740316, 58.21930597, 58.22014035, 58.22034112,
           58.21849188, 58.21853606, 58.21824033, 58.21748949)
  expect_equal(sgs_area(sgs_points(list(lon, lat), epsg=4326)), 133610.6)

  #3D

  #Lots of vertices Big area (council?)

})
