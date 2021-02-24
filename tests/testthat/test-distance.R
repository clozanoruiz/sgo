context("Planar, harvesine and great circle distances")
library(sgs)

test_that("Perimeters", {
  #2D EPSG:4326
  lon <- c(-6.43698696, -6.43166843, -6.42706831, -6.42102546,
           -6.42248238, -6.42639092, -6.42998435, -6.43321409)
  lat <- c(58.21740316, 58.21930597, 58.22014035, 58.22034112,
           58.21849188, 58.21853606, 58.21824033, 58.21748949)
  pol <- sgs_points(list(lon, lat), epsg=4326)
  # Create a copy of the polygon with its coordinates shifted one
  # position so that we can calculate easily the distance between vertices
  coords <- sgs_coordinates(pol)
  pol.shift.one <- sgs_points(rbind(coords[-1, ], coords[1, ]), epsg=pol$epsg)
  expect_equal(sum(sgs_distance(pol, pol.shift.one, by.element=TRUE)), 2115.33)

})
