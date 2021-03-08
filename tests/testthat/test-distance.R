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
  perimeter <- sum(sgs_distance(pol, pol.shift.one, by.element=TRUE))
  expect_equal(round(perimeter, 2), 2115.33)

  # Stirling Parliament Constituency (> 10000 points)
  stirling.p.const <- readRDS(file="stirling.p.const.rds")
  stirling.sgs <- sgs_bng_lonlat(sgs_points(stirling.p.const, epsg=27700),
                                 to=4326)
  coords <- sgs_coordinates(stirling.sgs)
  stir.shift.one <- sgs_points(rbind(coords[-1, ], coords[1, ]),
                               epsg=stirling.sgs$epsg)
  perimeter <- sum(sgs_distance(stirling.sgs, stir.shift.one, by.element=TRUE))
  expect_equal(round(perimeter, 3), 331024.547)
})
