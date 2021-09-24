library(sgo)

# Comparing also against geographiclib (https://geographiclib.sourceforge.io/)
# https://geographiclib.sourceforge.io/cgi-bin/Planimeter

test_that("Geodetic area", {
  # 2D EPSG:4326
  lon <- c(-6.43698696, -6.43166843, -6.42706831, -6.42102546,
           -6.42248238, -6.42639092, -6.42998435, -6.43321409)
  lat <- c(58.21740316, 58.21930597, 58.22014035, 58.22034112,
           58.21849188, 58.21853606, 58.21824033, 58.21748949)
  expect_true(all(abs(sgo_area(sgo_points(list(lon, lat), epsg=4326)) -
                        133610.63495) < 0.00001))

  # 3D input (should just ignre the heights)
  lon <- c(-6.43698696, -6.43166843, -6.42706831, -6.42102546,
           -6.42248238, -6.42639092, -6.42998435, -6.43321409)
  lat <- c(58.21740316, 58.21930597, 58.22014035, 58.22034112,
           58.21849188, 58.21853606, 58.21824033, 58.21748949)
  # Fake heights in metres
  h <- c(32, 87.5, 0, 32, 76.7, 45,7, 87.4, 26.87)
  expect_true(all(abs(sgo_area(sgo_points(list(lon=lon, lat=lat, h=h),
                                   coords=c("lon", "lat", "h"), epsg=4979)) -
               133610.63495) < 0.00001))

  # Lots of vertices (converted from BNG data)
  stirling.p.const <- readRDS(file="stirling.p.const.rds")
  stirling.sgo <- sgo_bng_lonlat(sgo_points(stirling.p.const, epsg=27700),
                                 to=4326)
  expect_equal(round(sgo_area(stirling.sgo) / 10000, 4), 216400.2725) #in ha

  # Same but interpolating vertices (the shorter the sides the more accurate)
  # in this case, interpolating every 30m. matches the results of geographiclib
  # https://geographiclib.sourceforge.io/scripts/geod-calc.html
  expect_equal(round(sgo_area(stirling.sgo, interpolate=30), 2),
               2164002729.34) #in m^2

})


test_that("Planar area", {
  # Wrong input
  expect_error(sgo_area(sgo_points(list(-344578.12, 7751020.42), epsg=3857)),
               "function doesn't support the input's EPSG")

  # Lots of vertices (Stirling Parliament Constituency)
  stirling.p.const <- readRDS(file="stirling.p.const.rds")
  stirling.sgo <- sgo_points(stirling.p.const, epsg=27700)
  expect_equal(round(sgo_area(stirling.sgo) / 10000, 3), 216359.129) #in ha
})
