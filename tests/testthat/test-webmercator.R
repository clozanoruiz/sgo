context("Pseudo-Mercator to/from WGS84")
library(sgs)

#http://epsg.io/transform#s_srs=4326&t_srs=3857
test_that("Convert from WGS84 to Pseudo-Mercator", {
  expect_equal(sgs_points_xy(sgs_wgs84_en(sgs_points(list(18.5, 54.2),
                                                     epsg=4326))),
               c(2059410.580, 7208125.261), check.attributes=FALSE)
  expect_equal(sgs_points_xy(sgs_wgs84_en(sgs_points(list(113.4, 46.78),
                                                     epsg=4326))),
               c(12623630.256, 5906238.114), check.attributes=FALSE)
  expect_equal(sgs_points_xy(sgs_wgs84_en(sgs_points(list(16.9, 67.8),
                                                     epsg=4326))),
               c(1881299.394, 10387819.721), check.attributes=FALSE)
})

#http://epsg.io/transform#s_srs=3857&t_srs=4326
test_that("Convert from Pseudo-Mercator to WGS84", {
  expect_equal(sgs_points_xy(
    sgs_en_wgs84(sgs_points(list(-489196.98, 7504281.69), epsg=3857))),
    c(-4.39453124, 55.72711009), check.attributes=FALSE)
  expect_equal(sgs_points_xy(
    sgs_en_wgs84(sgs_points(list(-533224.71, 8030168.44), epsg=3857))),
    c(-4.79003907, 58.29794403), check.attributes=FALSE)
  expect_equal(sgs_points_xy(
    sgs_en_wgs84(sgs_points(list(-841418.81, 7602121.08), epsg=3857))),
    c(-7.55859377, 56.21892316), check.attributes=FALSE)
})
