context("Pseudo-Mercator to/from WGS84")
library(sgs)

#http://epsg.io/transform#s_srs=4326&t_srs=3857
test_that("Convert from WGS84 to Pseudo-Mercator", {
  # Check inputs
  expect_error(sgs_wgs84_en(sgs_points(list(3487823.234, -305433.201,
                                            5313739.634), epsg=4936)),
    "only supports WGS84 or ETRS89 polar entries")
  expect_error(sgs_wgs84_en(sgs_points(list(18.5, 54.2), epsg=4326), to=4936),
               fixed = TRUE,
    "This routine only supports converting to EPSG:3857 (Pseudo-Mercator)")

  expect_equal(sgs_coordinates(sgs_wgs84_en(sgs_points(list(18.5, 54.2),
                                                     epsg=4326))),
               c(2059410.58, 7208125.26), check.attributes=FALSE)
  expect_equal(sgs_coordinates(sgs_wgs84_en(sgs_points(list(113.4, 46.78),
                                                     epsg=4326))),
               c(12623630.26, 5906238.11), check.attributes=FALSE)
  expect_equal(sgs_coordinates(sgs_wgs84_en(sgs_points(list(16.9, 67.8),
                                                     epsg=4326))),
               c(1881299.39, 10387819.72), check.attributes=FALSE)
  #3D input
  expect_equal(sgs_coordinates(sgs_wgs84_en(sgs_points(list(18.5, 54.2, 47),
                                                       epsg=4326))),
               c(2059410.58, 7208125.26), check.attributes=FALSE)

  # Additional elements
  ln <- c(-4.22472, -2.09908)
  lt <- c(57.47777, 57.14965)
  N <- c("Inverness", "Aberdeen")
  country <- c("Scotland", "Scotland")
  df <- data.frame(N, ln, lt, country, stringsAsFactors = FALSE)
  p1 <- sgs_wgs84_en(sgs_points(df, coords=c("ln", "lt"), epsg=4326))
  e <- c(-470293.68, -233668.52)
  n <- c(7858404.23, 7790767.54)
  df2 <- data.frame(N, country, e, n, stringsAsFactors = FALSE)
  p2 <- sgs_points(df2, coords=c("e", "n"), epsg=3857)
  expect_true(all(as.data.frame(p1) == as.data.frame(p2)))

})

#http://epsg.io/transform#s_srs=3857&t_srs=4326
test_that("Convert from Pseudo-Mercator to WGS84", {
  # Check inputs
  expect_error(sgs_en_wgs84(sgs_points(list(18.5, 54.2), epsg=4326)),
               fixed = TRUE,
               "This routine only supports EPSG:3857 entries")
  expect_error(sgs_en_wgs84(sgs_points(list(-533224.71, 8030168.44),
                                       epsg=3857), to=4258),
               fixed = TRUE,
               "This routine only supports converting to EPSG:4326")

  expect_true(all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-489196.98, 7504281.69), epsg=3857), to=4326))
    - c(-4.394531241, 55.727110090)) < 0.000000001))
  expect_true(all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-533224.71, 8030168.44), epsg=3857), to=4326))
    - c(-4.790039069, 58.297944029)) < 0.000000001))
  expect_true(all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-841418.81, 7602121.08), epsg=3857), to=4326))
    - c(-7.558593774, 56.218923164)) < 0.000000001))

  # Additional elements
  e <- c(-470293.68, -233668.52)
  n <- c(7858404.23, 7790767.54)
  N <- c("Inverness", "Aberdeen")
  country <- c("Scotland", "Scotland")
  df <- data.frame(N, e, n, country, stringsAsFactors = FALSE)
  p1 <- sgs_en_wgs84(sgs_points(df, coords=c("e", "n"), epsg=3857))
  ln <- c(-4.22472, -2.09908)
  lt <- c(57.47777, 57.14965)
  df2 <- data.frame(N, country, ln, lt, stringsAsFactors = FALSE)
  p2 <- sgs_points(df2, coords=c("ln", "lt"), epsg=4326)
  expect_true(all(abs(as.data.frame(p1[1:2]) - as.data.frame(p2[1:2])) <
                    0.000001))
  expect_equal(as.data.frame(p1[3:4]), as.data.frame(p2[3:4]))
})
