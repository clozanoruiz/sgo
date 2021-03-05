context("Helmert transformation routine and set_gcs")
library(sgs)

test_that("Main function sgs_set_gcs", {
  # x == to
  p <- sgs_points(list(56.1165, -3.9369), epsg=4326)
  expect_equal(sgs_set_gcs(p, to=4326), p, check.attributes=FALSE)

  # wrong input
  expect_error(sgs_set_gcs(sgs_points(list(166341.986, 788816.800),
                                      epsg=27700), to=4258),
               "only accepts Geodetic Coordinate Systems")
  expect_error(sgs_set_gcs(sgs_points(list(18.5, 54.2),
                                      epsg=4326), to=7405),
               "only transforms to Geodetic Coordinate Systems")

  # input 'll', 'c'
  expect_equal(sgs_coordinates(sgs_set_gcs(
    sgs_points(list(18.5, 54.2), epsg=4326), to=4258)),
    c(18.5, 54.2), check.attributes=FALSE)
  expect_true(all(abs(sgs_coordinates(sgs_set_gcs(
    sgs_points(list(18.5, 54.2, 20), epsg=4937), to=4936)) -
    c(3545966.25772, 1186463.71294, 5149813.49954)) < 0.00001))
  expect_true(all(abs(sgs_coordinates(sgs_set_gcs(
    sgs_points(list(3545966.258, 1186463.713, 5149813.500), epsg=4936),
    to=4937)) - c(18.5, 54.2, 20)) < c(0.00000001, 0.00000001, 0.001)))

  # additional elements
  ln <- c(-4.22472, -2.09908)
  lt <- c(57.47777, 57.14965)
  N <- c("Inverness", "Aberdeen")
  country <- c("Scotland", "Scotland")
  df <- data.frame(N, ln, lt, country, stringsAsFactors = FALSE)
  p1 <- sgs_set_gcs(sgs_points(df, coords=c("ln", "lt"), epsg=4326), to=4258)
  df2 <- data.frame(N, country, ln, lt, stringsAsFactors = FALSE)
  p2 <- sgs_points(df2, coords=c("ln", "lt"), epsg=4258)
  expect_equal(as.data.frame(p1[1:2]),
                        as.data.frame(p2[1:2]), check.attributes=FALSE)
  expect_equal(as.data.frame(p1[3:4]), as.data.frame(p2[3:4]))

  # 'x' and 'to' different to ETRS89
  p <- sgs_points(list(-3.9369234, 56.1165135), epsg=4326)
  expect_true(all(abs(sgs_coordinates(sgs_set_gcs(p, to=4277))
    - c(-3.93558807, 56.11660085)) < 0.00000001))
  p <- sgs_points(list(3737197.092, -302954.150, 5142476.100), epsg=4978)
  expect_equal(sgs_coordinates(sgs_set_gcs(p, to=4277)),
               c(-4.63335099411, 54.0865177713), check.attributes=FALSE)
  p <- sgs_points(list(-1.6644422222, 53.6119903611, 299.800), epsg=4979)
  expect_equal(sgs_coordinates(sgs_set_gcs(p, to=4277)),
               c(-1.66292822467, 53.6117492333), check.attributes=FALSE)

})

test_that("Testing sgs_lonlat_cart", {
  # wrong input
  # additional elements
})

test_that("Testing sgs_cart_lonlat", {
  # wrong input
  # additional elements
})
