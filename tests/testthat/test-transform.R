context("Testing sgs_transform wrapper")
library(sgs)

test_that("Transform from 4326", {
  #to 3857
  expect_equal(sgs_points_xy(sgs_transform(
    sgs_points(list(-3.9369234, 56.1165135), epsg=4326), to=3857)),
    c(-438256.308, 7581645.249), check.attributes=FALSE)

  #to 4277
  #Meall a Phubuill
  # accurate up to (at least) the seventh decimal
  expect_true(all(abs(sgs_points_xy(sgs_transform(
    sgs_points(list(-5.25118234, 56.91644979), epsg=4326), to=4277))
    - c(-5.25, 56 + 55/60)) < 0.00000005))

  #to 27700
  expect_equal(sgs_points_xy(sgs_transform(
    sgs_points(list(-3.9369234, 56.1165135), epsg=4326), to=27700)),
    c(279665.251, 693220.644), check.attributes=FALSE)

  #to 4258
  expect_equal(sgs_points_xy(sgs_transform(
    sgs_points(list(-3.9369234, 56.1165135), epsg=4326), to=4258)),
    c(-3.9369234, 56.1165135), check.attributes=FALSE)
})
test_that("Transform from 3857", {
  #to 4326
  expect_equal(sgs_points_xy(sgs_transform(
    sgs_points(list(-344578.123, 7751020.416), epsg=3857), to=4326)),
    c( -3.09539794, 56.95545946), check.attributes=FALSE)

  #to 4277
  # accurate up to (at least) the seventh decimal
  expect_true(all(abs(sgs_points_xy(sgs_transform(
    sgs_points(list(-344578.123, 7751020.416), epsg=3857), to=4277))
    - c(-3.09391468, 56.95564807)) < 0.00000005))

  #to 27700
  expect_equal(sgs_points_xy(sgs_transform(
    sgs_points(list(-344578.123, 7751020.416), epsg=3857), to=27700)),
    c(333476.068, 785447.993), check.attributes=FALSE)

  #to 4258
  expect_equal(sgs_points_xy(sgs_transform(
    sgs_points(list(-344578.123, 7751020.416), epsg=3857), to=4258)),
    c(-3.09539794, 56.95545946), check.attributes=FALSE)
})
test_that("Transform from 4277", {
  #to 4326
  # accurate up to (at least) the seventh decimal
  expect_true(all(abs(sgs_points_xy(sgs_transform(
    sgs_points(list(-3.09391641, 56.95563591), epsg=4277), to=4326))
    - c(-3.09539967, 56.95544731)) < 0.00000005))

  #to 3857
  expect_equal(sgs_points_xy(sgs_transform(
    sgs_points(list(-3.0939164, 56.9556359), epsg=4277), to=3857)),
    c(-344578.315, 7751017.934), check.attributes=FALSE)

  #to 27700
  expect_equal(sgs_points_xy(sgs_transform(
    sgs_points(list(-3.0939164, 56.9556359), epsg=4277), to=27700)),
    c(333475.941, 785446.641), check.attributes=FALSE)

  #to 4258
  # accurate up to (at least) the seventh decimal
  expect_true(all(abs(sgs_points_xy(sgs_transform(
    sgs_points(list(-5.25, 56 + 55/60), epsg=4277), to=4258))
    - c(-5.25118234, 56.91644979)) < 0.00000005))
  expect_true(all(abs(sgs_points_xy(sgs_transform(
    sgs_points(list(-3.09391642, 56.95563593), epsg=4277), to=4258))
    - c(-3.09539969, 56.95544732)) < 0.00000005))
})
test_that("Transform from 27700", {
  #to 4326
  expect_equal(sgs_points_xy(sgs_transform(
    sgs_points(list(166341.986, 788816.800), epsg=27700), to=4326)),
    c(-5.8419610340, 56.9314678393), check.attributes=FALSE)
  #to 3857
  expect_equal(sgs_points_xy(sgs_transform(
    sgs_points(list(166341.986, 788816.800), epsg=27700), to=3857)),
    c(-650324.128, 7746124.175), check.attributes=FALSE)
  #to 4277
  expect_equal(sgs_points_xy(sgs_transform(
    sgs_points(list(166341.986, 788816.800), epsg=27700), to=4277)),
    c(-5.84086249, 56.93169654), check.attributes=FALSE)
  #to 4258
  expect_equal(sgs_points_xy(sgs_transform(
    sgs_points(list(166341.986, 788816.800), epsg=27700), to=4258)),
    c(-5.8419610340, 56.9314678393), check.attributes=FALSE)
})
test_that("Transform from 4258", {
  #to 4326
  expect_equal(sgs_points_xy(sgs_transform(
    sgs_points(list(-3.93692341, 56.11651351), epsg=4258), to=4326)),
    c(-3.93692341, 56.11651351), check.attributes=FALSE)
  #to 3857
  expect_equal(sgs_points_xy(sgs_transform(
    sgs_points(list(-3.93692340, 56.11651350), epsg=4258), to=3857)),
    c(-438256.308, 7581645.249), check.attributes=FALSE)

  #to 4277
  # accurate up to (at least) the seventh decimal
  expect_true(all(abs(sgs_points_xy(sgs_transform(
    sgs_points(list(-3.93692341, 56.11651351), epsg=4258), to=4277))
    - c(-3.93559444, 56.11660784)) < 0.00000005))

  #to 27700
  expect_equal(sgs_points_xy(sgs_transform(
    sgs_points(list(-3.93692340, 56.11651350), epsg=4258), to=27700)),
    c(279665.251, 693220.644), check.attributes=FALSE)

})
