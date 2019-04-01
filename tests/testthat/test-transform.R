context("Testing sgs_transform wrapper")
library(sgs)

test_that("Transform from 4326", {
  #to 3857
  expect_equal(sgs_points_xy(sgs_transform(sgs_points(list(56.1165135, -3.9369234), epsg=4326),
                                                  to=3857)),
               c(-438256.308, 7581645.249), check.attributes=FALSE)
  #to 4277
  #Meall a Phubuill
  expect_true(all(abs(sgs_points_xy(sgs_transform(sgs_points(list(56.91644979, -5.25118234), epsg=4326),
                                           to=4277))
                      - c(56 + 55/60, -5.25)) < 0.00000005)) # accurate up to (at least) the seventh decimal
  #to 27700
  expect_equal(sgs_points_xy(sgs_transform(sgs_points(list(56.1165135, -3.9369234), epsg=4326),
                                           to=27700)),
               c(279665.251, 693220.644), check.attributes=FALSE)
  #to 4258
  expect_equal(sgs_points_xy(sgs_transform(sgs_points(list(56.1165135, -3.9369234), epsg=4326),
                                           to=4258)),
               c(56.1165135, -3.9369234), check.attributes=FALSE)
})
test_that("Transform from 3857", {
  #to 4326
  expect_equal(sgs_points_xy(sgs_transform(sgs_points(list(-344578.123, 7751020.416), epsg=3857),
                                           to=4326)),
               c(56.95545946, -3.09539794), check.attributes=FALSE)
  #to 4277
  expect_true(all(abs(sgs_points_xy(sgs_transform(sgs_points(list(-344578.123, 7751020.416), epsg=3857),
                                                  to=4277))
                      - c(56.95564807, -3.09391468)) < 0.00000005)) # accurate up to (at least) the seventh decimal
  #to 27700
  expect_equal(sgs_points_xy(sgs_transform(sgs_points(list(-344578.123, 7751020.416), epsg=3857),
                                           to=27700)),
               c(333476.068, 785447.993), check.attributes=FALSE)
  #to 4258
  expect_equal(sgs_points_xy(sgs_transform(sgs_points(list(-344578.123, 7751020.416), epsg=3857),
                                           to=4258)),
               c(56.95545946, -3.09539794), check.attributes=FALSE)
})
test_that("Transform from 4277", {
  #to 4326
  expect_true(all(abs(sgs_points_xy(sgs_transform(sgs_points(list(56.95563591, -3.09391641), epsg=4277),
                                                  to=4326))
                      - c(56.95544731, -3.09539967)) < 0.00000005)) # accurate up to (at least) the seventh decimal
  #to 3857
  expect_equal(sgs_points_xy(sgs_transform(sgs_points(list(56.9556359, -3.0939164), epsg=4277),
                                           to=3857)),
               c(-344578.315, 7751017.934), check.attributes=FALSE)
  #to 27700
  expect_equal(sgs_points_xy(sgs_transform(sgs_points(list(56.9556359, -3.0939164), epsg=4277),
                                           to=27700)),
               c(333475.941, 785446.641), check.attributes=FALSE)
  #to 4258
  expect_true(all(abs(sgs_points_xy(sgs_transform(sgs_points(list(56 + 55/60, -5.25), epsg=4277),
                                                  to=4258))
                      - c(56.91644979, -5.25118234)) < 0.00000005)) # accurate up to (at least) the seventh decimal
  expect_true(all(abs(sgs_points_xy(sgs_transform(sgs_points(list(56.95563593, -3.09391642), epsg=4277),
                                                  to=4258))
                      - c(56.95544732, -3.09539969)) < 0.00000005))
})
test_that("Transform from 27700", {
  #to 4326
  expect_equal(sgs_points_xy(sgs_transform(sgs_points(list(166341.986, 788816.800), epsg=27700),
                                           to=4326)),
               c(56.9314678393, -5.8419610340), check.attributes=FALSE)
  #to 3857
  expect_equal(sgs_points_xy(sgs_transform(sgs_points(list(166341.986, 788816.800), epsg=27700),
                                           to=3857)),
               c(-650324.128, 7746124.175), check.attributes=FALSE)
  #to 4277
  expect_equal(sgs_points_xy(sgs_transform(sgs_points(list(166341.986, 788816.800), epsg=27700),
                                           to=4277)),
               c(56.93169654, -5.84086249), check.attributes=FALSE)
  #to 4258
  expect_equal(sgs_points_xy(sgs_transform(sgs_points(list(166341.986, 788816.800), epsg=27700),
                                           to=4258)),
               c(56.9314678393, -5.8419610340), check.attributes=FALSE)
})
test_that("Transform from 4258", {
  #to 4326
  expect_equal(sgs_points_xy(sgs_transform(sgs_points(list(56.11651351, -3.93692341), epsg=4258),
                                           to=4326)),
               c(56.11651351, -3.93692341), check.attributes=FALSE)
  #to 3857
  expect_equal(sgs_points_xy(sgs_transform(sgs_points(list(56.11651350, -3.93692340), epsg=4258),
                                           to=3857)),
               c(-438256.308, 7581645.249), check.attributes=FALSE)
  #to 4277
  expect_true(all(abs(sgs_points_xy(sgs_transform(sgs_points(list(56.11651351, -3.93692341), epsg=4258),
                                                  to=4277))
                      - c(56.11660784, -3.93559444)) < 0.00000005)) # accurate up to (at least) the seventh decimal
  #to 27700
  expect_equal(sgs_points_xy(sgs_transform(sgs_points(list(56.11651350, -3.93692340), epsg=4258),
                                           to=27700)),
               c(279665.251, 693220.644), check.attributes=FALSE)

})
