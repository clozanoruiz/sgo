context("Testing sgs_transform wrapper")
library(sgs)

#TODO: add all the transformations! (many are left)
#Laso using cs2cs 6.3.1 to compare results

test_that("Check inputs", {
  expect_error(sgs_transform(
    sgs_points(list(-3.9369234, 56.1165135), epsg=4326)),
    "Parameter 'to' must be specified")
})

test_that("Transform from 4326", {
  #to itself
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.9369234, 56.1165135), epsg=4326), to=4326)),
    c(-3.9369234, 56.1165135), check.attributes=FALSE)

  #to 3857
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.9369234, 56.1165135), epsg=4326), to=3857)),
    c(-438256.31, 7581645.25), check.attributes=FALSE)

  #to 4277
  #Meall a Phubuill
  # accurate up to (at least) the seventh decimal
  expect_warning(res <- all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-5.25118234, 56.91644979), epsg=4326), to=4277))
    - c(-5.25, 56 + 55/60)) < 0.00000005), "Loss of accuracy")
  expect_true(res)

  #to 27700
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.9369234, 56.1165135), epsg=4326), to=27700)),
    c(279665.251, 693220.644), check.attributes=FALSE)

  #to 4258
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.9369234, 56.1165135), epsg=4326), to=4258)),
    c(-3.9369234, 56.1165135), check.attributes=FALSE)

  #to 4937
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.9369234, 56.1165135), epsg=4326), to=4937)),
    c(-3.9369234, 56.1165135, 0), check.attributes=FALSE)

  #to 4936
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.9369234, 56.1165135), epsg=4326), to=4936)),
    c(3555669.675, -244703.389, 5271685.677), check.attributes=FALSE)

  #to 3035
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.9369234, 56.1165135), epsg=4326), to=3035)),
    c(3459763.30, 3751984.65), check.attributes=FALSE)

  #to 4979
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.9369234, 56.1165135), epsg=4326), to=4979)),
    c(-3.9369234, 56.1165135, 0), check.attributes=FALSE)

  #to 4978
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.9369234, 56.1165135), epsg=4326), to=4978)),
    c(3555669.675, -244703.389, 5271685.677), check.attributes=FALSE)

  #to 7405
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.9369234, 56.1165135), epsg=4326), to=7405)),
    c(279665.251, 693220.644, 0), check.attributes=FALSE)
})

test_that("Transform from 3857", {
  #to itself
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-344578.12, 7751020.42), epsg=3857), to=3857)),
    c(-344578.12, 7751020.42), check.attributes=FALSE)

  #to 4326
  expect_true(all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-344578.12, 7751020.42), epsg=3857), to=4326))
    - c(-3.095397918, 56.955459476)) < 0.000000001))

  #to 4277
  # accurate up to (at least) the seventh decimal
  expect_warning(res <- all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-344578.12, 7751020.42), epsg=3857), to=4277))
    - c(-3.09391468, 56.95564807)) < 0.00000005))
  expect_true(res)

  #to 27700
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-344578.12, 7751020.42), epsg=3857), to=27700)),
    c(333476.069, 785447.995), check.attributes=FALSE)

  #to 4258
  expect_true(all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-344578.12, 7751020.42), epsg=3857), to=4258))
    - c(-3.095397918, 56.955459476)) < 0.000000001))

  #to 4937
  expect_true(all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-344578.12, 7751020.42), epsg=3857), to=4937))
    - c(-3.095397918, 56.955459476, 0)) < 0.000000001))

  #to 4936
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-344578.12, 7751020.42), epsg=3857), to=4936)),
    c(3481062.238, -188247.169, 5323197.068), check.attributes=FALSE)

  #to 3035
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-344578.12, 7751020.42), epsg=3857), to=3035)),
    c(3528573.94, 3834119.30), check.attributes=FALSE)

  #to 4979
  expect_true(all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-344578.12, 7751020.42), epsg=3857), to=4979))
    - c(-3.095397918, 56.955459476, 0)) < 0.000000001))

  #to 4978
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-344578.12, 7751020.42), epsg=3857), to=4978)),
    c(3481062.238, -188247.169, 5323197.068), check.attributes=FALSE)

  #to 7405
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-344578.12, 7751020.42), epsg=3857), to=7405)),
    c(333476.069, 785447.995, 0), check.attributes=FALSE)
})

# 4277 to other than 27700/7405 are just a single Helmert transformation so
# don't expect great accuracy!
test_that("Transform from 4277", {
  #to itself
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.0939164, 56.9556359), epsg=4277), to=4277)),
    c(-3.0939164, 56.9556359), check.attributes=FALSE)

  #to 4326
  # accurate up to (at least) the seventh decimal
  expect_warning(res <- all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.09391641, 56.95563591), epsg=4277), to=4326))
    - c(-3.09539967, 56.95544731)) < 0.00000005))
  expect_true(res)

  #to 3857
  expect_warning(res <- sgs_coordinates(sgs_transform(
    sgs_points(list(-3.0939164, 56.9556359), epsg=4277), to=3857)))
  expect_equal(res, c(-344578.31, 7751017.93), check.attributes=FALSE)

  #to 27700
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.0939164, 56.9556359), epsg=4277), to=27700)),
    c(333475.941, 785446.641), check.attributes=FALSE)

  #to 4258
  # accurate up to (at least) the seventh decimal
  expect_warning(res <- all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-5.25, 56 + 55/60), epsg=4277), to=4258))
    - c(-5.25118234, 56.91644979)) < 0.00000005))
  expect_true(res)
  expect_warning(res <- all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.09391642, 56.95563593), epsg=4277), to=4258))
    - c(-3.09539969, 56.95544732)) < 0.00000005))
  expect_true(res)

  #to 4937
  expect_warning(res <- all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.09391641, 56.95563591), epsg=4277), to=4937))
    - c(-3.09539967, 56.95544731, 0)) < 0.00000005))
  expect_true(res)

  #to 4936
  expect_warning(res <- sgs_coordinates(sgs_transform(
    sgs_points(list(-3.0939164, 56.9556359), epsg=4277), to=4936)))
  expect_equal(res, c(3481063.368, -188247.337, 5323196.329),
               check.attributes=FALSE)

  #to 3035
  expect_warning(res <- sgs_coordinates(sgs_transform(
    sgs_points(list(-3.0939164, 56.9556359), epsg=4277), to=3035)))
  expect_equal(res, c(3528573.580, 3834117.990),
               check.attributes=FALSE)

  #to 4979
  expect_warning(res <- all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.09391641, 56.95563591), epsg=4277), to=4979))
    - c(-3.09539967, 56.95544731, 0)) < 0.00000005))
  expect_true(res)

  #to 4978
  expect_warning(res <- sgs_coordinates(sgs_transform(
    sgs_points(list(-3.0939164, 56.9556359), epsg=4277), to=4978)))
  expect_equal(res, c(3481063.368, -188247.337, 5323196.329),
               check.attributes=FALSE)

  #to 7405
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.0939164, 56.9556359), epsg=4277), to=7405)),
    c(333475.941, 785446.641, 0), check.attributes=FALSE)
})

test_that("Transform from 27700", {
  #to itself
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(166341.986, 788816.800), epsg=27700), to=27700)),
    c(166341.986, 788816.800), check.attributes=FALSE)
  #to 4326
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(166341.986, 788816.800), epsg=27700), to=4326)),
    c(-5.8419610340, 56.9314678393), check.attributes=FALSE)
  #to 3857
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(166341.986, 788816.800), epsg=27700), to=3857)),
    c(-650324.13, 7746124.17), check.attributes=FALSE)
  #to 4277
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(166341.986, 788816.800), epsg=27700), to=4277)),
    c(-5.84086249, 56.93169654), check.attributes=FALSE)
  #to 4258
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(166341.986, 788816.800), epsg=27700), to=4258)),
    c(-5.8419610340, 56.9314678393), check.attributes=FALSE)
  #to 4937
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(166341.986, 788816.800), epsg=27700), to=4937)),
    c(-5.8419610340, 56.9314678393, 0), check.attributes=FALSE)
  #to 4936
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(166341.986, 788816.800), epsg=27700), to=4936)),
    c(3470270.538, -355065.122, 5321739.756), check.attributes=FALSE)
  #to 3035
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(166341.986, 788816.800), epsg=27700), to=3035)),
    c(3364671.81, 3865100.26), check.attributes=FALSE)
  #to 4979
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(166341.986, 788816.800), epsg=27700), to=4979)),
    c(-5.8419610341, 56.9314678393, 0), check.attributes=FALSE)
  #to 4978
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(166341.986, 788816.800), epsg=27700), to=4978)),
    c(3470270.538, -355065.122, 5321739.756), check.attributes=FALSE)
  #to 7405
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(166341.986, 788816.800), epsg=27700), to=7405)),
    c(166341.986, 788816.800, 0.000), check.attributes=FALSE)
  #test .add.z
  p <- sgs_points(list(166341.986, 788816.800), epsg=27700)
  p2 <- .add.z(p)
  expect_true(class(p)==class(p2) && p2$z == rep(0,length(p$x)) &&
                p2[c("epsg", "dimension")] == c(7405, "XYZ"))
})

test_that("Transform from 4258", {
  #to itlsef
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.93692341, 56.11651351), epsg=4258), to=4258)),
    c(-3.93692341, 56.11651351), check.attributes=FALSE)
  #to 4326
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.93692341, 56.11651351), epsg=4258), to=4326)),
    c(-3.93692341, 56.11651351), check.attributes=FALSE)
  #to 3857
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.93692340, 56.11651350), epsg=4258), to=3857)),
    c(-438256.31, 7581645.25), check.attributes=FALSE)
  #to 4277
  # accurate up to (at least) the seventh decimal
  expect_warning(res <- all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.93692341, 56.11651351), epsg=4258), to=4277))
    - c(-3.93559444, 56.11660784)) < 0.00000005))
  expect_true(res)
  #to 27700
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.93692340, 56.11651350), epsg=4258), to=27700)),
    c(279665.251, 693220.644), check.attributes=FALSE)
  #to 4937
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.93692341, 56.11651351), epsg=4258), to=4937)),
    c(-3.93692341, 56.11651351, 0), check.attributes=FALSE)
  #to 4936
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.93692341, 56.11651351), epsg=4258), to=4936)),
    c(3555669.674, -244703.389, 5271685.677), check.attributes=FALSE)
  #to 3035
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.93692341, 56.11651351), epsg=4258), to=3035)),
    c(3459763.30, 3751984.65), check.attributes=FALSE)
  #to 4979
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.93692341, 56.11651351), epsg=4258), to=4979)),
    c(-3.93692341, 56.11651351, 0), check.attributes=FALSE)
  #to 4978
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.93692341, 56.11651351), epsg=4258), to=4978)),
    c(3555669.674, -244703.389, 5271685.678), check.attributes=FALSE)
  #to 7405
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.93692341, 56.11651351), epsg=4258), to=7405)),
    c(279665.250, 693220.645, 0), check.attributes=FALSE)
})

test_that("Transform from 4937", {

  #to itself
  #to 4326
  #to 3857
  #to 4277
  #to 27700
  #to 4258
  #to 4936
  #to 3035
  #to 4979
  #to 4978
  #to 7405

})

test_that("Transform from 4936", {

  #to itself
  #to 4326
  #to 3857
  #to 4277
  #to 27700
  #to 4258
  #to 4937
  #to 3035
  #to 4979
  #to 4978
  #to 7405

})

test_that("Transform from 3035", {

  #to itself
  #to 4326
  #to 3857
  #to 4277
  #to 27700
  #to 4258
  #to 4937
  #to 4936
  #to 4979
  #to 4978
  #to 7405

})

test_that("Transform from 4979", {

  #to itself
  #to 4326
  #to 3857
  #to 4277
  #to 27700
  #to 4258
  #to 4937
  #to 4936
  #to 3035
  #to 4978
  #to 7405

})

test_that("Transform from 4978", {

  #to itself
  #to 4326
  #to 3857
  #to 4277
  #to 27700
  #to 4258
  #to 4937
  #to 4936
  #to 3035
  #to 4979
  #to 7405

})

test_that("Transform from 7405", {
  #to itself
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(525745.670,470703.214,41.232), epsg=7405), to=7405)),
    c(525745.670,470703.214,41.232), check.attributes=FALSE)
  #to 4326
  #to 3857
  #to 4277
  #to 27700
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(525745.670,470703.214,41.232), epsg=7405), to=27700)),
    c(525745.670,470703.214), check.attributes=FALSE)
  #test .remove.z
  p <- sgs_points(list(525745.670,470703.214,41.232), epsg=7405)
  p2 <- .remove.z(p)
  expect_true(class(p)==class(p2) && length(p2) == (length(p) - 1) &&
                p2[c("epsg", "dimension")] == c(27700, "XY"))
  #to 4258
  #to 4937
  #to 4936
  #to 3035
  #to 4979
  #to 4978

})
