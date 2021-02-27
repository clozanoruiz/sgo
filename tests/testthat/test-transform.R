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
  expect_true(all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-5.25118234, 56.91644979), epsg=4326), to=4277))
    - c(-5.25, 56 + 55/60)) < 0.00000001))

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
  expect_true(all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.9369234, 56.1165135), epsg=4326), to=3035))
    - c(3459763.300, 3751984.646)) < 0.001))

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
  expect_true(all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-344578.12, 7751020.42), epsg=3857), to=4277))
    - c(-3.09391468, 56.95564807)) < 0.00000005))

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
  expect_true(all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-344578.12, 7751020.42), epsg=3857), to=3035))
    - c(3528573.939, 3834119.302)) < 0.001))

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

test_that("Transform from 4277", {
  #to itself
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.0939164, 56.9556359), epsg=4277), to=4277)),
    c(-3.0939164, 56.9556359), check.attributes=FALSE)

  #to 4326
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.09391641, 56.95563591), epsg=4277), to=4326)),
    c(-3.09539967, 56.95544731), check.attributes=FALSE)

  #to 3857
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.0939164, 56.9556359), epsg=4277), to=3857)),
    c(-344578.31, 7751017.93), check.attributes=FALSE)

  #to 27700
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.0939164, 56.9556359), epsg=4277), to=27700)),
    c(333475.941, 785446.641), check.attributes=FALSE)

  #to 4258
  expect_true(all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-5.25, 56 + 55/60), epsg=4277), to=4258))
    - c(-5.25118234, 56.91644979)) < 0.00000001))
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.09391642, 56.95563593), epsg=4277), to=4258)),
    c(-3.09539969, 56.95544732), check.attributes=FALSE)

  #to 4937
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.09391641, 56.95563591), epsg=4277), to=4937)),
    c(-3.09539967, 56.95544731, 0), check.attributes=FALSE)

  #to 4936
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.0939164, 56.9556359), epsg=4277), to=4936)),
    c(3481063.368, -188247.337, 5323196.329), check.attributes=FALSE)

  #to 3035
  expect_true(all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.0939164, 56.9556359), epsg=4277), to=3035))
    - c(3528573.579, 3834117.987)) < 0.001))

  #to 4979
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.09391641, 56.95563591), epsg=4277), to=4979)),
    c(-3.09539967, 56.95544731, 0), check.attributes=FALSE)

  #to 4978
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.0939164, 56.9556359), epsg=4277), to=4978)),
    c(3481063.368, -188247.337, 5323196.329), check.attributes=FALSE)

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
  expect_true(all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(166341.986, 788816.800), epsg=27700), to=3035))
    - c(3364671.806, 3865100.263)) < 0.001))
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
                all(p2[c("epsg", "dimension")] == c(7405, "XYZ")))
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
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.93692341, 56.11651351), epsg=4258), to=4277)),
    c(-3.93559444, 56.11660784), check.attributes=FALSE)
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
  expect_true(all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-3.93692341, 56.11651351), epsg=4258), to=3035))
    - c(3459763.300, 3751984.648)) < 0.001))
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
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-1.6644422222, 53.6119903611, 299.800), epsg=4937),
    to=4937)), c(-1.6644422222, 53.6119903611, 299.800), check.attributes=FALSE)
  #to 4326
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-1.6644422222, 53.6119903611, 299.800), epsg=4937),
    to=4326)), c(-1.6644422222, 53.6119903611), check.attributes=FALSE)
  #to 3857
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-1.6644422222, 53.6119903611, 299.800), epsg=4937),
    to=3857)), c(-185284.86, 7097011.72), check.attributes=FALSE)
  #to 4277
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-1.6644422222, 53.6119903611, 299.800), epsg=4937),
    to=4277)), c(-1.662925215, 53.61173358), check.attributes=FALSE)
  #to 27700
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-1.6644422222, 53.6119903611, 299.800), epsg=4937),
    to=27700)), c(422298.000, 412877.001), check.attributes=FALSE)
  #to 4258
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-1.6644422222, 53.6119903611, 299.800), epsg=4937),
    to=4258)), c(-1.6644422222, 53.6119903611), check.attributes=FALSE)
  #to 4936
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-1.6644422222, 53.6119903611, 299.800), epsg=4937),
    to=4936)), c(3790644.900, -110149.210, 5111482.970), check.attributes=FALSE)
  #to 3035
  expect_true(all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-1.6644422222, 53.6119903611, 299.800), epsg=4937),
    to=3035)) - c(3552763.693, 3451397.456)) < 0.001))
  #to 4979
  expect_true(all(abs(sgs_coordinates(sgs_transform(
    sgs_points(list(-1.6644422222, 53.6119903611, 299.800), epsg=4937),
    to=4979)) - c(-1.6644422222, 53.6119903611, 299.800)) <
      c(0.000000001, 0.000000001, 0.0001)))
  #to 4978
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-1.6644422222, 53.6119903611, 299.800), epsg=4937),
    to=4978)), c(3790644.900, -110149.210, 5111482.970), check.attributes=FALSE)
  #to 7405
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(-1.6644422222, 53.6119903611, 299.800), epsg=4937),
    to=7405)), c(422298.000, 412877.001, 250.004), check.attributes=FALSE)
})

test_that("Transform from 4936", {
  #to itself
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(3737197.092, -302954.150, 5142476.100), epsg=4936),
    to=4936)), c(3737197.092, -302954.150, 5142476.100), check.attributes=FALSE)
  #to 4326
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(3737197.092, -302954.150, 5142476.100), epsg=4936),
    to=4326)), c(-4.63452168103, 54.0866631817), check.attributes=FALSE)
  #to 3857
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(3737197.092, -302954.150, 5142476.100), epsg=4936),
    to=3857)), c(-515912.59, 7186586.38), check.attributes=FALSE)
  #to 4277
  #to 27700
  #to 4258
  expect_equal(sgs_coordinates(sgs_transform(
    sgs_points(list(3737197.092, -302954.150, 5142476.100), epsg=4936),
    to=4258)), c(-4.63452168103, 54.0866631826), check.attributes=FALSE)
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
                all(p2[c("epsg", "dimension")] == c(27700, "XY")))
  #to 4258
  #to 4937
  #to 4936
  #to 3035
  #to 4979
  #to 4978

})
