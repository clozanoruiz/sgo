to_mtx <- function(x) {
  matrix(x, ncol=2, dimnames=list(NULL, c("x", "y")))
}
to_mtx3 <- function(x) {
  matrix(x, ncol=3, dimnames=list(NULL, c("x", "y", "z")))
}

#https://www.ordnancesurvey.co.uk/gps/transformation/
#Also using cs2cs 6.3.1 to compare results

### Check inputs ###
expect_error(sgo_transform(
  sgo_points(list(-3.9369234, 56.1165135), epsg=4326)),
  "Parameter 'to' must be specified")


### Transform from 4326 ###
#to itself
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.9369234, 56.1165135), epsg=4326), to=4326)),
  to_mtx(c(-3.9369234, 56.1165135)))

#to 3857
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.9369234, 56.1165135), epsg=4326), to=3857)) -
    c(-438256.3082, 7581645.2490)) < 0.0001))

#to 4277
#Meall a Phubuill
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-5.25118234, 56.91644979), epsg=4326), to=4277))
  - c(-5.25, 56 + 55/60)) < 0.00000001))

#to 27700
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.9369234, 56.1165135), epsg=4326), to=27700)),
  to_mtx(c(279665.251, 693220.644)))

#to 4258
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.9369234, 56.1165135), epsg=4326), to=4258)),
  to_mtx(c(-3.9369234, 56.1165135)))

#to 4937
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.9369234, 56.1165135), epsg=4326), to=4937)),
  to_mtx3(c(-3.9369234, 56.1165135, 0)))

#to 4936
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.9369234, 56.1165135), epsg=4326), to=4936)) -
    c(3555669.6748, -244703.3887, 5271685.6769)) < 0.0001))

#to 3035
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.9369234, 56.1165135), epsg=4326), to=3035))
  - c(3459763.300, 3751984.646)) < 0.001))

#to 4979
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.9369234, 56.1165135), epsg=4326), to=4979)),
  to_mtx3(c(-3.9369234, 56.1165135, 0)))

#to 4978
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.9369234, 56.1165135), epsg=4326), to=4978)) -
    c(3555669.6748, -244703.3887, 5271685.6770)) < 0.0001))

#to 7405
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.9369234, 56.1165135), epsg=4326), to=7405)),
  to_mtx3(c(279665.251, 693220.644, -53.513)))


### Transform from 3857 ###
#to itself
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-344578.12, 7751020.42), epsg=3857), to=3857)),
  to_mtx(c(-344578.12, 7751020.42)))

#to 4326
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-344578.12, 7751020.42), epsg=3857), to=4326))
  - c(-3.095397918, 56.955459476)) < 0.000000001))

#to 4277
# accurate up to (at least) the seventh decimal
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-344578.12, 7751020.42), epsg=3857), to=4277))
  - c(-3.09391468, 56.95564807)) < 0.00000005))

#to 27700
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-344578.12, 7751020.42), epsg=3857), to=27700)),
  to_mtx(c(333476.069, 785447.995)))

#to 4258
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-344578.12, 7751020.42), epsg=3857), to=4258))
  - c(-3.095397918, 56.955459476)) < 0.000000001))

#to 4937
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-344578.12, 7751020.42), epsg=3857), to=4937))
  - c(-3.095397918, 56.955459476, 0)) < 0.000000001))

#to 4936
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-344578.12, 7751020.42), epsg=3857), to=4936)) -
    c(3481062.2382, -188247.1688, 5323197.0684)) < 0.0001))

#to 3035
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-344578.12, 7751020.42), epsg=3857), to=3035))
  - c(3528573.939, 3834119.302)) < 0.001))

#to 4979
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-344578.12, 7751020.42), epsg=3857), to=4979))
  - c(-3.095397918, 56.955459476, 0)) < 0.000000001))

#to 4978
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-344578.12, 7751020.42), epsg=3857), to=4978)) -
    c(3481062.2383, -188247.1688, 5323197.0685)) < 0.0001))

#to 7405
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-344578.12, 7751020.42), epsg=3857), to=7405)),
  to_mtx3(c(333476.069, 785447.995, -52.091)))


### Transform from 4277 ###
#to itself
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.0939164, 56.9556359), epsg=4277), to=4277)),
  to_mtx(c(-3.0939164, 56.9556359)))

#to 4326
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.09391641, 56.95563591), epsg=4277), to=4326)),
  to_mtx(c(-3.09539967, 56.95544731)))

#to 3857
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.0939164, 56.9556359), epsg=4277), to=3857)) -
    c(-344578.3150, 7751017.9336)) < 0.0001))

#to 27700
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.0939164, 56.9556359), epsg=4277), to=27700)),
  to_mtx(c(333475.941, 785446.641)))

#to 4258
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-5.25, 56 + 55/60), epsg=4277), to=4258))
  - c(-5.25118234, 56.91644979)) < 0.00000001))
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.09391642, 56.95563593), epsg=4277), to=4258)),
  to_mtx(c(-3.09539969, 56.95544732)))

#to 4937
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.09391641, 56.95563591), epsg=4277), to=4937)),
  to_mtx3(c(-3.09539967, 56.95544731, 0)))

#to 4936
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.0939164, 56.9556359), epsg=4277), to=4936)) -
    c(3481063.3678, -188247.3366, 5323196.3288)) < 0.0001))

#to 3035
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.0939164, 56.9556359), epsg=4277), to=3035))
  - c(3528573.579, 3834117.987)) < 0.001))

#to 4979
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.09391641, 56.95563591), epsg=4277), to=4979)),
  to_mtx3(c(-3.09539967, 56.95544731, 52.091)))

#to 4978
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.0939164, 56.9556359), epsg=4277), to=4978)) -
    c(3481063.3678, -188247.3366, 5323196.3289)) < 0.0001))

#to 7405
expect_error(sgo_transform(
  sgo_points(list(-3.0939164, 56.9556359), epsg=4277), to=7405),
  "Can't convert from EPSG:4277 (2D CS) to a 3D Coordinate System",
  fixed=TRUE)


### Transform from 27700 ###
#to itself
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(166341.986, 788816.800), epsg=27700), to=27700)),
  to_mtx(c(166341.986, 788816.800)))
#to 4326
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(166341.986, 788816.800), epsg=27700), to=4326)),
  to_mtx(c(-5.8419610340, 56.9314678393)))
#to 3857
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(166341.986, 788816.800), epsg=27700), to=3857)) -
    c(-650324.1275, 7746124.1748)) < 0.0001))
#to 4277
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(166341.986, 788816.800), epsg=27700), to=4277)),
  to_mtx(c(-5.84086249, 56.93169654)))
#to 4258
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(166341.986, 788816.800), epsg=27700), to=4258)),
  to_mtx(c(-5.8419610340, 56.9314678393)))
#to 4937
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(166341.986, 788816.800), epsg=27700), to=4937)),
  to_mtx3(c(-5.8419610340, 56.9314678393, 55.2705)))
#to 4936
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(166341.986, 788816.800), epsg=27700), to=4936)) -
    c(3470300.5396, -355068.1913, 5321786.0739)) < 0.0001))
#to 3035
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(166341.986, 788816.800), epsg=27700), to=3035))
  - c(3364671.806, 3865100.263)) < 0.001))
#to 4979
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(166341.986, 788816.800), epsg=27700), to=4979)),
  to_mtx3(c(-5.8419610341, 56.9314678393, 55.2705)))
#to 4978
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(166341.986, 788816.800), epsg=27700), to=4978)) -
    c(3470300.5396, -355068.1913, 5321786.0739)) < 0.0001))
#to 7405
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(166341.986, 788816.800), epsg=27700), to=7405)),
  to_mtx3(c(166341.986, 788816.800, 0.000)))
#test .add.z
p <- sgo_points(list(166341.986, 788816.800), epsg=27700)
p2 <- sgo:::.add.z(p)
expect_true(class(p)==class(p2) && p2$z == rep(0,length(p$x)) &&
              all(p2[c("epsg", "dimension")] == c(7405, "XYZ")))


### Transform from 4258 ###
#to itlsef
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.93692341, 56.11651351), epsg=4258), to=4258)),
  to_mtx(c(-3.93692341, 56.11651351)))
#to 4326
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.93692341, 56.11651351), epsg=4258), to=4326)),
  to_mtx(c(-3.93692341, 56.11651351)))
#to 3857
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.93692340, 56.11651350), epsg=4258), to=3857)) -
    c(-438256.3082, 7581645.2490)) < 0.0001))
#to 4277
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.93692341, 56.11651351), epsg=4258), to=4277)),
  to_mtx(c(-3.93559444, 56.11660784)))
#to 27700
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.93692340, 56.11651350), epsg=4258), to=27700)),
  to_mtx(c(279665.251, 693220.644)))
#to 4937
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.93692341, 56.11651351), epsg=4258), to=4937)),
  to_mtx3(c(-3.93692341, 56.11651351, 0)))
#to 4936
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.93692341, 56.11651351), epsg=4258), to=4936)) -
    c(3555669.6739, -244703.3892, 5271685.6775)) < 0.0001))
#to 3035
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.93692341, 56.11651351), epsg=4258), to=3035))
  - c(3459763.300, 3751984.648)) < 0.001))
#to 4979
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.93692341, 56.11651351), epsg=4258), to=4979)),
  to_mtx3(c(-3.93692341, 56.11651351, 0)))
#to 4978
expect_true(all(abs(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.93692341, 56.11651351), epsg=4258), to=4978)) -
    c(3555669.6740, -244703.3892, 5271685.6775)) < 0.0001))
#to 7405
expect_equal(sgo_coordinates(sgo_transform(
  sgo_points(list(-3.93692341, 56.11651351), epsg=4258), to=7405)),
  to_mtx3(c(279665.250, 693220.645, -53.513)))


### Transform from 4937 ###
p <- sgo_points(list(-1.6644422222, 53.6119903611, 299.800), epsg=4937)
#to itself
expect_equal(sgo_coordinates(sgo_transform(p, to=4937)),
             to_mtx3(c(-1.6644422222, 53.6119903611, 299.800)))
#to 4326
expect_equal(sgo_coordinates(sgo_transform(p, to=4326)),
             to_mtx(c(-1.6644422222, 53.6119903611)))
#to 3857
expect_true(all(abs(sgo_coordinates(sgo_transform(p, to=3857)) -
                      c(-185284.8606, 7097011.7155)) < 0.0001))
#to 4277
expect_equal(sgo_coordinates(sgo_transform(p, to=4277)),
             to_mtx(c(-1.662925215, 53.61173358)))
#to 27700
expect_equal(sgo_coordinates(sgo_transform(p, to=27700)),
             to_mtx(c(422298.000, 412877.001)))
#to 4258
expect_equal(sgo_coordinates(sgo_transform(p, to=4258)),
             to_mtx(c(-1.6644422222, 53.6119903611)))
#to 4936
expect_true(all(abs(sgo_coordinates(sgo_transform(p, to=4936)) -
                      c(3790644.8999, -110149.2097, 5111482.9705)) < 0.0001))
#to 3035
expect_true(all(abs(sgo_coordinates(sgo_transform(p, to=3035)) -
                      c(3552763.693, 3451397.456)) < 0.001))
#to 4979
expect_true(all(abs(sgo_coordinates(sgo_transform(p, to=4979)) -
                      c(-1.6644422222, 53.6119903611, 299.800)) <
                  c(0.000000001, 0.000000001, 0.0001)))
#to 4978
expect_true(all(abs(sgo_coordinates(sgo_transform(p, to=4978)) -
                      c(3790644.8999, -110149.2097, 5111482.9705)) < 0.0001))
#to 7405
expect_equal(sgo_coordinates(sgo_transform(p, to=7405)),
             to_mtx3(c(422298.000, 412877.001, 250.004)))


### Transform from 4936 ###
p <- sgo_points(list(3737197.092, -302954.150, 5142476.100), epsg=4936)
#to itself
expect_equal(sgo_coordinates(sgo_transform(p, to=4936)),
             to_mtx3(c(3737197.092, -302954.150, 5142476.100)))
#to 4326
expect_equal(sgo_coordinates(sgo_transform(p, to=4326)),
             to_mtx(c(-4.63452168103, 54.0866631817)))
#to 3857
expect_true(all(abs(sgo_coordinates(sgo_transform(p, to=3857)) -
                      c(-515912.5936, 7186586.3782)) < 0.0001))
#to 4277
expect_equal(sgo_coordinates(sgo_transform(p, to=4277)),
             to_mtx(c(-4.63335522305, 54.0865010151)))
#to 27700
expect_equal(sgo_coordinates(sgo_transform(p, to=27700)),
             to_mtx(c(227778.330, 468847.388)))
#to 4258
expect_equal(sgo_coordinates(sgo_transform(p, to=4258)),
             to_mtx(c(-4.63452168103, 54.0866631826)))
#to 4937
expect_equal(sgo_coordinates(sgo_transform(p, to=4937)),
             to_mtx3(c(-4.63452168103, 54.0866631826, 84.3656614413)))
#to 3035
expect_true(all(abs(sgo_coordinates(sgo_transform(p, to=3035)) -
                      c(3370704.4606, 3538801.6044)) < 0.0001))
#to 4979
expect_equal(sgo_coordinates(sgo_transform(p, to=4979)),
             to_mtx3(c(-4.63452168103, 54.0866631817, 84.3655927628)))
#to 4978
expect_equal(sgo_coordinates(sgo_transform(p, to=4978)),
             to_mtx3(c(3737197.092, -302954.150, 5142476.100)))
#to 7405
expect_equal(sgo_coordinates(sgo_transform(p, to=7405)),
             to_mtx3(c(227778.330, 468847.388, 29.335)))
expect_equal(sgo_transform(p, to=7405, OD=TRUE)$height.datum,
             "Douglas02, Isle of Man")


### Transform from 3035 ###
p <- sgo_points(list(3503432.1087, 3724031.0646), epsg=3035)
#to itself
expect_equal(sgo_coordinates(sgo_transform(p, to=3035)),
             to_mtx(c(3503432.1087, 3724031.0646)))
#to 4326
expect_equal(sgo_coordinates(sgo_transform(p, to=4326)),
             to_mtx(c(-3.16155687884, 55.9441045733)))
#to 3857
expect_true(all(abs(sgo_coordinates(sgo_transform(p, to=3857)) -
                      c(-351942.9019, 7547296.4950)) < 0.0001))
#to 4277
expect_equal(sgo_coordinates(sgo_transform(p, to=4277)),
             to_mtx(c(-3.16012697275, 55.9441659966)))
#to 27700
expect_equal(sgo_coordinates(sgo_transform(p, to=27700)),
             to_mtx(c(327550, 672950)))
#to 4258
expect_equal(sgo_coordinates(sgo_transform(p, to=4258)),
             to_mtx(c(-3.16155687884, 55.9441045742)))
#to 4937
expect_equal(sgo_coordinates(sgo_transform(p, to=4937)),
             to_mtx3(c(-3.16155687884, 55.9441045742, 0)))
#to 4936
expect_true(all(abs(sgo_coordinates(sgo_transform(p, to=4936)) -
                      c(3574551.4871, -197442.6700, 5260959.5994)) < 0.0001))
#to 4979
expect_equal(sgo_coordinates(sgo_transform(p, to=4979)),
             to_mtx3(c(-3.16155687884, 55.9441045733, 0)))
#to 4978
expect_true(all(abs(sgo_coordinates(sgo_transform(p, to=4978)) -
                      c(3574551.4872, -197442.6700, 5260959.5994)) < 0.0001))
#to 7405
expect_equal(sgo_coordinates(sgo_transform(p, to=7405)),
             to_mtx3(c(327550, 672950, -52.279)))


### Transform from 4979 ###
p <- sgo_points(list(-1.6644422222, 53.6119903611, 299.800), epsg=4979)
#to itself
expect_equal(sgo_coordinates(sgo_transform(p, to=4979)),
             to_mtx3(c(-1.6644422222, 53.6119903611, 299.800)))
#to 4326
expect_equal(sgo_coordinates(sgo_transform(p, to=4326)),
             to_mtx(c(-1.6644422222, 53.6119903611)))
#to 3857
expect_true(all(abs(sgo_coordinates(sgo_transform(p, to=3857)) -
                      c(-185284.8606, 7097011.7155)) < 0.0001))
#to 4277
expect_equal(sgo_coordinates(sgo_transform(p, to=4277)),
             to_mtx(c(-1.662925215, 53.61173358)))
#to 27700
expect_equal(sgo_coordinates(sgo_transform(p, to=27700)),
             to_mtx(c(422298.000, 412877.001)))
#to 4258
expect_equal(sgo_coordinates(sgo_transform(p, to=4258)),
             to_mtx(c(-1.6644422222, 53.6119903611)))
#to 4937
expect_true(all(abs(sgo_coordinates(sgo_transform(p, to=4937)) -
                      c(-1.6644422222, 53.6119903611, 299.800068)) < 0.000001))
#to 4936
expect_true(all(abs(sgo_coordinates(sgo_transform(p, to=4936)) -
                      c(3790644.8999, -110149.2097, 5111482.9705)) < 0.0001))
#to 3035
expect_true(all(abs(sgo_coordinates(sgo_transform(p, to=3035)) -
                      c(3552763.693, 3451397.456)) < 0.001))
#to 4978
expect_true(all(abs(sgo_coordinates(sgo_transform(p, to=4978)) -
                      c(3790644.8999, -110149.2097, 5111482.9705)) < 0.0001))
#to 7405
expect_equal(sgo_coordinates(sgo_transform(p, to=7405)),
             to_mtx3(c(422298.000, 412877.001, 250.004)))


### Transform from 4978 ###
p <- sgo_points(list(3737197.092, -302954.150, 5142476.100), epsg=4978)
#to itself
expect_equal(sgo_coordinates(sgo_transform(p, to=4978)),
             to_mtx3(c(3737197.092, -302954.150, 5142476.100)))
#to 4326
expect_equal(sgo_coordinates(sgo_transform(p, to=4326)),
             to_mtx(c(-4.63452168103, 54.0866631817)))
#to 3857
expect_true(all(abs(sgo_coordinates(sgo_transform(p, to=3857)) -
                      c(-515912.5936, 7186586.3780)) < 0.0001))
#to 4277
expect_equal(sgo_coordinates(sgo_transform(p, to=4277)),
             to_mtx(c(-4.63335522305, 54.0865010151)))
#to 27700
expect_equal(sgo_coordinates(sgo_transform(p, to=27700)),
             to_mtx(c(227778.330, 468847.388)))
#to 4258
expect_equal(sgo_coordinates(sgo_transform(p, to=4258)),
             to_mtx(c(-4.63452168103, 54.0866631826)))
#to 4937
expect_equal(sgo_coordinates(sgo_transform(p, to=4937)),
             to_mtx3(c(-4.63452168103, 54.0866631826, 84.3656614413)))
#to 4936
expect_equal(sgo_coordinates(sgo_transform(p, to=4936)),
             to_mtx3(c(3737197.092, -302954.150, 5142476.100)))
#to 3035
expect_true(all(abs(sgo_coordinates(sgo_transform(p, to=3035)) -
                      c(3370704.4606, 3538801.6044)) < 0.0001))
#to 4979
expect_equal(sgo_coordinates(sgo_transform(p, to=4979)),
             to_mtx3(c(-4.63452168103, 54.0866631817, 84.3655927628)))
#to 7405
expect_equal(sgo_coordinates(sgo_transform(p, to=7405)),
             to_mtx3(c(227778.330, 468847.388, 29.335)))
expect_equal(sgo_transform(p, to=7405, OD=TRUE)$height.datum,
             "Douglas02, Isle of Man")


### Transform from 7405 ###
p <- sgo_points(list(525745.670, 470703.214, 41.232), epsg=7405)
#to itself
expect_equal(sgo_coordinates(sgo_transform(p, to=7405)),
             to_mtx3(c(525745.670, 470703.214, 41.232)))
#to 4326
expect_equal(sgo_coordinates(sgo_transform(p, to=4326)),
             to_mtx(c(-0.0777313318421, 54.1168514428)))
#to 3857
expect_true(all(abs(sgo_coordinates(sgo_transform(p, to=3857)) -
                      c(-8653.0123, 7192317.6926)) < 0.0001))
#to 4277
expect_equal(sgo_coordinates(sgo_transform(p, to=4277)),
             to_mtx(c(-0.0759902115296, 54.1166242465)))
#to 27700
expect_equal(sgo_coordinates(sgo_transform(p, to=27700)),
             to_mtx(c(525745.670, 470703.214)))
#test .remove.z
p2 <- sgo:::.remove.z(p)
expect_true(class(p)==class(p2) && length(p2) == (length(p) - 1) &&
              all(p2[c("epsg", "dimension")] == c(27700, "XY")))
#to 4258
expect_equal(sgo_coordinates(sgo_transform(p, to=4258)),
             to_mtx(c(-0.0777313318622, 54.1168514428)))
#to 4937
expect_equal(sgo_coordinates(sgo_transform(p, to=4937)),
             to_mtx3(c(-0.0777313318622, 54.1168514428, 86.7778)))
#to 4936
expect_true(all(abs(sgo_coordinates(sgo_transform(p, to=4936)) -
                      c(3746732.40205, -5083.0738, 5144448.3028)) < 0.0001))
#to 3035
expect_true(all(abs(sgo_coordinates(sgo_transform(p, to=3035)) -
                      c(3664331.3618, 3491400.6431)) < 0.0001))
#to 4979
expect_equal(sgo_coordinates(sgo_transform(p, to=4979)),
             to_mtx3(c(-0.0777313318421, 54.1168514428, 86.7778)))
#to 4978
expect_true(all(abs(sgo_coordinates(sgo_transform(p, to=4978)) -
                      c(3746732.4020, -5083.0738, 5144448.3029)) < 0.0001))
