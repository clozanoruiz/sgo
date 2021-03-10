context("Planar, harvesine and great circle distances")
library(sgs)

test_that("Inputs and BNG", {
  p <- sgs_points(list(x=c(100000, 100000, 100000),
                       y=c(100000, 200000, 300000)), epsg=27700)
  # y defaults to x and by.element = FALSE
  expect_equal(sgs_distance(p, grid.true.distance = FALSE),
               matrix(data=c(0e+00, 1e+05, 2e+05,
                             1e+05, 0e+00, 1e+05,
                             2e+05, 1e+05, 0e+00), nrow=3, byrow=FALSE))

  # grid.true.distance = TRUE
  expect_true(all(abs(sgs_distance(p, grid.true.distance = TRUE) -
              matrix(data=c(0, 99929.3852, 199858.8042,
                            99929.3739, 0, 99929.4078,
                            199858.7591, 99929.3965, 0), nrow=3, byrow=TRUE)) <
                0.0001))

  # by.element = TRUE
  expect_equal(sgs_distance(p, grid.true.distance = FALSE, by.element = TRUE),
               c(0, 0, 0))

  p2 <- sgs_points(list(x=c(100000, 100000, 100000),
                        y=c(100000, 200000, 300000),
                        z=c(0, 0, 0)), epsg=7405)
  expect_error(sgs_distance(p, p2), "points must have the same EPSG code")

  # Do not accept EPSG where calculating distances does not make much sense.
  p3 <- sgs_points(list(-344578.12, 7751020.42), epsg=3857)
  expect_error(sgs_distance(p3), "function doesn't support the input's EPSG")

  # distance less than default simpson (20km) - F only for mid point
  p1 <- sgs_points(list(100000, 200000), epsg=27700)
  p2 <- sgs_points(list(100005, 200005), epsg=27700)
  expect_true(abs(sgs_distance(p1,p2) - 7.066075125) < 0.00000001)
})

test_that("Harvesine and Vicenty", {
  p1 <- sgs_points(list(-6.43698696, 58.21740316), epsg=4326)
  p2 <- sgs_points(list(-6.43166843, 58.21930597), epsg=4326)
  expect_equal(round(sgs_distance(p1, p2, which = "Harvesine"), 3),
               matrix(376.544))
  expect_equal(round(sgs_distance(p1, p2, which = "Harvesine",
                                  by.element = TRUE), 3), 376.544)

  expect_equal(round(sgs_distance(p1, p2, which = "Vicenty"), 3),
               matrix(377.658))
  expect_equal(round(sgs_distance(p1, p2, which = "Vicenty",
                                  by.element = TRUE), 3), 377.658)

  # Harvesine. Antipodal points
  p1 <- sgs_points(list(-177.5, -5.5), epsg=4326)
  p2 <- sgs_points(list(2.5, 5.5), epsg=4326)
  m1 <- matrix(unlist(p1[1:2], use.names = FALSE),
               ncol = 2, byrow = FALSE) / RAD.TO.GRAD
  m2 <- matrix(unlist(p2[1:2], use.names = FALSE),
               ncol = 2, byrow = FALSE) / RAD.TO.GRAD
  expect_equal(round(.great.circle.harvesine(m1, m2), 0), 20015112)

  p1 <- sgs_points(list(0, 0), epsg=4326)
  p2 <- sgs_points(list(90, 90), epsg=4326)
  m1 <- matrix(unlist(p1[1:2], use.names = FALSE),
               ncol = 2, byrow = FALSE) / RAD.TO.GRAD
  m2 <- matrix(unlist(p2[1:2], use.names = FALSE),
               ncol = 2, byrow = FALSE) / RAD.TO.GRAD
  expect_equal(round(.great.circle.harvesine(m1, m2), 0), 10007556)

  ## inverse.vicenty
  # nearly antipodal points may need a higher number of iterations to converge
  new.zealand <- sgs_points(list(174.35, -35.76), epsg=4326)
  gibraltar <- sgs_points(list(-5.35, 36.13), epsg=4326)
  expect_warning(s <- sgs_distance(new.zealand, gibraltar,
                                   which="Vicenty",
                                   by.element = TRUE),
    "Vicenty formula failed to converge. Try to increase iterations",
    fixed=TRUE)
  expect_true(is.na(s))

  expect_equal(round(sgs_distance(new.zealand, gibraltar, which="Vicenty",
                 by.element = TRUE, iterations=300), 3), 19958627.395)

  # testing coincident points
  p <- sgs_points(list(x=c(-6.43698696, -6.43166843),
                       y=c(58.21740316, 58.21930597)), epsg=4326)
  expect_equal(sgs_distance(p, which="Vicenty", by.element=TRUE), c(0,0))

  ## direct.vicenty
  # when s = 0 (final bearing == initial bearing)
  p <- matrix(c(-6.43698696/RAD.TO.GRAD, 58.21740316/RAD.TO.GRAD), ncol=2)
  d.vicenty <- .direct.vicenty.ellipsoid(p, 0, 45/RAD.TO.GRAD, "WGS84")
  expect_equal(d.vicenty$final.bearing * RAD.TO.GRAD, 45)

  # alpha1 == 0
  d.vicenty <- .direct.vicenty.ellipsoid(p, 500, 0, "WGS84")
  expect_equal(unlist(d.vicenty, use.names = FALSE) * RAD.TO.GRAD,
               c(-6.43698696, 58.22189224, 0))

  # alpha1 == 90
  d.vicenty <- .direct.vicenty.ellipsoid(p, 500, 90/RAD.TO.GRAD, "WGS84")
  expect_equal(unlist(d.vicenty, use.names = FALSE) * RAD.TO.GRAD,
               c(-6.428479802, 58.217402877, 90.007231532))

  # force no convergence
  expect_warning(d.vicenty <- .direct.vicenty.ellipsoid(p, 500, 45/RAD.TO.GRAD,
                                "WGS84", iterations = 1),
    "Vicenty formula failed to converge. Try to increase iterations")
  expect_true(all(is.na(unlist(d.vicenty))))


})

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
