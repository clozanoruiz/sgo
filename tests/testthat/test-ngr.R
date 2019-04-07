context("Parsing and Getting Grid References")
library(sgs)

test_that("Grid References are correctly parsed", {
  expect_equal(sgs_points_xy(sgs_ngr_bng("TA15")),           c(510000, 450000),
               check.attributes=FALSE)
  expect_equal(sgs_points_xy(sgs_ngr_bng("TA1256")),         c(512000, 456000),
               check.attributes=FALSE)
  expect_equal(sgs_points_xy(sgs_ngr_bng("TA123567")),       c(512300, 456700),
               check.attributes=FALSE)
  expect_equal(sgs_points_xy(sgs_ngr_bng("TA12345678")),     c(512340, 456780),
               check.attributes=FALSE)
  expect_equal(sgs_points_xy(sgs_ngr_bng("TA1234567890")),   c(512345, 467890),
               check.attributes=FALSE)
  expect_equal(sgs_points_xy(sgs_ngr_bng("TA 1 5")),         c(510000, 450000),
               check.attributes=FALSE)
  expect_equal(sgs_points_xy(sgs_ngr_bng("TA 12 56")),       c(512000, 456000),
               check.attributes=FALSE)
  expect_equal(sgs_points_xy(sgs_ngr_bng("TA 123 567")),     c(512300, 456700),
               check.attributes=FALSE)
  expect_equal(sgs_points_xy(sgs_ngr_bng("TA 1234 5678")),   c(512340, 456780),
               check.attributes=FALSE)
  expect_equal(sgs_points_xy(sgs_ngr_bng("TA 12345 67890")), c(512345, 467890),
               check.attributes=FALSE)
  expect_equal(sgs_points_xy(sgs_ngr_bng("TA 123567")),      c(512300, 456700),
               check.attributes=FALSE)
  expect_equal(sgs_points_xy(sgs_ngr_bng("TA123 567")),      c(512300, 456700),
               check.attributes=FALSE)
  expect_equal(sgs_points_xy(sgs_ngr_bng("SV9055710820")),   c(90557, 10820),
               check.attributes=FALSE)
  expect_equal(sgs_points_xy(sgs_ngr_bng("HU4795841283")),   c(447958, 1141283),
               check.attributes=FALSE)

  # Lists:
  expect_equal(sgs_points_xy(sgs_ngr_bng(
    c("TA12345678", "TA1234567890", "TA 123 567"))),
    matrix(c(512340, 456780, 512345, 467890, 512300, 456700),
           ncol=2, byrow=TRUE), check.attributes=FALSE)

  # Error messages:
  expect_error(sgs_ngr_bng("WE950950"), "Invalid grid reference.*")
})

test_that("Eastings are correctly converted to NGR", {
  expect_equal(sgs_bng_ngr(sgs_points(list(x=510000, y=450000), epsg=27700),
                           digits=2)$ngr, "TA 1 5")
  expect_equal(sgs_bng_ngr(sgs_points(list(x=512000, y=456000), epsg=27700),
                           digits=4)$ngr, "TA 12 56")
  expect_equal(sgs_bng_ngr(sgs_points(list(x=512345, y=467890),
                                      epsg=27700))$ngr, "TA 12345 67890")
  expect_equal(sgs_bng_ngr(sgs_points(list(x=512345, y=467890), epsg=27700),
                           digits=10)$ngr, "TA 12345 67890")
  expect_equal(sgs_bng_ngr(sgs_points(list(x=512345, y=467890), epsg=27700),
                           digits=8)$ngr, "TA 1234 6789")
  expect_equal(sgs_bng_ngr(sgs_points(list(x=512345, y=467890), epsg=27700),
                           digits=6)$ngr, "TA 123 678")

  # Lists:
  expect_equal(sgs_bng_ngr(sgs_points(list(easting=c(512000, 512345),
                                           northing=c(456000, 467890)),
                                      epsg=27700), digits= 6)$ngr,
    c("TA 120 560", "TA 123 678"))

  # Truncate, not rounding:
  expect_equal(sgs_bng_ngr(sgs_points(list(x=512345.387, y=467890.456),
                                      epsg=27700))$ngr, "TA 12345 67890")
  expect_equal(sgs_bng_ngr(sgs_points(list(x=512345.998, y=467890.656),
                                      epsg=27700))$ngr, "TA 12345 67890")

})
