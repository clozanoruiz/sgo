context("Parsing and Getting Grid References")
library(sgs)

test_that("Grid References are correctly parsed", {
  expect_equal(sgs_coordinates(sgs_ngr_bng("TA15")),         c(510000, 450000),
               check.attributes=FALSE)
  expect_equal(sgs_coordinates(sgs_ngr_bng("TA1256")),       c(512000, 456000),
               check.attributes=FALSE)
  expect_equal(sgs_coordinates(sgs_ngr_bng("TA123567")),     c(512300, 456700),
               check.attributes=FALSE)
  expect_equal(sgs_coordinates(sgs_ngr_bng("TA12345678")),   c(512340, 456780),
               check.attributes=FALSE)
  expect_equal(sgs_coordinates(sgs_ngr_bng("TA1234567890")), c(512345, 467890),
               check.attributes=FALSE)
  expect_equal(sgs_coordinates(sgs_ngr_bng("TA 1 5")),       c(510000, 450000),
               check.attributes=FALSE)
  expect_equal(sgs_coordinates(sgs_ngr_bng("TA 12 56")),     c(512000, 456000),
               check.attributes=FALSE)
  expect_equal(sgs_coordinates(sgs_ngr_bng("TA 123 567")),   c(512300, 456700),
               check.attributes=FALSE)
  expect_equal(sgs_coordinates(sgs_ngr_bng("TA 1234 5678")), c(512340, 456780),
               check.attributes=FALSE)
  expect_equal(sgs_coordinates(sgs_ngr_bng("TA 12345 67890")),c(512345, 467890),
               check.attributes=FALSE)
  expect_equal(sgs_coordinates(sgs_ngr_bng("TA 123567")),    c(512300, 456700),
               check.attributes=FALSE)
  expect_equal(sgs_coordinates(sgs_ngr_bng("TA123 567")),    c(512300, 456700),
               check.attributes=FALSE)
  expect_equal(sgs_coordinates(sgs_ngr_bng("SV9055710820")), c(90557, 10820),
               check.attributes=FALSE)
  expect_equal(sgs_coordinates(sgs_ngr_bng("HU4795841283")), c(447958, 1141283),
               check.attributes=FALSE)

  # Lists:
  expect_equal(sgs_coordinates(sgs_ngr_bng(
    c("TA1234 5678", "TA1234567890", "TA 123 567"))),
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

test_that("3D Eastings are correctly converted to NGR", {
  expect_equal(sgs_bng_ngr(sgs_points(list(x=510000, y=450000, z=53),
                                      epsg=27700), digits=2)$ngr, "TA 1 5")
  expect_equal(sgs_bng_ngr(sgs_points(list(x=512000, y=456000, z=47),
                                      epsg=27700), digits=4)$ngr, "TA 12 56")
  expect_equal(sgs_bng_ngr(sgs_points(list(x=512345, y=467890, z=61),
                                      epsg=27700))$ngr, "TA 12345 67890")
  expect_equal(sgs_bng_ngr(sgs_points(list(x=512345, y=467890, z=61),
                                      epsg=27700), digits=10)$ngr,
               "TA 12345 67890")
  expect_equal(sgs_bng_ngr(sgs_points(list(x=512345, y=467890, z=47),
                                      epsg=27700), digits=8)$ngr,
               "TA 1234 6789")
  expect_equal(sgs_bng_ngr(sgs_points(list(x=512345, y=467890, z=47),
                                      epsg=27700), digits=6)$ngr, "TA 123 678")

  # Lists:
  expect_equal(sgs_bng_ngr(sgs_points(list(easting=c(512000, 512345),
                                           northing=c(456000, 467890),
                                           height=c(47, 61)),
                                      epsg=27700), digits= 6)$ngr,
               c("TA 120 560", "TA 123 678"))

  # Truncate, not rounding:
  expect_equal(sgs_bng_ngr(sgs_points(list(x=512345.387, y=467890.456, z=61.32),
                                      epsg=27700))$ngr, "TA 12345 67890")
  expect_equal(sgs_bng_ngr(sgs_points(list(x=512345.998, y=467890.656, z=61.47),
                                      epsg=27700))$ngr, "TA 12345 67890")

})

test_that("Different inputs for NGR routines", {

  # 'col' defined?
  lst <- list(ngr=c("HU4795841283", "TA1256"), attr=c("attr1", "attr2"))
  expect_error(sgs_ngr_bng(lst), "Parameter 'col' must be entered")

  df <- as.data.frame(sgs_ngr_bng(lst, col="ngr"))
  df1 <- data.frame(x=c(447958, 512000), y=c(1141283, 456000),
                    attr=c("attr1", "attr2"))
  expect_true(all(df1 == df))

  # All empty/invalid NGR values
  expect_error(sgs_ngr_bng(list("")), "There are empty or null coordinates")
  expect_error(sgs_ngr_bng(c(NA, NA)), "There are empty or null coordinates")
  expect_error(sgs_ngr_bng(c("txt")), "Invalid grid reference(s) :  txt",
               fixed=TRUE)
  expect_error(sgs_ngr_bng(list(c(NaN,NA, ""))),
               "There are empty or null coordinates")

  # Some empty/invalid NGR entries
  expect_error(sgs_ngr_bng(list(c("", "TA1256"))),
               "There are empty or null coordinates")
  expect_error(sgs_ngr_bng(c("TA1256", NA)),
               "There are empty or null coordinates")
  expect_error(sgs_ngr_bng(c("txt", "TA1256")),
               "Invalid grid reference(s) :  txt", fixed=TRUE)
  expect_error(sgs_ngr_bng(list(c(NaN,"TA1256", NA, ""))),
               "There are empty or null coordinates")

  # check only
  expect_true(all(sgs_ngr_bng(c("TA 1205 60", "TA 123 678"),
                              check.only = TRUE) == TRUE))
  expect_true(all(sgs_ngr_bng(c("PZ120560", "TX 123 678"),
                              check.only = TRUE) == FALSE))
  expect_true(all(sgs_ngr_bng(c("TA120 560", "TA 123 678"),
                              check.only = TRUE) == TRUE))

  # data.frames
  df <- data.frame(name="Ben Nevis", grid="NN166712")
  res <- data.frame(x=216600, y=771200, name="Ben Nevis")
  expect_error(sgs_ngr_bng(df), "Parameter 'col' must be entered")
  expect_true(all(as.data.frame(sgs_ngr_bng(df, col="grid")) == res) == TRUE)

  # inputs other than data.frame, vectors or list
  expect_error(sgs_ngr_bng(sgs_points(list(x=512345, y=467890), epsg=27700)),
               "sgs_ngr_bng only accepts lists, dataframes or atomic types")

})


test_that("Different inputs for BNG to NGR routines", {
  expect_error(sgs_bng_ngr(sgs_points(list(x=512345, y=467890), epsg=4326)),
               "This routine only supports BNG projected coordinate system")
  expect_error(sgs_bng_ngr(sgs_points(list(x=512345, y=467890), epsg=27700),
                           digits=7),
               "Invalid precision 'digits=7'")
  expect_error(sgs_bng_ngr(sgs_points(list(x=512345, y=467890), epsg=27700),
                           digits=18),
               "Invalid precision 'digits=18'")
  expect_true(sgs_bng_ngr(sgs_points(list(x=512345, y=467890), epsg=27700),
                          digits = 0)$ngr == "512345, 467890")
  expect_true(all(unlist(sgs_bng_ngr(sgs_points(list(x=512345, y=467890,
                                                 name="name1"),
                                            epsg=27700), digits = 0)) ==
                    unlist(list(ngr="512345, 467890", name="name1"))) == TRUE)

  # Out of bounds warning (and NA)
  expect_warning(sgs_bng_ngr(sgs_points(list(c(-200000,512345),
                                             c(46789, 467890)), epsg=27700)),
                 "There are 100km-grid indices out of bounds")

  # Additional elements
  expect_true(all(unlist(sgs_bng_ngr(sgs_points(list(x=512345, y=467890,
                              name="name1"),
                         epsg=27700), digits = 6)) ==
                    unlist(list(ngr="TA 123 678", name="name1"))) == TRUE)

})
