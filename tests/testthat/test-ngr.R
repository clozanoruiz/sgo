library(sgo)

test_that("Grid References are correctly parsed", {
  dnames <- list(NULL, c("x","y"))
  expect_equal(sgo_coordinates(sgo_ngr_bng("TA15")),
               matrix(c(510000, 450000), ncol=2, dimnames=dnames))
  expect_equal(sgo_coordinates(sgo_ngr_bng("TA1256")),
               matrix(c(512000, 456000), ncol=2, dimnames=dnames))
  expect_equal(sgo_coordinates(sgo_ngr_bng("TA123567")),
               matrix(c(512300, 456700), ncol=2, dimnames=dnames))
  expect_equal(sgo_coordinates(sgo_ngr_bng("TA12345678")),
               matrix(c(512340, 456780), ncol=2, dimnames=dnames))
  expect_equal(sgo_coordinates(sgo_ngr_bng("TA1234567890")),
               matrix(c(512345, 467890), ncol=2, dimnames=dnames))
  expect_equal(sgo_coordinates(sgo_ngr_bng("TA 1 5")),
               matrix(c(510000, 450000), ncol=2, dimnames=dnames))
  expect_equal(sgo_coordinates(sgo_ngr_bng("TA 12 56")),
               matrix(c(512000, 456000), ncol=2, dimnames=dnames))
  expect_equal(sgo_coordinates(sgo_ngr_bng("TA 123 567")),
               matrix(c(512300, 456700), ncol=2, dimnames=dnames))
  expect_equal(sgo_coordinates(sgo_ngr_bng("TA 1234 5678")),
               matrix(c(512340, 456780), ncol=2, dimnames=dnames))
  expect_equal(sgo_coordinates(sgo_ngr_bng("TA 12345 67890")),
               matrix(c(512345, 467890), ncol=2, dimnames=dnames))
  expect_equal(sgo_coordinates(sgo_ngr_bng("TA 123567")),
               matrix(c(512300, 456700), ncol=2, dimnames=dnames))
  expect_equal(sgo_coordinates(sgo_ngr_bng("TA123 567")),
               matrix(c(512300, 456700), ncol=2, dimnames=dnames))
  expect_equal(sgo_coordinates(sgo_ngr_bng("SV9055710820")),
               matrix(c(90557, 10820), ncol=2, dimnames=dnames))
  expect_equal(sgo_coordinates(sgo_ngr_bng("HU4795841283")),
               matrix(c(447958, 1141283), ncol=2, dimnames=dnames))

  # Lists:
  expect_equal(sgo_coordinates(sgo_ngr_bng(
    c("TA1234 5678", "TA1234567890", "TA 123 567"))),
    matrix(c(512340, 456780, 512345, 467890, 512300, 456700),
           ncol=2, byrow=TRUE, dimnames=(dnames)))

  # Error messages:
  expect_error(sgo_ngr_bng("WE950950"), "Invalid grid reference.*")
})

test_that("Eastings are correctly converted to NGR", {
  expect_equal(sgo_bng_ngr(sgo_points(list(x=510000, y=450000), epsg=27700),
                           digits=2)$ngr, "TA 1 5")
  expect_equal(sgo_bng_ngr(sgo_points(list(x=512000, y=456000), epsg=27700),
                           digits=4)$ngr, "TA 12 56")
  expect_equal(sgo_bng_ngr(sgo_points(list(x=512345, y=467890),
                                      epsg=27700))$ngr, "TA 12345 67890")
  expect_equal(sgo_bng_ngr(sgo_points(list(x=512345, y=467890), epsg=27700),
                           digits=10)$ngr, "TA 12345 67890")
  expect_equal(sgo_bng_ngr(sgo_points(list(x=512345, y=467890), epsg=27700),
                           digits=8)$ngr, "TA 1234 6789")
  expect_equal(sgo_bng_ngr(sgo_points(list(x=512345, y=467890), epsg=27700),
                           digits=6)$ngr, "TA 123 678")

  # Lists:
  expect_equal(sgo_bng_ngr(sgo_points(list(easting=c(512000, 512345),
                                           northing=c(456000, 467890)),
                                      epsg=27700), digits= 6)$ngr,
    c("TA 120 560", "TA 123 678"))

  # Truncate, not rounding:
  expect_equal(sgo_bng_ngr(sgo_points(list(x=512345.387, y=467890.456),
                                      epsg=27700))$ngr, "TA 12345 67890")
  expect_equal(sgo_bng_ngr(sgo_points(list(x=512345.998, y=467890.656),
                                      epsg=27700))$ngr, "TA 12345 67890")

})

test_that("3D Eastings are correctly converted to NGR", {
  expect_equal(sgo_bng_ngr(sgo_points(list(x=510000, y=450000, z=53),
                                      epsg=27700), digits=2)$ngr, "TA 1 5")
  expect_equal(sgo_bng_ngr(sgo_points(list(x=512000, y=456000, z=47),
                                      epsg=27700), digits=4)$ngr, "TA 12 56")
  expect_equal(sgo_bng_ngr(sgo_points(list(x=512345, y=467890, z=61),
                                      epsg=27700))$ngr, "TA 12345 67890")
  expect_equal(sgo_bng_ngr(sgo_points(list(x=512345, y=467890, z=61),
                                      epsg=27700), digits=10)$ngr,
               "TA 12345 67890")
  expect_equal(sgo_bng_ngr(sgo_points(list(x=512345, y=467890, z=47),
                                      epsg=27700), digits=8)$ngr,
               "TA 1234 6789")
  expect_equal(sgo_bng_ngr(sgo_points(list(x=512345, y=467890, z=47),
                                      epsg=27700), digits=6)$ngr, "TA 123 678")

  # Lists:
  expect_equal(sgo_bng_ngr(sgo_points(list(easting=c(512000, 512345),
                                           northing=c(456000, 467890),
                                           height=c(47, 61)),
                                      epsg=27700), digits= 6)$ngr,
               c("TA 120 560", "TA 123 678"))

  # Truncate, not rounding:
  expect_equal(sgo_bng_ngr(sgo_points(list(x=512345.387, y=467890.456, z=61.32),
                                      epsg=27700))$ngr, "TA 12345 67890")
  expect_equal(sgo_bng_ngr(sgo_points(list(x=512345.998, y=467890.656, z=61.47),
                                      epsg=27700))$ngr, "TA 12345 67890")

})

test_that("Different inputs for NGR routines", {

  # 'col' defined?
  lst <- list(ngr=c("HU4795841283", "TA1256"), attr=c("attr1", "attr2"))
  expect_error(sgo_ngr_bng(lst), "Parameter 'col' must be entered")

  df <- as.data.frame(sgo_ngr_bng(lst, col="ngr"))
  df1 <- data.frame(x=c(447958, 512000), y=c(1141283, 456000),
                    attr=c("attr1", "attr2"))
  expect_true(all(df1 == df))

  # All empty/invalid NGR values
  expect_error(sgo_ngr_bng(list("")), "There are empty or null coordinates")
  expect_error(sgo_ngr_bng(c(NA, NA)), "There are empty or null coordinates")
  expect_error(sgo_ngr_bng(c("txt")), "Invalid grid reference(s) :  txt",
               fixed=TRUE)
  expect_error(sgo_ngr_bng(list(c(NaN,NA, ""))),
               "There are empty or null coordinates")

  # Some empty/invalid NGR entries
  expect_error(sgo_ngr_bng(list(c("", "TA1256"))),
               "There are empty or null coordinates")
  expect_error(sgo_ngr_bng(c("TA1256", NA)),
               "There are empty or null coordinates")
  expect_error(sgo_ngr_bng(c("txt", "TA1256")),
               "Invalid grid reference(s) :  txt", fixed=TRUE)
  expect_error(sgo_ngr_bng(list(c(NaN,"TA1256", NA, ""))),
               "There are empty or null coordinates")

  # check only
  expect_true(all(sgo_ngr_bng(c("TA 1205 60", "TA 123 678"),
                              check.only = TRUE) == TRUE))
  expect_true(all(sgo_ngr_bng(c("PZ120560", "TX 123 678"),
                              check.only = TRUE) == FALSE))
  expect_true(all(sgo_ngr_bng(c("TA120 560", "TA 123 678"),
                              check.only = TRUE) == TRUE))

  # data.frames
  df <- data.frame(name="Ben Nevis", grid="NN166712")
  res <- data.frame(x=216600, y=771200, name="Ben Nevis")
  expect_error(sgo_ngr_bng(df), "Parameter 'col' must be entered")
  expect_true(all(as.data.frame(sgo_ngr_bng(df, col="grid")) == res) == TRUE)

  # inputs other than data.frame, vectors or list
  expect_error(sgo_ngr_bng(sgo_points(list(x=512345, y=467890), epsg=27700)),
               "sgo_ngr_bng only accepts lists, dataframes or atomic types")

})


test_that("Different inputs for BNG to NGR routines", {
  expect_error(sgo_bng_ngr(sgo_points(list(x=512345, y=467890), epsg=4326)),
               "This routine only supports BNG projected coordinate system")
  expect_error(sgo_bng_ngr(sgo_points(list(x=512345, y=467890), epsg=27700),
                           digits=7),
               "Invalid precision 'digits=7'")
  expect_error(sgo_bng_ngr(sgo_points(list(x=512345, y=467890), epsg=27700),
                           digits=18),
               "Invalid precision 'digits=18'")
  expect_true(sgo_bng_ngr(sgo_points(list(x=512345, y=467890), epsg=27700),
                          digits = 0)$ngr == "512345, 467890")
  expect_true(all(unlist(sgo_bng_ngr(sgo_points(list(x=512345, y=467890,
                                                 name="name1"),
                                                coords=c("x", "y"),
                                                epsg=27700),
                                     digits = 0)) ==
                    unlist(list(ngr="512345, 467890", name="name1"))) == TRUE)

  # Out of bounds warning (and NA)
  expect_warning(sgo_bng_ngr(sgo_points(list(c(-200000,512345),
                                             c(46789, 467890)), epsg=27700)),
                 "There are 100km-grid indices out of bounds")

  # Additional elements
  expect_true(all(unlist(sgo_bng_ngr(sgo_points(list(x=512345, y=467890,
                                                     name="name1"),
                                                coords=c("x", "y"),
                                                epsg=27700),
                                     digits = 6)) ==
                    unlist(list(ngr="TA 123 678", name="name1"))) == TRUE)

})
