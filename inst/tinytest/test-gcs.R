### Main function sgo_set_gcs ###
dnames <- list(NULL, c("x","y"))

# x == to
p <- sgo_points(list(56.1165, -3.9369), epsg=4326)
expect_equal(sgo_set_gcs(p, to=4326), p)

# wrong input
expect_error(sgo_set_gcs(sgo_points(list(166341.986, 788816.800),
                                    epsg=27700), to=4258),
             "only accepts Geodetic Coordinate Systems")
expect_error(sgo_set_gcs(sgo_points(list(18.5, 54.2),
                                    epsg=4326), to=7405),
             "only transforms to Geodetic Coordinate Systems")

# input 'll', 'c'
expect_equal(sgo_coordinates(sgo_set_gcs(
  sgo_points(list(18.5, 54.2), epsg=4326), to=4258)),
  matrix(c(18.5, 54.2), ncol=2, dimnames=dnames))
expect_true(all(abs(sgo_coordinates(sgo_set_gcs(
  sgo_points(list(18.5, 54.2, 20), epsg=4937), to=4936)) -
    c(3545966.25772, 1186463.71294, 5149813.49954)) < 0.00001))
expect_true(all(abs(sgo_coordinates(sgo_set_gcs(
  sgo_points(list(3545966.258, 1186463.713, 5149813.500), epsg=4936),
  to=4937)) - c(18.5, 54.2, 20)) < c(0.00000001, 0.00000001, 0.001)))

# additional elements
ln <- c(-4.22472, -2.09908)
lt <- c(57.47777, 57.14965)
N <- c("Inverness", "Aberdeen")
country <- c("Scotland", "Scotland")
df <- data.frame(N, ln, lt, country, stringsAsFactors = FALSE)
p1 <- sgo_set_gcs(sgo_points(df, coords=c("ln", "lt"), epsg=4326), to=4258)
df2 <- data.frame(N, country, ln, lt, stringsAsFactors = FALSE)
p2 <- sgo_points(df2, coords=c("ln", "lt"), epsg=4258)
expect_equal(as.data.frame(p1[1:2]),
             as.data.frame(p2[1:2]))
expect_equal(as.data.frame(p1[3:4]), as.data.frame(p2[3:4]))

# 'x' and 'to' different to ETRS89
p <- sgo_points(list(-3.9369234, 56.1165135), epsg=4326)
expect_true(all(abs(sgo_coordinates(sgo_set_gcs(p, to=4277))
                    - c(-3.93558807, 56.11660085)) < 0.00000001))
p <- sgo_points(list(3737197.092, -302954.150, 5142476.100), epsg=4978)
expect_equal(sgo_coordinates(sgo_set_gcs(p, to=4277)),
             matrix(c(-4.63335099411, 54.0865177713),
                    ncol=2, dimnames=dnames))
p <- sgo_points(list(-1.6644422222, 53.6119903611, 299.800), epsg=4979)
expect_equal(sgo_coordinates(sgo_set_gcs(p, to=4277)),
             matrix(c(-1.66292822467, 53.6117492333),
                    ncol=2, dimnames=dnames))


### Testing sgo_lonlat_cart ###
# wrong input
p <- sgo_points(list(-3.0939164, 56.9556359), epsg=4277)
expect_error(sgo_lonlat_cart(p),
             "can only convert from epsg 4258, 4937, 4326 or 4979")

# additional elements
ln <- c(-4.22472, -2.09908)
lt <- c(57.47777, 57.14965)
N <- c("Inverness", "Aberdeen")
country <- c("Scotland", "Scotland")
df <- data.frame(N, ln, lt, country, stringsAsFactors = FALSE)
p1 <- sgo_lonlat_cart(sgo_points(df, coords=c("ln", "lt"), epsg=4326))
x <- c(3427907.0081, 3465674.1815)
y <- c(-253216.7327, -127024.7800)
z <- c(5354692.0241, 5334958.3584)
df2 <- data.frame(N, country, x, y, z, stringsAsFactors = FALSE)
p2 <- sgo_points(df2, coords=c("x", "y", "z"), epsg=4978)
expect_true(all(abs(as.data.frame(p1[1:2]) -
                      as.data.frame(p2[1:2])) < 0.0001))
expect_equal(as.data.frame(p1[3:4]), as.data.frame(p2[3:4]))
expect_true(all(abs(p1$z - z) < 0.0001))


### Testing sgo_cart_lonlat ###
# wrong input
p <- sgo_points(list(x=3737197.092, y=-302954.150, z=5142476.100,
                     attr="attr1"), coords=c("x", "y", "z"), epsg=7405)
expect_error(sgo_cart_lonlat(p),
             "can only convert from epsg 4936, 4978")

# additional elements
p <- sgo_cart_lonlat(sgo_points(list(x=3737197.092, y=-302954.150,
                                     z=5142476.100,
                                     attr="attr1"),
                                coords=c("x", "y", "z"),
                                epsg=4936))
df <- data.frame(x=-4.63452168103, y=54.0866631826, z=84.3656614413,
                 attr="attr1", stringsAsFactors = FALSE)
expect_true(all(abs(as.data.frame(p[1:3]) - df[1:3]) < 0.00000001))
expect_equal(as.data.frame(p[4], stringsAsFactors = FALSE), df[4])
