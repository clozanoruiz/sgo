cols <- c("x", "y", "epsg", "datum")

### sgo_points constructors ###
#lists
p1 <- sgo_points(list(56.1165, -3.9369), epsg=4326)
expect_true(all(cols %in% names(p1)) && class(p1) == "sgo_points")

lon <- c(-4.25181,-3.18827)
lat <- c(55.86424, 55.95325)
p2 <- sgo_points(list(longitude=lon, latitude=lat), epsg=4326)
expect_true(all(cols %in% names(p2)) && class(p2) == "sgo_points")

p3 <- sgo_points(list(255005, 749423), epsg=27700)
expect_true(all(cols %in% names(p3)) && class(p3) == "sgo_points")

#data.frame
p1b <- sgo_points(data.frame(-3.9369, 56.1165), epsg=4326)
expect_true(all(cols %in% names(p1)) && class(p1) == "sgo_points")

ln <- c(-4.22472, -2.09908)
lt <- c(57.47777, 57.14965)
n <- c("Inverness", "Aberdeen")
df <- data.frame(n, ln, lt, stringsAsFactors = FALSE)
p4 <- sgo_points(df, coords=c("ln", "lt"), epsg=4326)
expect_true(all(cols %in% names(p4)) && class(p4) == "sgo_points")

#matrix
m <- cbind(ln, lt)
p5 <- sgo_points(m, coords=c("ln", "lt"), epsg=4326)
expect_true(all(cols %in% names(p5)) && class(p5) == "sgo_points")


### Wrong inputa data ###
n <- c("Inverness", "Aberdeen")
ln <- c(-4.22472, -2.09908)
lt <- c(57.47777, 57.14965)

#lists
expect_error(sgo_points(list(ln), epsg=4326),
             "method accepts lists with at least 2 elements")
expect_error(sgo_points(list(-4.22472, 57.47777)),
             "'epsg' must be entered as one of the accepted numbers")
expect_error(sgo_points(list(-4.22472, 57.47777), epsg=-1),
             "'epsg' must be entered as one of the accepted numbers")
expect_error(sgo_points(list(n, ln=ln, lt=lt),
                        coords=c("ln", "lt"), epsg=4326),
             "All elements in 'x' must be named")

#data.frame
df <- data.frame(ln, stringsAsFactors = FALSE)
expect_error(sgo_points(df, epsg=4326),
             "method accepts dataframes with at least 2 columns")

df <- data.frame(n, ln, lt, stringsAsFactors = FALSE)
expect_error(sgo_points(df),
             "'epsg' must be entered as one of the accepted numbers")
expect_error(sgo_points(df, epsg=-1),
             "'epsg' must be entered as one of the accepted numbers")

df <- data.frame(n, ln, stringsAsFactors = FALSE)
expect_error(sgo_points(df, epsg=4326),
             "coordinates must be numeric")

#matrix
m <- cbind(ln)
expect_error(sgo_points(m, epsg=4326),
             "method accepts matrices with at least 2 columns")
m <- cbind(ln, lt)
expect_error(sgo_points(m),
             "'epsg' must be entered as one of the accepted numbers")

#parameter 'coords' is not informed
expect_error(sgo_points(list(n=n, ln=ln, lt=lt), epsg=4326),
             "Must specify the coordinate columns using the 'coords' parameter")

#all coords must be in x
expect_error(sgo_points(list(lon=ln, lt=lt), coords=c("ln", "lt"), epsg=4326),
             "'x' must include all the coordinates defined in 'coords'")

#epsg corresponds with the number of coordinates
expect_error(sgo_points(list(x=-1.6644422222, y=53.6119903611, z=299.800),
                        coords=c("x", "y"), epsg=4937),
             "Wrong number of coordinates for the the specified 'epsg'")

#all coordinates must be numeric
lt.t <- c(57.47777, "57.14965")
expect_error(sgo_points(list(ln, lt.t), epsg=4326),
             "All coordinates must be numeric")

#fix 3D EPSG codes if needed
m <- cbind(ln,lt, h=c(1.0, 1.1))
expect_true(sgo_points(m, coords=c("ln", "lt", "h"),
                       epsg=4326)$epsg == 4979)
expect_true(sgo_points(list(x=-1.6644422222, y=53.6119903611, z=299.800),
                       coords=c("x", "y", "z"),
                       epsg=4258)$epsg == 4937)

#rename columns with identical names as the ones in sgo_points core
expect_warning(sgo_points(list(ln=ln, lt=lt, epsg=c(4326, 4326)),
                          coords=c("ln", "lt"), epsg=4326),
               paste("The column\\(s\\) from input data named epsg",
                     "has\\(have\\) been renamed appending the suffix '.1'"))
expect_warning(sgo_points(list(ln=ln, lt=lt, epsg=c(4326, 4326),
                               dimension=c("XY", "XY")),
                          coords=c("ln", "lt"), epsg=4326),
               paste("The column\\(s\\) from input data named epsg, dimension",
                     "has\\(have\\) been renamed appending the suffix '.1'"))

### Outputs ###
#print all elements
p <- sgo_points(list(56.1165, -3.9369), epsg=4326)

expect_stdout(print(p), "An sgo object with 1 feature (point)", fixed = TRUE)
expect_stdout(print(p), "dimension: XY", fixed = TRUE)
expect_stdout(print(p), "EPSG:      4326", fixed = TRUE)

#print n first elements only
N <- c("Inverness", "Aberdeen")
country <- c("Scotland", "Scotland")
x <- c(3427907.0081, 3465674.1815)
y <- c(-253216.7327, -127024.7800)
z <- c(5354692.0241, 5334958.3584)
df <- data.frame(N, country, x, y, z, stringsAsFactors = FALSE)
p <- sgo_points(df, coords=c("x", "y", "z"), epsg=4978)

expect_stdout(print(p, n=1),
              "An sgo object with 2 features (points) and 2 fields ",
              fixed = TRUE)
expect_stdout(print(p, n=1), "dimension: XYZ", fixed = TRUE)
expect_stdout(print(p, n=1), "EPSG:      4978", fixed = TRUE)
expect_stdout(print(p, n=1), "First 1 feature:",  fixed = TRUE)

#export to list
l <- list(x=x, y=y, z=z, N=N)
p <- sgo_points(list(N=N, z=z, x=x, y=y), coords=c("x", "y", "z"), epsg=4978)
expect_identical(as.list(p), l)

#export to data.frame
df <- data.frame(x, y, z, N)
expect_identical(as.data.frame(p), df)


### Extract coordinates ###
lon <- c(-4.25181,-3.18827)
lat <- c(55.86424, 55.95325)
p <- sgo_points(list(longitude=lon, latitude=lat), epsg=4326)

#new names
new.names <- c("longitude", "latitude")
too.many.names <- c("longitude", "latitude", "shouldn't_be_considered")
expect_equal(colnames(sgo_coordinates(p, names.xyz=too.many.names)), new.names)
expect_equal(colnames(sgo_coordinates(p, names.xyz=new.names)), new.names)

#as latitude and longited (reverse lon/lat)
expect_equal(sgo_coordinates(p, as.latlon = TRUE),
             matrix(data=c(lat, lon), ncol=2, byrow=FALSE,
                    dimnames=list(NULL, c('y', 'x'))))

expect_equal(sgo_coordinates(p, names.xyz=new.names, as.latlon=TRUE),
             matrix(data=c(lat, lon), ncol=2, byrow=FALSE,
                    dimnames=list(NULL, c("latitude", "longitude"))))

#as DMS
expect_equal(sgo_coordinates(p, names.xyz=new.names, as.latlon=TRUE,
                             ll.format='DMS'),
             matrix(data=c("55\U00B0 51\U2032 51.26\U2033 N",
                           "55\U00B0 57\U2032 11.70\U2033 N",
                           "4\U00B0 15\U2032 6.52\U2033 W",
                           "3\U00B0 11\U2032 17.77\U2033 W"),
                    ncol=2, byrow=FALSE,
                    dimnames=list(NULL, c("latitude", "longitude"))))

expect_equal(sgo_coordinates(p, ll.format='DMS'),
             matrix(data=c("4\U00B0 15\U2032 6.52\U2033 W",
                           "3\U00B0 11\U2032 17.77\U2033 W",
                           "55\U00B0 51\U2032 51.26\U2033 N",
                           "55\U00B0 57\U2032 11.70\U2033 N"),
                    ncol=2, byrow=FALSE, dimnames=list(NULL, c("x", "y"))))

#y 3D con y sin nuevos nombres (tanto para DMS como normal)
#mirar by.element=FALSE/TRUE de sgo_distance.
