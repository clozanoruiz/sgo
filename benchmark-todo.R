library(maps)
library(sgo)

xmin <- -3.5
xmax <- 2.5
ymin <- 53.5
ymax <- 56.5

each_n <- function(n, reps) {

  calc_coords <- function(i) {
    p <- cbind(round(runif(n, xmin, xmax), 8), round(runif(n, ymin, ymax), 8))
    #points(p)
    #map('world', regions=('uk'), xlim=c(xmin, xmax), ylim=c(ymin, ymax))
    #rect(xmin, ymin, xmax, ymax)

    #3D bng to lonlat (OD=TRUE)
    start_time <- Sys.time()
    bng.3d2ll <- sgo_transform(bng.3d, to=4979, OD=TRUE)
    t6 <- Sys.time() - start_time

    #3D bng to lonlat
    start_time <- Sys.time()
    bng.3d2ll <- sgo_transform(bng.3d, to=4979, OD=FALSE)
    t4 <- Sys.time() - start_time

    #2D bng to lonlat
    start_time <- Sys.time()
    bng2ll <- sgo_transform(bng, to=4258)
    t2 <- Sys.time()- start_time

    #3D lonlat to bng (OD=TRUE)
    p.3d <- cbind(p, rep(0, n))
    lonlat.3d <- sgo_points(p.3d, epsg=4937)
    start_time <- Sys.time()
    bng.3d <- sgo_transform(lonlat.3d, to=7405, OD=TRUE)
    t5 <- Sys.time() - start_time

    #3D lonlat to bng
    p.3d <- cbind(p, rep(0, n))
    lonlat.3d <- sgo_points(p.3d, epsg=4937)
    start_time <- Sys.time()
    bng.3d <- sgo_transform(lonlat.3d, to=7405, OD=FALSE)
    t3 <- Sys.time() - start_time

    #2D lonlat to bng
    lonlat <- sgo_points(p, epsg=4258)
    start_time <- Sys.time()
    bng <- sgo_transform(lonlat, to=27700)
    t1 <- Sys.time()- start_time

    out <- c(t1, t2, t3, t4, t5, t6)
  }

  apply(sapply(1:reps, calc_coords), 1, mean)

}

n <- c(1000, 10000, 100000, 1000000, 5000000, 10000000)
reps <- 2
result <- sapply(n, each_n, reps)

result

##### PLOT RESULTS #####
result <- matrix(NA,nrow=6, ncol=6)
series <- c("BNG to LL (3D+OD)", "BNG to LL (3D)",  "BNG to LL (2D)",
            "LL to BNG (3D+OD)", "LL to BNG (3D)", "LL to BNG (2D)")
result[1,] <- c(0.006632924, 0.04785049, 0.3512925, 3.412248, 17.286100, 35.01287)
result[2,] <- c(0.005520940, 0.04217410, 0.2820065, 2.825136, 14.126171, 28.38339)
result[3,] <- c(0.008078456, 0.03790104, 0.2718840, 2.567024, 12.890800, 26.27742)
result[4,] <- c(0.004070640, 0.01709044, 0.1517755, 1.505867,  7.350618, 14.95144)
result[5,] <- c(0.003953457, 0.01806092, 0.1187755, 1.205132,  6.302003, 12.52924)
result[6,] <- c(0.004461050, 0.01568353, 0.1435580, 1.070004,  5.667857, 11.35794)

color <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02")

par(mar=c(5.1, 4.1, 4.1, 11.5), xpd=TRUE)


matplot(n, t(result), type="b", pch = 20, lty=1,  col=color, xaxt = 'n', ylim=c(0,45), xlim=c(1, 10000000),
     main="OSTN15 transformations", xlab="num. pair of coordinates", ylab="num. seconds")

myTicks = axTicks(1)
axis(1, at = myTicks, labels = formatC(myTicks, format = 'd', big.mark=","))

legend("topright", inset=c(-0.55,0), legend = series,
       lty = c("6a"), pch=20, col = color, box.lty=0, cex=0.9)



