#################################################################################
#                                                                               #
# Adapted from:                                                                 #
# https://github.com/chrisveness/geodesy/blob/master/osgridref.js               #
# v.13/03/2017                                                                  #
#                                                                               #
# Version:  1.0                                                                 #
# Revision: 0 - 26/02/2018. Published version                                   #
#                                                                               #
#################################################################################

#' @encoding UTF-8
#' @title NGR to BNG Easting/Northing
#'
#' @description
#' Converts OS National Grid References to Easting/Northing coordinates
#'
#' @name sgs_ngr_bng
#' @usage sgs_ngr_bng(x, col = NULL)
#' @param x A list or vector containing strings describing OS National Grid
#' References, with or without whitespace separators. (eg 'SU 387 148').
#' @param col Character string with the name of the 'column' containing the
#' vector of NGR values, it is required when \code{x} is a list with more than
#' one column.
#' @details
#' All entered standard grid references can range from two-digit references up
#' to 10-digit references (1m × 1m square).
#' If \code{x} is a list with 2 or more vector elements, \code{col} can be used
#' to inform the function which of the elements contains the NGR strings. The rest
#' of the elements will be appended to the resulting object. See examples.
#' @return
#' An object of class \code{sgs_points} whose coordinates are defined as
#' Easting/Northing.
#' @seealso \code{\link{sgs_points}}, \code{\link{sgs_bng_ngr}}.
#' @examples
#' vec <- c("NN 166 712", "HU 3863 7653")
#' lst <- list(p1="NN166712", p2="HU 38637653")
#' v <- sgs_ngr_bng(vec)
#' l <- sgs_ngr_bng(lst)
#'
#' # any additional column (here 'attr') will be added to the result
#' extra <- list(p=c("NN 166712", "HU38637653"),
#'               attr=c("name1","name2"))
#' res <- sgs_ngr_bng(extra, col="p")
#' res
#'
#' # grid references returned by sgs_bng_ngr are within an
#' # element (column) named 'ngr'
#' grid <- sgs_bng_ngr(sgs_points(list(x=247455, y=706338,
#'                                     name="Ben Venue"),
#'                                 epsg=27700))
#' bng <- sgs_ngr_bng(grid, col="ngr")
#' @export
sgs_ngr_bng <- function(x, col=NULL) {

  err.msg <- "Invalid grid reference(s)"
  old.x <- x

  if (!is.null(col)) {
    x <- unlist(x[col], use.names = FALSE)
  } else {
    x <- unlist(x, use.names = FALSE)
  }


  # Validate format
  # Parse only alphanumeric data
  x <- gsub("[^a-zA-Z0-9]", "", x)
  match <- grepl("^[a-zA-Z]{2}(\\d{2}|\\d{4}|\\d{6}|\\d{8}|\\d{10})$", x, ignore.case=TRUE, perl=TRUE)
  invalid.indices <- match==FALSE
  if (any(invalid.indices)) {
    if ( all( is_nothing(x[which(invalid.indices)]) ) ) {
      #from here we will only work only with those that do not have invalid values
      #x.holder <- rep(NA, length(x))
      #x <- x[!invalid.indices]
      stop( paste(err.msg , ": ",  "There are empty or null coordinates",sep=" "))
    } else {
      invalid.values <- x[which(invalid.indices)]
      first.invalid.value <- invalid.values[which(!is_nothing(invalid.values))][1]
      stop( paste(err.msg , ": ",  first.invalid.value,sep=" "))
    }
  }


  # Get numeric values of letter references, mapping A->0, B->1, C->2, etc:
  a.code <- strtoi(charToRaw("A"), 16L)
  l1 <- strtoi(vapply(substr(toupper(x), 1, 1), charToRaw, as.raw(0)), 16L) - a.code
  l2 <- strtoi(vapply(substr(toupper(x), 2, 2), charToRaw, as.raw(0)), 16L) - a.code
  # Shuffle down letters after 'I' since 'I' is not used in grid:
  l1 <- ifelse (l1 > 7, l1-1, l1)
  l2 <- ifelse (l2 > 7, l2-1, l2)

  # Convert grid letters into 100km-square indexes from false origin (grid square SV):
  e100km <- ((l1-2)%%5)*5 + (l2%%5)
  n100km <- (19-trunc(l1/5)*5) - trunc(l2/5)

  # Skip grid letters to get numeric (easting/northing) part of ref (splitting numbers half way)
  en <- substring(x, 3)
  n.char <- nchar(en)
  e <- substr(en, 1, n.char/2)
  n <- substr(en, n.char/2 + 1, n.char)

  # Validation
  if (any(e100km<0 | e100km>6 | n100km<0 | n100km>12)) stop( err.msg )
  if (any(nchar(e) != nchar(n))) stop( err.msg )

  # Standardise up to to 10-digit refs (metres)
  e <- paste0( e100km, substr(paste0(e, "00000"), 1, 5) )
  n <- paste0( n100km, substr(paste0(n, "00000"), 1, 5) )

  #if (any(invalid.indices)) {
    #do something if we accept invalid (ie. nulls/NA/""...)
  #}

  if(is.null(col)) {
    lst <-  list(x=as.numeric(e), y=as.numeric(n))
  } else {
    lst <- c(old.x[!names(old.x) %in% col], list(x=as.numeric(e), y=as.numeric(n)))
  }
  sgs_points(lst, epsg=27700)

}

#' @encoding UTF-8
#' @title BNG Easting/Northing to National Grid References (NGR)
#'
#' @description
#' Converts BNG Easting/Northing coordinates to National Grid References
#'
#' @name sgs_bng_ngr
#' @usage sgs_bng_ngr(x, digits = 10)
#' @param x A \code{sgs_points} object with coordinates defined as \code{epsg=27700}.
#' @param digits Numeric. It defines the precision of the resulting grid references.
#' @details
#' All resulting grid references will have 10 digits (1m × 1m square) by default.
#' In order to reduce the ouput precision change the digits paramater accordingly.
#' When \code{digits=0}, it returns the numeric format of the grid references.
#'
#' Note that rather than being rounded, national grid references are truncated
#' when converting to less precise references (as the OS system demands). By
#' doing so, the grid reference refers to the lower left corner of the relevant
#' square - to ensure the more precise polygon will remain within the boundaries
#' of the less precise polygon.
#' @return
#' A list with at least one element named 'ngr'.
#' @seealso \code{\link{sgs_points}}, \code{\link{sgs_ngr_bng}}.
#' @examples
#' sgs <- sgs_points(list(x=247455, y=706338, name="Ben Venue"), epsg=27700)
#' grid10 <- sgs_bng_ngr(sgs)
#' grid8 <- sgs_bng_ngr(sgs, digits=8)
#' #and notice the truncating, not rounding, of grid8 regarding grid10.
#' @export
sgs_bng_ngr <- function(x, digits=10) UseMethod("sgs_bng_ngr")


#' @export
sgs_bng_ngr.sgs_points <- function(x, digits=10) {

  if (x$epsg != 27700) stop("This routine only supports BNG Easting and Northing entries")

  if (digits%%2!=0 || digits>16) stop(paste0("Invalid precision 'digits=", digits, "'"))

  core.cols <- sgs_points.core
  #core.cols <- sgs_points.core[!sgs_points.core %in% c("latitude", "longitude")]

  additional.elements <- !names(x) %in% core.cols
  num.elements <- sum(additional.elements, na.rm=TRUE)
  e <- x$easting
  n <- x$northing

  # Use digits = 0 to return numeric format (in metres, allowing for decimals & for northing > 1e6)
  if (digits == 0) {

    e.int <- trunc(e); e.dec <- e - e.int
    n.int <- trunc(n); n.dec <- n - n.int
    e.pad <- paste0(substr.r(paste0('000000', e.int), 6),
                    ifelse(e.dec>0, substring(as.character(round(e.dec, 3)), 2), ""))
    n.pad <- paste0(ifelse(n.int<1e6, substr.r(paste0('000000', n.int), 6), n.int),
                    ifelse(n.dec>0, substring(as.character(round(n.dec, 3)), 2), ""))

    #return
    ngr <- list(ngr=paste0(e.pad, ",",  n.pad))
    if (num.elements > 0) {
      return (c(x[additional.elements], ngr))
    } else { return (ngr) }

  }

  # Get the 100km-grid indices
  e100k <- trunc(e/100000); n100k <- trunc(n/100000)

  if (any(e100k<0 | e100k>6 | n100k<0 | n100k>12)) {
    return (list(ngr="100km-grid indices out of bounds"))
  }

  # Translate those into numeric equivalents of the grid letters
  l1 <- (19-n100k) - (19-n100k)%%5 + trunc((e100k+10)/5)
  l2 <- ((19-n100k)*5)%%25 + e100k%%5

  # Compensate for skipped 'I' and build grid letter-pairs
  l1 <- ifelse(l1 > 7, l1+1, l1)
  l2 <- ifelse(l2 > 7, l2+1, l2)
  a.code <- strtoi(charToRaw("A"), 16L) #A is the (charcode) origin to which l1 and l2 codes will be added
  let.pair <- paste0( vapply(as.raw(l1 + a.code), rawToChar,""),
                      vapply(as.raw(l2 + a.code), rawToChar,""))

  # Strip 100km-grid indices from easting & northing, and reduce precision
  # Note that rather than being rounded, the easting and northing are truncated
  # to hectometres (as the OS system demands), so the grid reference refers to the
  # lower left corner of the relevant square, to ensure the more precise
  # polygon will remain within the boundaries of the less precise polygon
  e <- trunc( (e%%100000) / (10 ^ (5-digits/2)) )
  n <- trunc( (n%%100000) / (10 ^ (5-digits/2)) )


  # Pad eastings & northings with leading zeros (just in case, allow up to 16-digit (mm) refs)
  e <- substr.r(paste0("00000000", e), (digits/2))
  n <- substr.r(paste0("00000000", n), (digits/2))

  #return
  ngr <- list(ngr=paste0(let.pair, " ", e, " ", n))
  if (num.elements > 0) {
    return (c(x[additional.elements], ngr))
  } else { return (ngr) }

}


#' @encoding UTF-8
#' @title GCS to BNG Easting/Northing
#'
#' @description
#' Converts a geodetic coordinate system to BNG (projected) Easting/Northing coordinates.
#'
#' @name sgs_latlon_bng
#' @usage sgs_latlon_bng(x, OSTN=TRUE)
#' @param x A \code{sgs_points} object with coordinates defined in a Geodetic
#' Coordinate System (eg. epsg=4258, 4326 or 4277)
#' @param OSTN Logical variable indicating whether use OSTN15 transformation when TRUE or
#' a less accurate but slightly faster single Helmert transformation when FALSE.
#' @details
#' The UK Ordnance Survey defined 'OSGB36' as the datum for the UK, based on the
#' 'Airy 1830' ellipsoid. However, in 2014, they deprecated OSGB36 in favour of
#' ETRS89 for latitude/longitude coordinates. Thus, \code{x} should be defined as
#' ETRS89 (or WGS84) most of the times.
#'
#' According to the Transformations and OSGM15 User Guide, p. 8: \emph{"...ETRS89 is a precise
#' version of the better known WGS84 reference system optimised for use in Europe;
#' however, for most purposes it can be considered equivalent to WGS84."} and \emph{"For
#' all navigation, mapping, GIS, and engineering applications within the tectonically
#' stable parts of Europe (including UK and Ireland), the term ETRS89 should
#' be taken as synonymous with WGS84."}
#' This means that EPSG:4258(ETRS89) and EPSG:4326(WGS84) will be considered equivalent
#' by this routine.
#'
#' \strong{Note}: All those coordinates outside the rectangle covered by OSTN15, will be automatically
#' computed using the small Helmert transformation. Such coordinates will be accurate
#' up to about +/-5 metres, therefore the resulting easting and northing coordinates
#' will be rounded to the metre.
#' Converting from lat/lon to BNG coordinates is faster than the other way around, due to
#' the iterative nature of the algorithm that is built into OSTN15.
#' @return
#' An object of class \code{sgs_points} whose coordinates are defined as
#' Easting/Northing (epsg=27700). They are adjusted to the SW corner of
#' 1m grid square.
#' @seealso \code{\link{sgs_points}}, \code{\link{sgs_bng_latlon}},
#' \code{\link{sgs_set_gcs}}.
#' @references
#' Ordnance Survey Limited, 2018. \emph{Transformations and OSGM15 user guide}
#' @examples
#' lat <- c(55.86424, 55.95325)
#' lon <- c(-4.25181,-3.18827)
#' pts <- sgs_points(list(latitude=lat, longitude=lon), epsg=4326)
#' bng.pts <- sgs_latlon_bng(pts)
#' @export
sgs_latlon_bng <- function(x, OSTN=TRUE) UseMethod("sgs_latlon_bng")

#' @export
sgs_latlon_bng.sgs_points <- function(x, OSTN=TRUE) {

  if (x$epsg == 27700) stop("This routine only supports geodetic coordinate systems")

  core.cols <- sgs_points.core
  #core.cols <- sgs_points.core[!sgs_points.core %in% c("easting", "northing")]

  additional.elements <- !names(x) %in% core.cols
  num.elements <- sum(additional.elements, na.rm=TRUE)

  if (OSTN) {

  # Convert datum from WGS84 to ETRS89
  if (x$epsg == 4326) { x <- sgs_set_gcs(x, to=4258) }

  projected <- project.onto.grid(x$latitude, x$longitude, x$datum)
  e <- projected[, 1]
  n <- projected[, 2]

  # If datum is OSGB36, no need to do anything else
  if (x$epsg == 4277) {

    # Round to mm precision
    e <- round(e, 3)
    n <- round(n, 3)

  }

  # If datum is WGS84/ETRS89, we need to adjust with OSTN15
  if (x$epsg == 4258) {

    shifts <- find.OSTN.shifts.at(e, n)
    # Round to mm precision
    e <- round(e + shifts$dx, 3)
    n <- round(n + shifts$dy, 3)

    # Helmert shift into OSGB36 and then reproject all those coordinates out of bounds of OSTN15.
    if (any(shifts$out) == TRUE) {
      helmert.x <- sgs_set_gcs(x[shifts$out], to = 4277)
      helmert.projected <- project.onto.grid(helmert.x$latitude, helmert.x$longitude, helmert.x$datum)
      e[shifts$out] <- round(helmert.projected[, 1], 0) # Round to metres
      n[shifts$out] <- round(helmert.projected[, 2], 0)
    }

  }

  } else {  # single Helmert transformation

  helmert.x <- sgs_set_gcs(x, to = 4277)
  helmert.projected <- project.onto.grid(helmert.x$latitude, helmert.x$longitude, helmert.x$datum)
  e <- round(helmert.projected[, 1], 0) # Round to metres
  n <- round(helmert.projected[, 2], 0)

  } # end if (OSTN)

  # Return values
  en <- list(x=e, y=n)
  if (num.elements > 0) en <- c(x[additional.elements], en)

  return (sgs_points(en, epsg=27700))

}

#' @encoding UTF-8
#' @title British National Grid (BNG) Easting/Northing to Geodetic Coordinate System (GCS)
#'
#' @description
#' Converts Ordnance Survey grid reference easting/northing coordinates to GCS
#' latitude/longitude (SW corner of grid square).
#'
#' @name sgs_bng_latlon
#' @usage sgs_bng_latlon(x, to = 4258, OSTN = TRUE)
#' @param x A \code{sgs_points} object with coordinates defined in the projected
#' coordinate system BNG (epsg=27700)
#' @param to Numeric. Sets the \code{epsg} code of the destination Geodetic
#' Coordinate System. 4258 (ETRS89) by default.
#' @param OSTN Logical variable indicating whether use OSTN15 transformation when TRUE or
#' a less accurate but slightly faster single Helmert transformation when FALSE.
#' @details
#' The UK Ordnance Survey defined 'OSGB36' as the datum for the UK, based on the
#' 'Airy 1830' ellipsoid. However, in 2014, they deprecated OSGB36 in favour of
#' ETRS89 for latitude/longitude coordinates. Thus, when converting to latitude/longitude
#' the OSGB36 datum should be always converted to ETRS89 (or WGS84).
#'
#' According to the Transformations and OSGM15 User Guide, p. 8: \emph{"...ETRS89 is a precise
#' version of the better known WGS84 reference system optimised for use in Europe;
#' however, for most purposes it can be considered equivalent to WGS84."} and \emph{"For
#' all navigation, mapping, GIS, and engineering applications within the tectonically
#' stable parts of Europe (including UK and Ireland), the term ETRS89 should
#' be taken as synonymous with WGS84."}
#' This means that EPSG:4258(ETRS89) and EPSG:4326(WGS84) will be considered equivalent
#' by this routine.
#'
#' If, for historical reasons, latitude/longitude coordinates must have the old OSGB36
#' datum, then the parameter \code{to} must be set to 4277.
#'
#' \strong{Note}: All those coordinates outside the rectangle covered by OSTN15, will be automatically
#' computed using the small Helmert transformation. Such coordinates will be accurate
#' up to about +/-5 metres.
#' Converting from BNG to lat/lon coordinates is slower than the other way around, due to
#' the iterative nature of the algorithm that is built into OSTN15.
#' @return
#' An object of class \code{sgs_points} whose coordinates are defined as
#' Latitude/Longitude.
#' @seealso \code{\link{sgs_points}}, \code{\link{sgs_latlon_bng}},
#' \code{\link{sgs_set_gcs}}.
#' @references
#' Ordnance Survey Limited, 2018. \emph{Transformations and OSGM15 user guide}
#' @examples
#' p <- sgs_points(list(651409.903, 313177.270), epsg=27700)
#' p.84 <- sgs_bng_latlon(p) #ETRS89 lat/lon
#' p.36 <- sgs_bng_latlon(p, to=4277) #OSGB36 lat/lon
#' @export
sgs_bng_latlon <- function(x, to=4258, OSTN=TRUE) UseMethod("sgs_bng_latlon")

#' @export
sgs_bng_latlon.sgs_points <- function(x, to=4258, OSTN=TRUE) {

  if (x$epsg != 27700) stop("This routine only supports BNG Easting and Northing entries")
  if (!to %in% c(4258, 4326, 4277)) stop("This routine only supports converting to epsg 4258, 4277 or 4326")

  core.cols <- sgs_points.core
  #core.cols <- sgs_points.core[!sgs_points.core %in% c("latitude", "longitude")]

  additional.elements <- !names(x) %in% core.cols
  num.elements <- sum(additional.elements, na.rm=TRUE)

  if (OSTN) {

  if (to == 4277) {
    unprojected <- unproject.onto.ellipsoid(x$easting, x$northing, x$datum)
    ##unprojected <- sgs_points(list(x=unprojected[, 1], y=unprojected[, 2]),
    ##                          epsg=to)
  }

  if (to == 4258 || to == 4326) {

    shifts <- find.OSTN.shifts.at(x$easting, x$northing)
    e <- x$easting - shifts$dx
    n <- x$northing - shifts$dy
    last.shifts <- shifts

    for (i in c(1:20)) {

      shifts <- find.OSTN.shifts.at(e, n)
      if (all(shifts$out) == TRUE) {
        # all coordinates have been shifted off the edge
        break
      }

      e <- x$easting - shifts$dx
      n <- x$northing - shifts$dy
      if (max(abs(shifts$dx - last.shifts$dx)) < 0.0001 &&
          max(abs(shifts$dy - last.shifts$dy)) < 0.0001) { break }
      last.shifts <- shifts

    }


    #initialise 'unprojected' matrix of coordinates
    items <- rep(NA, length(x$easting))
    unprojected <- cbind(items, items, deparse.level = 0)

    # unproject any shifted coordinates
    if (any(shifts$out) == FALSE) {
      e <- x$easting[!shifts$out] - shifts$dx[!shifts$out]
      n <- x$northing[!shifts$out] - shifts$dy[!shifts$out]
      unprojected[!shifts$out, ] <- unproject.onto.ellipsoid(e, n,
                                      epsgs[epsgs$epsg==to, "datum"])
    }

    # unproject the rest of coordinates (the ones that couldn't be shifted)
    if (any(shifts$out) == TRUE) {
      os.ll <- unproject.onto.ellipsoid(x$easting[shifts$out], x$northing[shifts$out], x$datum)
      os.ll.points <- sgs_set_gcs(sgs_points(list(x=os.ll[, 1], y=os.ll[, 2]), epsg=4277),
                                  to=to)
      unprojected[shifts$out, ] <- cbind(lat=os.ll.points$latitude, lon=os.ll.points$longitude)
    }

    ##unprojected <- sgs_points(list(x=unprojected[, 1], y=unprojected[, 2]), epsg=to)

  }
  ##
  unprojected <- list(x=unprojected[, 1], y=unprojected[, 2])
  if (num.elements > 0) unprojected <- c(x[additional.elements], unprojected)
  unprojected <- sgs_points(unprojected, coords=c("x", "y"), epsg=to)

  } else {  # single Helmert transformation

    os.ll <- unproject.onto.ellipsoid(x$easting, x$northing, x$datum)
    ##unprojected <- sgs_set_gcs(sgs_points(list(x=os.ll[, 1], y=os.ll[, 2]), epsg=4277),
    ##                           to=to)
    unprojected <- list(x=os.ll[, 1], y=os.ll[, 2])
    if (num.elements > 0) unprojected <- c(x[additional.elements], unprojected)
    unprojected <- sgs_set_gcs(sgs_points(unprojected, coords=c("x", "y"), epsg=4277), to=to)

  } # end if (OSTN)

  # In truth, we should have done the transformation to 4258 and then set_gcs to 4326,
  # but we consider them practically equal

  # Return
  ##if (num.elements > 0) unprojected <- c(x[additional.elements], unprojected)
  unprojected

}

#Helper function. Unproject BNG (OSGB36) to geodetic coordinates
unproject.onto.ellipsoid <- function(e, n, datum) {

  E <- e
  N <- n

  ellipsoid <- latlon.datum[latlon.datum$datum==datum, "ellipsoid"]
  a <- latlon.ellipsoid[latlon.ellipsoid$ellipsoid==ellipsoid, "a"]   # Major semi-axis
  b <- latlon.ellipsoid[latlon.ellipsoid$ellipsoid==ellipsoid, "b"]   # Minor semi-axis
  e2 <- latlon.ellipsoid[latlon.ellipsoid$ellipsoid==ellipsoid, "e2"] # eccentricity squared

  f0 = 0.9996012717                                     # Converge factor
  af <- a * f0
  n <- (a-b) / (a+b)
  N0 <- (-100000L); E0 <- (400000L)                     # northing & easting of true origin, metres

  dN <- N - N0
  dE <- E - E0
  phi0 <- 49L / 57.29577951308232087679815481410517     # NatGrid true origin is 49°N 2°W
  lambda0 <- -2L / 57.29577951308232087679815481410517

  phi <- phi0 + dN/af
  lambda <- lambda0


  M <- NA
  phi.plus <- NA
  phi.minus <- NA
  repeat {
    phi.minus <- phi - phi0
    phi.plus <- phi + phi0

    M <- b * f0 * (
      (1L + n * (1L + 5L/4L * n * (1L + n))) * phi.minus
      - 3L * n * (1L + n * (1L + 7L / 8L * n)) * sin(phi.minus) * cos(phi.plus)
      + (15L / 8L * n * (n * (1L + n))) * sin(2L * phi.minus) * cos(2L * phi.plus)
      - 35L / 24L * n^3 * sin(3L * phi.minus) * cos(3L * phi.plus)
      ) # meridional arc

    if ( max(abs(dN - M)) < 0.00001 ) { break }              # ie until < 0.01mm
    phi <- phi + (dN - M) / af
  }

  cos.phi <- cos(phi)
  sin.phi <- sin(phi)
  tan.phi <- sin.phi / cos.phi

  splat <- 1L - e2 * sin.phi * sin.phi
  sqrtsplat <- sqrt(splat)
  nu <- af / sqrtsplat                        # nu = transverse radius of curvature
  rho <- af * (1L - e2) / (splat * sqrtsplat) # rho = meridional radius of curvature
  eta2 <- nu / rho - 1L                       # eta = ?

  tan2.phi <- tan.phi * tan.phi
  VII <- tan.phi / (2L * rho * nu)
  VIII <- tan.phi / (24L * rho * nu^3) * (5L + eta2 + ( 3L - 9L * eta2 ) * tan2.phi)
  IX <- tan.phi / (720L * rho * nu^5) * (61L + ( 90L + 45L * tan2.phi ) * tan2.phi)

  sec.phi <- 1L / cos.phi

  X <- sec.phi / nu
  XI <- sec.phi / (6L * nu^3) * (nu / rho + 2L * tan2.phi)
  XII <- sec.phi / (120L * nu^5) * ( 5L + ( 28L + 24L * tan2.phi ) * tan2.phi)
  XIIA <- sec.phi / (5040L * nu^7) * ( 61L + ( 662L + ( 1320L + 720L * tan2.phi ) * tan2.phi ) * tan2.phi )

  dE2 <- dE * dE
  phi <- phi + ( -VII + ( VIII - IX * dE2 ) * dE2) * dE2
  lambda <- lambda + ( X + ( -XI + ( XII - XIIA * dE2 ) * dE2) * dE2) * dE

  unname(cbind(phi    * 57.29577951308232087679815481410517,
               lambda * 57.29577951308232087679815481410517))

}

#Helper function. Project geodetic coordinates onto BNG
project.onto.grid <- function (lat, lon, datum) {

  phi <- lat / 57.29577951308232087679815481410517
  lambda <- lon / 57.29577951308232087679815481410517

  ellipsoid <- latlon.datum[latlon.datum$datum==datum, "ellipsoid"]
  a <- latlon.ellipsoid[latlon.ellipsoid$ellipsoid==ellipsoid, "a"]   # Major semi-axis
  b <- latlon.ellipsoid[latlon.ellipsoid$ellipsoid==ellipsoid, "b"]   # Minor semi-axis
  e2 <- latlon.ellipsoid[latlon.ellipsoid$ellipsoid==ellipsoid, "e2"] # eccentricity squared

  f0 <- 0.9996012717                                    # Convergence factor
  af <- a * f0                                          # NatGrid scale factor on central meridian
  phi0 <- 49L / 57.29577951308232087679815481410517     # NatGrid true origin is 49°N 2°W
  lambda0 <- -2L / 57.29577951308232087679815481410517
  n0 <- -100000L; e0 <- 400000L                         # northing & easting of true origin, metres
  n <- (a-b)/(a+b)

  cos.phi <- cos(phi)
  sin.phi <- sin(phi)
  sin2.phi <- sin(phi) * sin(phi)
  tan.phi <- sin.phi / cos.phi                          # cos(phi) cannot be zero in GB
  tan2.phi <- tan.phi * tan.phi
  tan4.phi <- tan2.phi * tan2.phi

  splat <- 1L - e2 * sin2.phi
  sqrtsplat <- sqrt(splat)
  nu <- af / sqrtsplat                                  # nu = transverse radius of curvature
  rho <- af * (1L-e2) / (splat * sqrtsplat)             # rho = meridional radius of curvature
  eta2 <- nu / rho - 1L                                 # eta = ?

  phi.minus <- phi - phi0
  phi.plus <- phi + phi0

  # meridional arc
  M <- b * f0 * ((1L + n * (1L + 5L/4L * n * (1L + n)))* phi.minus
                 - 3L * n * (1L + n * (1L + 7L/8L * n))  * sin(phi.minus) * cos(phi.plus)
                 + (15L/8L * n * (n * (1L + n))) * sin(2L * phi.minus) * cos(2L * phi.plus)
                 - 35L/24L * n^3 * sin(3L * phi.minus) * cos(3L * phi.plus)
  )

  I <- M + n0
  II <- (nu/2L) *sin.phi * cos.phi
  III <- (nu/24L) * sin.phi * cos.phi^3 * (5L - tan2.phi + 9L * eta2)
  IIIA <- (nu/720L) * sin.phi * cos.phi^5 * (61L-58L * tan2.phi + tan4.phi)

  IV <- nu * cos.phi
  V <- nu/6L * cos.phi^3 * (nu/rho - tan2.phi)
  VI <- (nu/120L) * cos.phi^5 * (5L - 18L * tan2.phi + tan4.phi + 14L*eta2 - 58L*tan2.phi*eta2)

  dlambda <- lambda-lambda0

  n <- I +  ( II + ( III + IIIA * dlambda * dlambda ) * dlambda * dlambda ) * dlambda * dlambda
  e <- e0 + ( IV + ( V   + VI   * dlambda * dlambda ) * dlambda * dlambda ) * dlambda

  unname(cbind(e, n))

}

#Helper function. Get OSTN shift of coordinates
find.OSTN.shifts.at <- function(e, n) {

  # Initialise list of shifts
  items <- rep(NA, length(e))
  out <- rep(TRUE, length(e))
  shifts <- list(dx=items, dy=items, out=out)

  # No need to continue when everything is NA
  if (all(is.na(e))) { return (shifts) }

  # OSTN15 covers grid point (0, 0) to (700000,1250000)
  out.of.bounds <- (e < 0L | e > 700000L) | (n < 0L | n > 1250000L)
  shifts$out <- out.of.bounds


  if (!all(out.of.bounds)) {

    # set coordinates to km
    os.e <- e[!out.of.bounds] / 1000L
    os.n <- n[!out.of.bounds] / 1000L

    min.ee.shift <- 82140L
    min.nn.shift <- -84180L

    east.km <- trunc(os.e)
    north.km <- trunc(os.n)

    # R 'lists' are 1-based not 0 as in perl (where this code originally comes from)!
    lle <- min.ee.shift + ostn_east_shift[east.km + north.km * 701L + 1L]
    lre <- min.ee.shift + ostn_east_shift[east.km + north.km * 701L + 2L]
    ule <- min.ee.shift + ostn_east_shift[east.km + north.km * 701L + 702L]
    ure <- min.ee.shift + ostn_east_shift[east.km + north.km * 701L + 703L]

    lln <- min.nn.shift + ostn_north_shift[east.km + north.km * 701L + 1L]
    lrn <- min.nn.shift + ostn_north_shift[east.km + north.km * 701L + 2L]
    uln <- min.nn.shift + ostn_north_shift[east.km + north.km * 701L + 702L]
    urn <- min.nn.shift + ostn_north_shift[east.km + north.km * 701L + 703L]

    t <- os.e - east.km
    u <- os.n - north.km

    dx <- ((1L-t) * (1L-u) * lle + t * (1L-u) * lre + (1L-t) * u * ule + t * u * ure)/1000L
    dy <- ((1L-t) * (1L-u) * lln + t * (1L-u) * lrn + (1L-t) * u * uln + t * u * urn)/1000L

    shifts$dx[!out.of.bounds] <- dx
    shifts$dy[!out.of.bounds] <- dy

  }

  return (shifts)

}

# Helper function. Extract last n characters from a string
substr.r <- function(x, n){
  substr(x, nchar(x) - n+1, nchar(x))
}
