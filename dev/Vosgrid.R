#https://github.com/chrisveness/geodesy/blob/master/osgridref.js
#https://github.com/chrisveness/geodesy/blob/master/latlon-ellipsoidal.js

# extract last n charcaters from a string
substr.r <- function(x, n){
  substr(x, nchar(x) - n+1, nchar(x))
}

# Ellipsoid parameters; major axis (a), minor axis (b), and flattening (f) for each ellipsoid.
#For all practical applications GRS80 abd WGS84 ellipsoids are identical
lonlat.ellipsoid <- data.frame(ellipsoid=c("WGS84", "Airy1830","GRS80"),
                               a=c(6378137, 6377563.396, 6378137),
                               b=c(6356752.31425, 6356256.909, 6356752.314140),
                               f=c(1/298.257223563, 1/299.3249646, 1/298.257222101),
                               stringsAsFactors = FALSE)


# Datums; with associated ellipsoid, and Helmert transform parameters to convert from WGS 84 into
# given datum.

# Note that precision of various datums will vary, and WGS-84 (original) is not defined to be
# accurate to better than ±1 metre. No transformation should be assumed to be accurate to better
# than a meter; for many datums somewhat less (OSGB36 to WGS84 or ETRS89 the lost of accuracy can
# be up to 5m with single Helmert transformations.

# transforms: t in metres, s in ppm, r in arcseconds  
lonlat.datum = data.frame(datum=c("OSGB36", "WGS84", "ETRS89"),
                          ellipsoid=c("Airy1830","WGS84", "ETRS89"),
                          tx=c(-446.448, 0.0, 0.0),
                          ty=c(125.157, 0.0, 0.0),
                          tz=c(-542.060, 0.0, 0.0),
                          s=c(20.4894, 0.0, 0.0),
                          rx=c(-0.1502, 0.0, 0.0),
                          ry=c(-0.2470, 0.0, 0.0),
                          rz=c(-0.8421, 0.0, 0.0),
                          stringsAsFactors = FALSE)

# type: "WGS84" or "BNG.LL" or "BNG.EN"
# as of now only working with OSGB36 and WGS84 datums
sgs_coords <- function (x, y, type=NULL) {

  if(!is.numeric(x) | !is.numeric(y)) stop("All coordinates must be numeric")

  if(is.null(type) || (type != "WGS84" && type != "BNG.LL" && type != "BNG.EN")) {
    stop("The type parameter must be entered as 'WGS84', 'BNG.LL' or 'BNG.EN'")
  }

  coords <- switch(type,
    WGS84 = structure(list(longitude = x, latitude = y, type=type, datum="WGS84"), class="sgs_coords"),
    BNG.LL = structure(list(longitude = x, latitude = y, type=type, datum="OSGB36"), class="sgs_coords"),
    BNG.EN = structure(list(easting = x, northing = y, type=type, datum="OSGB36"), class="sgs_coords")
  )

  return (coords)

}

# Converts from (geodetic) latitude/longitude coordinates to (geocentric)
# cartesian (x/y/z) coordinates.
lonlat_to_cartesian <- function(coords) {
  datum <- coords$datum
  ellipsoid <- lonlat.datum[lonlat.datum$datum==datum, "ellipsoid"]

  phi <- coords$latitude * pi / 180
  lambda <- coords$longitude * pi / 180
  h = 0 # height above ellipsoid - not currently used
  a <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid, "a"]
  f <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid, "f"]

  sin.phi <- sin(phi); cos.phi <- cos(phi)
  sin.lambda <- sin(lambda); cos.lambda <- cos(lambda)

  eSq <- 2*f - f*f                          # 1st eccentricity squared ≡ (a²-b²)/a²
  nu <- a / sqrt(1 - eSq*sin.phi*sin.phi)   # radius of curvature in prime vertical

  x <- (nu+h) * cos.phi * cos.lambda
  y <- (nu+h) * cos.phi * sin.lambda
  z <- (nu*(1-eSq)+h) * sin.phi

  point <- list(x=x, y=y, z=z)

  return (point)

}

# Applies Helmert transform  using transform parameters t.
apply_transform <- function(point, t)   {
  # this point
  x1 <- point$x; y1 <- point$y; z1 <- point$z

  # transform parameters
  tx <- t$tx                    # x-shift
  ty <- t$ty                    # y-shift
  tz <- t$tz                    # z-shift
  s1 <- t$s/1e6 + 1             # scale: normalise parts-per-million to (s+1)
  rx <- (t$rx/3600) * pi / 180  # x-rotation: normalise arcseconds to radians
  ry <- (t$ry/3600) * pi / 180  # y-rotation: normalise arcseconds to radians
  rz <- (t$rz/3600) * pi / 180  # z-rotation: normalise arcseconds to radians

  # apply transform
  x2 <- tx + x1*s1 - y1*rz + z1*ry
  y2 <- ty + x1*rz + y1*s1 - z1*rx
  z2 <- tz - x1*ry + y1*rx + z1*s1

  point <- list(x=x2, y=y2, z=z2)

  return (point)

}

# Converts cartesian (x/y/z) point to (ellipsoidal geodetic) latitude/longitude
# coordinates on specified datum.
cartesian_to_lonlat <- function(point, datum) {
  x <- point$x; y <- point$y; z <- point$z

  ellipsoid <- lonlat.datum[lonlat.datum$datum==datum, "ellipsoid"]
  params <- lonlat.ellipsoid[lonlat.ellipsoid$ellipsoid==ellipsoid, 2:4]
  a <- params$a
  b <- params$b
  f <- params$f

  e2 <- 2*f - f*f       # 1st eccentricity squared ≡ (a²-b²)/a²
  eps2 <- e2 / (1-e2)   # 2nd eccentricity squared ≡ (a²-b²)/b²
  p <- sqrt(x*x + y*y)  # distance from minor axis
  R <- sqrt(p*p + z*z)  # polar radius

  # parametric latitude (Bowring eqn 17, replacing tanβ = z·a / p·b)
  tan.beta <- (b*z)/(a*p) * (1+eps2*b/R)
  sin.beta <- tan.beta / sqrt(1+tan.beta*tan.beta)
  cos.beta <- sin.beta / tan.beta

  # geodetic latitude (Bowring eqn 18: tanφ = z+ε²bsin³β / p−e²cos³β)
  phi <- ifelse (is.nan(cos.beta), 0, atan2(z + eps2*b*sin.beta*sin.beta*sin.beta,
                                            p - e2*a*cos.beta*cos.beta*cos.beta))

  # longitude
  lambda <- atan2(y, x)

  # height above ellipsoid (Bowring eqn 7) [not currently used]
  sin.phi <- sin(phi); cos.phi <- cos(phi)
  nu <- a/sqrt(1-e2*sin.phi*sin.phi) # length of the normal terminated by the minor axis
  h <- unname(p*cos.phi + z*sin.phi - (a*a/nu))

  coords <- sgs_coords(x=lambda * 180 / pi, y=phi * 180 / pi, type=datum)

  return (coords)

}

convert_datum <- function(coords, to) {
  
  UseMethod("convert_datum", coords)
  
}

convert_datum.default <- function(coords) {
  
  print ("Anything to do?")
  return(coords)
  
}

# Converts lat/lon coordinate to new coordinate system.
convert_datum.sgs_coords <- function(coords, to="OSGB36") {
  old.coords <- coords
  transform <- NULL

  if (old.coords$datum == to) { return (coords) }
  
  if (old.coords$datum == "WGS84") {
    # converting from WGS 84
    transform <- lonlat.datum[lonlat.datum$datum==to, 3:9]
  }
  if (to == "WGS84") {
    # converting to WGS 84; use inverse transform (don't overwrite original!)
    transform <- -lonlat.datum[lonlat.datum$datum==old.coords$datum, 3:9]
  }
  if (is.null(transform)) {
    # neither this.datum nor toDatum are WGS84: convert this to WGS84 first
    old.coords <- convert_datum(coords, to="WGS84")
    transform <- lonlat.datum[lonlat.datum$datum==to, 3:9]
  }

  old.cartesian <- lonlat_to_cartesian(old.coords)           # convert polar to cartesian...
  new.cartesian <- apply_transform(old.cartesian, transform) # ...apply transform...
  new.lonlat <- cartesian_to_lonlat(new.cartesian, to)       # ...and convert cartesian to polar

  return (new.lonlat)

}

# Accepts standard grid references (eg 'SU 387 148'), with or without whitespace separators, from
# two-digit references up to 10-digit references (1m × 1m square)
str_to_bng <- function(grid.ref) {

  err.msg <- "Invalid grid reference(s)"

  # validate format
  match <- grepl("^[A-Z]{2}\\s*[0-9]+\\s*[0-9]+$", grid.ref, ignore.case=TRUE, perl=TRUE)
  if (any(match==FALSE)) stop( err.msg )

  # remove all types of whitespace
  grid.ref <- gsub("\\s", "", grid.ref) 

  # get numeric values of letter references, mapping A->0, B->1, C->2, etc:
  a.code <- strtoi(charToRaw("A"), 16L)
  l1 <- strtoi(vapply(substr(toupper(grid.ref), 1, 1), charToRaw, as.raw(0)), 16L) - a.code
  l2 <- strtoi(vapply(substr(toupper(grid.ref), 2, 2), charToRaw, as.raw(0)), 16L) - a.code
  # shuffle down letters after 'I' since 'I' is not used in grid:
  l1 <- ifelse (l1 > 7, l1-1, l1)
  l2 <- ifelse (l2 > 7, l2-1, l2)

  # convert grid letters into 100km-square indexes from false origin (grid square SV):
  e100km <- ((l1-2)%%5)*5 + (l2%%5)
  n100km <- (19-floor(l1/5)*5) - floor(l2/5)

  # skip grid letters to get numeric (easting/northing) part of ref (splitting numbers half way)
  en <- substring(grid.ref, 3)
  n.char <- nchar(en)
  e <- substr(en, 1, n.char/2)
  n <- substr(en, n.char/2 + 1, n.char)

  # validation
  if (any(e100km<0 | e100km>6 | n100km<0 | n100km>12)) stop( err.msg )
  if (any(nchar(e) != nchar(n))) stop( err.msg )

  # standardise to 10-digit refs (metres)
  e <- paste0( e100km, substr(paste0(e, '00000'), 1, 5) )
  n <- paste0( n100km, substr(paste0(n, '00000'), 1, 5) )

  return(sgs_coords(as.numeric(e), as.numeric(n), type="BNG.EN"))

}

bng_to_str <- function(coords, digits) {

  UseMethod("bng_to_str", coords)

}

bng_to_str.default <- function(coords) {

  print ("Should I try to convert it?")
  return(coords)

}

# Converts numeric grid reference to standard OS grid reference.
bng_to_str.sgs_coords <- function(coords, digits=10) {

  if (coords$type != "BNG.EN") stop("This routine only supports BNG Easting and Northing entries")

  if (digits%%2!=0 || digits>16) stop(paste0("Invalid precision 'digits=", digits, "'"))

  e <- coords$easting
  n <- coords$northing

  # use digits = 0 to return numeric format (in metres, allowing for decimals & for northing > 1e6)
  if (digits == 0) {

    e.int <- floor(e); e.dec <- e - e.int
    n.int <- floor(n); n.dec <- n - n.int
    e.pad <- paste0(substr.r(paste0('000000', e.int), 6), 
                    ifelse(e.dec>0, substring(as.character(round(e.dec, 3)), 2), ""))
    n.pad <- paste0(ifelse(n.int<1e6, substr.r(paste0('000000', n.int), 6), n.int), 
                    ifelse(n.dec>0, substring(as.character(round(n.dec, 3)), 2), ""))
    
    return (paste0(e.pad, ",",  n.pad))
  }

  # get the 100km-grid indices
  e100k <- floor(e/100000); n100k <- floor(n/100000)

  if (any(e100k<0 | e100k>6 | n100k<0 | n100k>12)) {
    return ("")
  }

  # translate those into numeric equivalents of the grid letters
  l1 <- (19-n100k) - (19-n100k)%%5 + floor((e100k+10)/5)
  l2 <- ((19-n100k)*5)%%25 + e100k%%5

  # compensate for skipped 'I' and build grid letter-pairs
  l1 <- ifelse(l1 > 7, l1+1, l1)
  l2 <- ifelse(l2 > 7, l2+1, l2)
  a.code <- strtoi(charToRaw("A"), 16L) #A is the (charcode) origin to which l1 and l2 codes will be added
  let.pair <- paste0( vapply(as.raw(l1 + a.code), rawToChar,""),
                      vapply(as.raw(l2 + a.code), rawToChar,""))

  # strip 100km-grid indices from easting & northing, and reduce precision
  #e <- floor( (e%%100000) / (10 ^ (5-digits/2)) )
  #n <- floor( (n%%100000) / (10 ^ (5-digits/2)) )
  e <- round( (e%%100000) / (10 ^ (5-digits/2)) )
  n <- round( (n%%100000) / (10 ^ (5-digits/2)) )

  # pad eastings & northings with leading zeros (just in case, allow up to 16-digit (mm) refs)
  e <- substr.r(paste0('00000000', e), (digits/2))
  n <- substr.r(paste0('00000000', n), (digits/2))

  grid.ref <- paste0(let.pair, " ", e, " ", n)

  return (grid.ref)

}

# reserve the name of the function
lonlat_to_bng <- function(coords) {

  UseMethod("lonlat_to_bng", coords)

}

lonlat_to_bng.default <- function(coords) {

  print ("Should I try to convert it?")
  return(coords)

}

# Converts latitude/longitude to Ordnance Survey grid reference easting/northing coordinate.
lonlat_to_bng.sgs_coords <- function(coords) {

  if (coords$type != "WGS84") stop("This routine only supports WGS84 Longitude and Latitude entries")

  # if necessary convert to OSGB36 first
  if (coords$datum != "OSGB36") {
    coords <- convert_datum(coords, to = "OSGB36")
  }

  phi <- coords$latitude * pi / 180
  lambda <- coords$longitude * pi / 180

  a <- 6377563.396; b <- 6356256.909                    # Airy 1830 major and minor semi-axes
  f0 = 0.9996012717                                     # NatGrid scale factor on central meridian
  phi0 <- (49) * pi / 180; lambda0 <- (-2) * pi / 180   # NatGrid true origin is 49°N 2°W
  n0 <- (-100000); e0 <- (400000)                       # northing & easting of true origin, metres
  e2 <- (1) - (b * b) / (a * a)                         # eccentricity squared
  n <- (a-b)/(a+b); n2 <- n*n; n3 <- n^3                # n, n², n³

  cos.phi <- cos(phi); sin.phi <- sin(phi)
  nu = a * f0 / sqrt((1)-e2*sin.phi*sin.phi)            # nu = transverse radius of curvature
  rho = a*f0*(1-e2) / ((1-e2*sin.phi*sin.phi) ^ 1.5)    # rho = meridional radius of curvature
  eta2 = nu/rho-1;                                      # eta = ?

  Ma <- (1 + n + (5/4)*n2 + (5/4)*n3) * (phi-phi0)
  Mb <- (3*n + 3*n*n + (21/8)*n3) * sin(phi-phi0) * cos(phi+phi0)
  Mc <- ((15/8)*n2 + (15/8)*n3) * sin(2*(phi-phi0)) * cos(2*(phi+phi0))
  Md <- (35/24)*n3 * sin(3*(phi-phi0)) * cos(3*(phi+phi0))
  M <- b * f0 * (Ma - Mb + Mc - Md)                     # meridional arc

  cos3.phi <- cos.phi^3
  cos5.phi <- cos.phi^5
  tan2.phi <- tan(phi)*tan(phi)
  tan4.phi <- tan2.phi*tan2.phi

  I <- M + n0
  II <- (nu/2)*sin.phi*cos.phi
  III <- (nu/24)*sin.phi*cos3.phi*(5-tan2.phi + 9*eta2)
  IIIA <- (nu/720)*sin.phi*cos5.phi*(61-58*tan2.phi+tan4.phi)
  IV <- nu*cos.phi
  V <- (nu/6)*cos3.phi*(nu/rho-tan2.phi);
  VI <- (nu/120) * cos5.phi * (5 - 18*tan2.phi + tan4.phi + 14*eta2 - 58*tan2.phi*eta2)

  offset.lambda <- lambda-lambda0
  offset.lambda2 <- offset.lambda*offset.lambda
  offset.lambda3 <- offset.lambda2*offset.lambda
  offset.lambda4 <- offset.lambda3*offset.lambda
  offset.lambda5 <- offset.lambda4*offset.lambda
  offset.lambda6 <- offset.lambda5*offset.lambda

  n <- I + II*offset.lambda2 + III*offset.lambda4 + IIIA*offset.lambda6
  e <- e0 + IV*offset.lambda + V*offset.lambda3 + VI*offset.lambda5

  # round to mm precision
  n <- round(n, 3)
  e <- round(e, 3)

  return(sgs_coords(e, n, type="BNG.EN")) # gets truncated to SW corner of 1m grid square

}

# reserve the name of the function
bng_to_lonlat <- function(coords, to) {
  
  UseMethod("bng_to_lonlat", coords)
  
}

bng_to_lonlat.default <- function(coords) {
  
  print ("Should I try to convert it?")
  return(coords)
  
}

# Converts Ordnance Survey grid reference easting/northing coordinate to
# latitude/longitude (SW corner of grid square).
bng_to_lonlat.sgs_coords <- function(coords, to="WGS84") {

  if (coords$type != "BNG.EN") stop("This routine only supports BNG Easting and Northing entries")
  if (to != "WGS84" && to != "BNG.LL") stop("This routine only supports converting to BNG.LL or WGS84 Lon Lat")

  E <- coords$easting
  N <- coords$northing

  a <- 6377563.396; b <- 6356256.909                    # Airy 1830 major and minor semi-axes
  f0 = 0.9996012717                                     # NatGrid scale factor on central meridian
  phi0 <- (49) * pi / 180; lambda0 <- (-2) * pi / 180   # NatGrid true origin is 49°N 2°W
  N0 <- (-100000); E0 <- (400000)                       # northing & easting of true origin, metres
  e2 <- (1) - (b * b) / (a * a)                         # eccentricity squared
  n <- (a-b)/(a+b); n2 <- n*n; n3 <- n^3                # n, n², n³

  phi <- phi0; M <- 0
  repeat {
    phi <- (N-N0-M)/(a*f0) + phi

    Ma <- (1 + n + (5/4)*n2 + (5/4)*n3) * (phi-phi0)
    Mb <- (3*n + 3*n2 + (21/8)*n3) * sin(phi-phi0) * cos(phi+phi0)
    Mc <- ((15/8)*n2 + (15/8)*n3) * sin(2*(phi-phi0)) * cos(2*(phi+phi0))
    Md <- (35/24)*n3 * sin(3*(phi-phi0)) * cos(3*(phi+phi0))
    M <- b * f0 * (Ma - Mb + Mc - Md)                   # meridional arc
    
    if ( min(N-N0-M) < 0.00001 ) { break }                   # ie until < 0.01mm
  }

  cos.phi <- cos(phi); sin.phi <- sin(phi)
  nu = a * f0 / sqrt((1)-e2*sin.phi*sin.phi)            # nu = transverse radius of curvature
  rho = a*f0*(1-e2) / ((1-e2*sin.phi*sin.phi) ^ 1.5)    # rho = meridional radius of curvature
  eta2 = nu/rho-1;                                      # eta = ?
 
  tan.phi <- tan(phi)
  tan2.phi <- tan.phi*tan.phi
  tan4.phi <- tan2.phi*tan2.phi
  tan6.phi <- tan4.phi*tan2.phi

  sec.phi <- 1/cos.phi
  nu3 <- nu^3; nu5 <- nu^5; nu7 <- nu^7
  VII <- tan.phi/(2*rho*nu)
  VIII <- tan.phi/(24*rho*nu3)*(5+3*tan2.phi+eta2-9*tan2.phi*eta2)
  IX <- tan.phi/(720*rho*nu5)*(61+90*tan2.phi+45*tan4.phi)
  X <- sec.phi/nu
  XI <- sec.phi/(6*nu3)*(nu/rho+2*tan2.phi)
  XII <- sec.phi/(120*nu5)*(5+28*tan2.phi+24*tan4.phi)
  XIIA <- sec.phi/(5040*nu7)*(61+662*tan2.phi+1320*tan4.phi+720*tan6.phi)

  dE <- (E-E0); dE2 <- dE*dE; dE3 <- dE^3; dE4 <- dE^4; dE5 <- dE^5; dE6 <- dE^6; dE7 <- dE^7
  phi <- phi - VII*dE2 + VIII*dE4 - IX*dE6
  lambda <- lambda0 + X*dE - XI*dE3 + XII*dE5 - XIIA*dE7

  coords <- sgs_coords(x=(lambda * 180 / pi), y=(phi * 180 / pi), type="BNG.LL")
  if (to != "BNG.LL") {
    coords <- convert_datum(coords, to=to)
  }

  return(coords)

}
