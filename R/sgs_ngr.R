################################################################################
#                                                                              #
# Adapted from 'geodesy', a JS routine by Chris Veness:                        #
# https://github.com/chrisveness/geodesy                                       #
#                                                                              #
################################################################################


#' @encoding UTF-8
#' @title NGR to BNG Easting/Northing
#'
#' @description
#' Converts OS National Grid References to Easting/Northing coordinates
#'
#' @name sgs_ngr_bng
#' @usage sgs_ngr_bng(x, col = NULL, check.only = FALSE)
#' @param x A data.frame, list or vector containing strings describing OS
#' National Grid References, with or without whitespace separators.
#' (e.g. 'SU 387 148').
#' @param col Character string with the name of the 'column' containing the
#' vector of NGR values, it is required when \code{x} is a list with more than
#' one column.
#' @param check.only Logical parameter. If it is set to TRUE then the routine
#' returns a logical vector indicating which references are correct.
#' @details
#' All entered standard grid references can range from two-digit references up
#' to 10-digit references (1m × 1m square).
#' If \code{x} is a list with 2 or more vector elements, \code{col} can be used
#' to inform the function which of the elements contains the NGR strings. The
#' rest of the elements will be appended to the resulting object. See examples.
#' @return
#' An object of class \code{sgs_points} whose coordinates are defined as
#' Easting/Northing when \code{check.only} is kept as FALSE. Otherwise, it
#' returns a logical vector indicating which grid references are correct and
#' which ones are not.
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
#' grid <- sgs_bng_ngr(sgs_points(list(x=247455, y=706338, name="Ben Venue"),
#'                                coords=c("x","y"),
#'                                epsg=27700))
#' bng <- sgs_ngr_bng(grid, col="ngr")
#'
#' # test
#' bad <- c("NN 166 712", "AA 3863 7653")
#' check <- sgs_ngr_bng(bad, check.only=TRUE) #returns a logical vector
#' @export
sgs_ngr_bng <- function(x, col=NULL, check.only=FALSE) UseMethod("sgs_ngr_bng")

#' @export
sgs_ngr_bng.list <- function(x, col=NULL, check.only=FALSE) {

  err.msg <- "Invalid grid reference(s)"
  old.x <- x

  if (!is.null(col)) {
    x <- unlist(x[col], use.names = FALSE)
  } else {
    if (length(x) > 1 && length(x[[1]]) > 1) {
      #error if more than one list in x and 'col' is not defined
      stop("Parameter 'col' must be entered")
    } else {
      x <- unlist(x, use.names = FALSE)
    }
  }


  # Validate format
  # Parse only printable ASCII characters (excluding spaces)
  # https://en.wikipedia.org/wiki/ASCII
  x <- gsub("[^\x21-\x7E]", "", x)
  match <- grepl(
    "^([HNOSThnost][A-Za-z]\\s?)(\\d{2}|\\d{4}|\\d{6}|\\d{8}|\\d{10})$", x,
    ignore.case=TRUE, perl=TRUE)
  invalid.indices <- match==FALSE

  if (check.only) {

    return (match)

  }

  if (any(invalid.indices)) {
    if ( all( is_nothing(x[which(invalid.indices)]) ) ) {
      #from here we will only work with those that do not have invalid values
      #x.holder <- rep(NA, length(x))
      #x <- x[!invalid.indices]
      stop( paste(err.msg , ": ",
                  "There are empty or null coordinates",sep=" "))
    } else {
      invalid.values <- x[which(invalid.indices)]
      one.invalid.value <- invalid.values[which(!is_nothing(invalid.values))][1]
      stop( paste(err.msg , ": ",  one.invalid.value, sep=" "))
    }
  }


  # Get numeric values of letter references, mapping A->0, B->1, C->2, etc:
  a.code <- strtoi(charToRaw("A"), 16L)
  l1 <- strtoi(vapply(substr(toupper(x), 1, 1),
                      charToRaw, as.raw(0)), 16L) - a.code
  l2 <- strtoi(vapply(substr(toupper(x), 2, 2),
                      charToRaw, as.raw(0)), 16L) - a.code
  # Shuffle down letters after 'I' since 'I' is not used in grid:
  l1 <- ifelse (l1 > 7, l1-1, l1)
  l2 <- ifelse (l2 > 7, l2-1, l2)

  # Convert grid letters into 100km-square indexes from false origin
  # (grid square SV):
  e100km <- ((l1-2)%%5)*5 + (l2%%5)
  n100km <- (19-trunc(l1/5)*5) - trunc(l2/5)

  # Skip grid letters to get numeric (easting/northing) part of the
  # reference (splitting numbers half way)
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
    lst <- c(old.x[!names(old.x) %in% col],
             list(x=as.numeric(e), y=as.numeric(n)))
  }
  sgs_points(lst, coords=c("x", "y"), epsg=27700)

}

#' @export
sgs_ngr_bng.data.frame <- function(x, col=NULL, check.only=FALSE) {

  sgs_ngr_bng(as.list(x), col=col, check.only=check.only)

}

#' @export
sgs_ngr_bng.default <- function(x, col=NULL, check.only=FALSE) {

  if (is.atomic(x)) {
    sgs_ngr_bng(list(x), col=col, check.only=check.only)
  } else {
    stop("sgs_ngr_bng only works with atomic vectors, lists or dataframes")
  }

}


#' @encoding UTF-8
#' @title BNG Easting/Northing to National Grid References (NGR)
#'
#' @description
#' Converts BNG Easting/Northing coordinates to National Grid References
#'
#' @name sgs_bng_ngr
#' @usage sgs_bng_ngr(x, digits = 10)
#' @param x A \code{sgs_points} object with coordinates defined as
#' \code{epsg=27700} or \code{epsg=7405}.
#' @param digits Numeric. It defines the precision of the resulting grid
#' references.
#' @details
#' All resulting grid references will have 10 digits (1m × 1m square) by
#' default. In order to reduce the output precision change the digits parameter
#' accordingly. When \code{digits=0}, it returns the numeric format of the grid
#' references.
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
#' sgs <- sgs_points(list(x=247455, y=706338, name="Ben Venue"),
#' coords=c("x", "y"), epsg=27700)
#' grid10 <- sgs_bng_ngr(sgs)
#' grid8 <- sgs_bng_ngr(sgs, digits=8)
#' #and notice the truncating, not rounding, of grid8 regarding grid10.
#' @export
sgs_bng_ngr <- function(x, digits=10) UseMethod("sgs_bng_ngr")


#' @export
sgs_bng_ngr.sgs_points <- function(x, digits=10) {

  if (!x$epsg %in% c(27700, 7405))
    stop("This routine only supports BNG Easting and Northing entries")

  if (digits%%2!=0 || digits>16)
    stop(paste0("Invalid precision 'digits=", digits, "'"))

  core.cols <- sgs_points.core

  additional.elements <- !names(x) %in% core.cols
  num.elements <- sum(additional.elements, na.rm=TRUE)
  e <- x$x
  n <- x$y

  # Use digits = 0 to return numeric format
  # (in metres, allowing for decimals & for northing > 1e6)
  if (digits == 0) {

    e.int <- trunc(e); e.dec <- e - e.int
    n.int <- trunc(n); n.dec <- n - n.int
    e.pad <- paste0(substr.r(paste0('000000', e.int), 6),
                    ifelse(e.dec>0,
                           substring(as.character(round(e.dec, 3)), 2), ""))
    n.pad <- paste0(ifelse(n.int<1e6,
                           substr.r(paste0('000000', n.int), 6), n.int),
                    ifelse(n.dec>0,
                           substring(as.character(round(n.dec, 3)), 2), ""))

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
  # A is the (charcode) origin to which l1 and l2 codes will be added:
  a.code <- strtoi(charToRaw("A"), 16L)
  let.pair <- paste0( vapply(as.raw(l1 + a.code), rawToChar,""),
                      vapply(as.raw(l2 + a.code), rawToChar,""))

  # Strip 100km-grid indices from easting & northing, and reduce precision
  # Note that rather than being rounded, the easting and northing are truncated
  # to hectometres (as the OS system demands), so the grid reference refers to
  # the lower left corner of the relevant square, to ensure the more precise
  # polygon will remain within the boundaries of the less precise polygon
  e <- trunc( (e%%100000) / (10 ^ (5-digits/2)) )
  n <- trunc( (n%%100000) / (10 ^ (5-digits/2)) )


  # Pad eastings & northings with leading zeros (just in case,
  # allow up to 16-digit (mm) refs)
  e <- substr.r(paste0("00000000", e), (digits/2))
  n <- substr.r(paste0("00000000", n), (digits/2))

  #return
  ngr <- list(ngr=paste0(let.pair, " ", e, " ", n))
  if (num.elements > 0) {
    return (c(x[additional.elements], ngr))
  } else { return (ngr) }

}
