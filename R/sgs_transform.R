#' @include sgs_wgs84.R
NULL

#' @encoding UTF-8
#' @title Coordinate transformation of a set of points
#'
#' @description
#' Transforms the coordinate system of a set of points to any supported
#' coordinate system.
#'
#' @name sgs_transform
#' @usage sgs_transform(x, to = NULL, ...)
#' @param x A \code{sgs_points} object.
#' @param to Specifies the EPSG code to convert the coordinates to. See
#' \code{\link{sgs_points}} for a list of supported EPSG codes.
#' @param ... Additional parameters passed to internal functions. Currently it
#' supports the additional arguments seen in \code{sgs_bng_lonlat} and
#' \code{sgs_lonlat_bng}.
#' this description and which parameters may admit...
#' @details
#' This function is a wrapper of specific transformation functions
#' (\code{\link{sgs_bng_lonlat}}, \code{\link{sgs_en_wgs84}},
#' \code{\link{sgs_lonlat_bng}}, \code{\link{sgs_wgs84_en}},
#' \code{\link{sgs_laea_etrs}}, \code{\link{sgs_etrs_laea}},
#' \code{\link{sgs_cart_lonlat}}, \code{\link{sgs_lonlat_cart}}) that transforms
#' the coordinate system of a set of points to any of the supported coordinate
#' systems by this package.
#'
#' Please note that this package assumes that the Coordinate Reference Systems
#' (CRS) ETRS89 and WGS84 are the same within the UK, but this shouldn't be a
#' problem for most civilian use of GPS satellites. If a high-precision
#' transformation between WGS84 and ETRS89 is required then it is recommended
#' to use a different package to do the conversion.
#'
#' \strong{Warning: }Coordinates defined in the Geodetic Coordinate System
#' EPSG:4277 (with datum OSGB 1936) should only be used to convert to or from
#' BNG coordinates and for historical reasons only.
#' @return
#' An object of class 'sgs_points'.
#' @seealso \code{\link{sgs_points}}, \code{\link{sgs_coordinates}},
#' \code{\link{sgs_set_gcs}}, \code{\link{sgs_bng_ngr}}
#' @examples
#' ln <- c(-4.22472, -2.09908)
#' lt <- c(57.47777, 57.14965)
#' n <- c("Inverness", "Aberdeen")
#' df <- data.frame(n, ln, lt, stringsAsFactors = FALSE)
#' locations <- sgs_points(df, coords=c("ln", "lt"), epsg=4326)
#'
#' locations.bng <- sgs_transform(locations, to=27700)
#' locations.osgb36 <- sgs_transform(locations, to=4277)
#' locations.ngr <- sgs_bng_ngr(sgs_transform(locations, to=27700))
#' locations.wgs84EN <- sgs_transform(locations.bng, to=3857)
#' @export
sgs_transform <- function(x, to=NULL, ...) UseMethod("sgs_transform")

#' @export
sgs_transform.sgs_points <- function(x, to=NULL, ...) {

  if (is.null(to)) stop("Parameter 'to' must be specified")
  if (x$epsg==to) return(x)

  #if ((x$epsg == 4277 && (!to %in% c(27700,7405))) ||
  #    (to == 4277 && (!x$epsg %in% c(27700,7405)))) {
  #  warning(paste("Loss of accuracy.",
  #                "Transformations from or to EPSG:4277",
  #                "rely on a single Helmert transformation."))
  #}


  if(x$dimension == "XYZ") {
    core.elements <- .sgs_points.3d.core
  } else  {
    core.elements <- .sgs_points.2d.core
  }

  additional.elements <- !names(x) %in% core.elements
  num.elements <- sum(additional.elements, na.rm=TRUE)
  lst.additional.elements <- x[additional.elements]

  # Don't need all the extra columns it might have while doing calculations
  if (num.elements > 0)
    x <- structure(x[core.elements], class = "sgs_points")

  input.args <- c(list(x=x), list(...))
  input.args <- input.args[unique(names(input.args))] # remove repeated args

  # Get list of functions and their arguments needed to run this transformation
  FUN_list <- .FUNCS[which(.FUNCS$FROM == x$epsg), as.character(to)][[1]]
  ARGS_list <- .ARGS[which(.ARGS$FROM == x$epsg), as.character(to)][[1]]
  empty_args <- !length(ARGS_list)

  # Run functions sequentially over the input
  for (i in seq_along(FUN_list)) {
    func <- FUN_list[[i]]
    args <- input.args[names(input.args) %in% names(formals(func))]
    args_list <- if(empty_args) NULL else ARGS_list[[i]]
    args <- c(args, args_list)
    input.args$x <- do.call(func, args)
  }

  # Return sgs_points
  ret <- input.args$x
  if (num.elements > 0) {
    ret <- structure(c(ret[setdiff(names(ret), .sgs_points.attr)],
                       lst.additional.elements,
                       ret[.sgs_points.attr]), class = "sgs_points")
  }
  ret

}


# Helper functions to remove or add the Z coordinate (EPSGs 27700 / 7405)
.remove.z <- function(x) {
  #x$height.datum <- NULL
  x$z <- NULL
  x$dimension <- "XY"
  x$epsg <- 27700
  x
}
.add.z <- function(x) {
  x$z <- rep(0, length(x$x))
  x$dimension <- "XYZ"
  x$epsg <- 7405
  structure(x[c(.sgs_points.3d.coords,
                names(x)[!names(x) %in% .sgs_points.3d.coords])],
            class="sgs_points")
}

#Dataframes of operations and their arguments
#from:4326  to:   -, 3857, 4277, 27700, 4258, 4979, 4978, 4937, 4936, 7405, 3035
#from:3857  to:4326,    -, 4277, 27700, 4258, 4979, 4978, 4937, 4936, 7405, 3035
#from:4277  to:4326, 3857,    -, 27700, 4258, 4979, 4978, 4937, 4936, 7405, 3035
#from:27700 to:4326, 3857, 4277,     -, 4258, 4979, 4978, 4937, 4936, 7405, 3035
#from:4258  to:4326, 3857, 4277, 27700,    -, 4979, 4978, 4937, 4936, 7405, 3035
#from:4979  to:4326, 3857, 4277, 27700, 4258,    -, 4978, 4937, 4936, 7405, 3035
#from:4978  to:4326, 3857, 4277, 27700, 4258, 4979,    -, 4937, 4936, 7405, 3035
#from:4937  to:4326, 3857, 4277, 27700, 4258, 4979, 4978,    -, 4936, 7405, 3035
#from:4936  to:4326, 3857, 4277, 27700, 4258, 4979, 4978, 4937,    -, 7405, 3035
#from:7405  to:4326, 3857, 4277, 27700, 4258, 4979, 4978, 4937, 4936,    -, 3035
#from:3035  to:4326, 3857, 4277, 27700, 4258, 4979, 4978, 4937, 4936, 7405,    -

.FROM <-          list(4326, 3857, 4277, 27700, 4258,
                       4979 ,4978, 4937, 4936, 7405,
                       3035)

                                                           #FROM:
.FUN_TO_4326 <-   list(NULL,                                 #4326
                       list(sgs_en_wgs84),                   #3857
                       list(sgs_lonlat_bng, sgs_bng_lonlat), #4277
                       list(sgs_bng_lonlat),                #27700
                       list(sgs_set_gcs),                    #4258
                       list(sgs_set_gcs),                    #4979
                       list(sgs_cart_lonlat, sgs_set_gcs),   #4978
                       list(sgs_set_gcs),                    #4937
                       list(sgs_cart_lonlat, sgs_set_gcs),   #4936
                       list(sgs_bng_lonlat),                 #7405
                       list(sgs_laea_etrs, sgs_set_gcs)      #3035
)
.ARGS_TO_4326 <-  list(NULL,                                 #4326
                       list(to=4326),                        #3857
                       list(list(to=27700), list(to=4326)),  #4277
                       list(to=4326),                       #27700
                       list(to=4326),                        #4258
                       list(to=4326),                        #4979
                       list(list(), list(to=4326)),          #4978
                       list(to=4326),                        #4937
                       list(list(), list(to=4326)),          #4936
                       list(to=4326),                        #7405
                       list(list(), list(to=4326))           #3035
)

                                                                         #FROM:
.FUN_TO_3857 <-   list(list(sgs_wgs84_en),                                 #4326
                       NULL,                                               #3857
                       list(sgs_lonlat_bng, sgs_bng_lonlat, sgs_wgs84_en), #4277
                       list(sgs_bng_lonlat, sgs_wgs84_en),
                       list(sgs_wgs84_en),                                 #4258
                       list(sgs_wgs84_en),                                 #4979
                       list(sgs_cart_lonlat, sgs_wgs84_en),                #4978
                       list(sgs_wgs84_en),                                 #4937
                       list(sgs_cart_lonlat, sgs_wgs84_en),                #4936
                       list(sgs_bng_lonlat, sgs_wgs84_en),                 #7405
                       list(sgs_laea_etrs, sgs_wgs84_en)                   #3035
)
.ARGS_TO_3857 <-  list(list(to=3857),
                       NULL,
                       list(list(to=27700), list(to=4326), list(to=3857)),
                       list(list(to=4326), list(to=3857)),
                       list(list(to=3857)),                                #4258
                       list(to=3857),                                      #4979
                       list(list(), to=3857),                              #4978
                       list(list(to=3857)),                                #4937
                       list(list(), list(to=3857)),                        #4936
                       list(list(to=4326), list(to=3857)),                 #7405
                       list(list(), list(to=3857))                         #3035
)

                                                                         #FROM:
.FUN_TO_4277 <-   list(list(sgs_lonlat_bng, sgs_bng_lonlat),               #4326
                       list(sgs_en_wgs84, sgs_lonlat_bng, sgs_bng_lonlat), #3857
                       NULL,                                               #4277
                       list(sgs_bng_lonlat),                              #27700
                       list(sgs_lonlat_bng, sgs_bng_lonlat),               #4258
                       list(sgs_lonlat_bng, sgs_bng_lonlat),               #4979
                       list(sgs_cart_lonlat, sgs_lonlat_bng, sgs_bng_lonlat),
                       list(sgs_lonlat_bng, sgs_bng_lonlat),               #4937
                       list(sgs_cart_lonlat, sgs_lonlat_bng, sgs_bng_lonlat),
                       list(sgs_bng_lonlat),                               #7405
                       list(sgs_laea_etrs, sgs_lonlat_bng, sgs_bng_lonlat) #3035
)
.ARGS_TO_4277 <-  list(list(list(to=27700),list(to=4277)),                 #4326
                       list(list(to=4326), list(to=27700), list(to=4277)), #3857
                       NULL,                                               #4277
                       list(to=4277),                                     #27700
                       list(list(to=27700), list(to=4277)),                #4258
                       list(list(to=27700), list(to=4277)),                #4979
                       list(list(), list(to=27700), list(to=4277)),        #4978
                       list(list(to=27700), list(to=4277)),                #4937
                       list(list(), list(to=27700), list(to=4277)),        #4936
                       list(to=4277),                                      #7405
                       list(list(), list(to=27700), list(to=4277))         #3035
)


.FUN_TO_27700 <-  list(list(sgs_lonlat_bng),                               #4326
                       list(sgs_en_wgs84, sgs_lonlat_bng),                 #3857
                       list(sgs_lonlat_bng),
                       NULL,                                              #27700
                       list(sgs_lonlat_bng),                               #4258
                       list(sgs_lonlat_bng),                               #4979
                       list(sgs_cart_lonlat, sgs_lonlat_bng),              #4978
                       list(sgs_lonlat_bng),                               #4937
                       list(sgs_cart_lonlat, sgs_lonlat_bng),              #4936
                       list(.remove.z),                                    #7405
                       list(sgs_laea_etrs, sgs_lonlat_bng)                 #3035
)
.ARGS_TO_27700 <- list(list(to=27700),
                       list(list(to=4326), list(to=27700)),
                       list(to=27700),
                       NULL,
                       list(to=27700),                                     #4258
                       list(to=27700),                                     #4979
                       list(list(), list(to=27700)),                       #4978
                       list(to=27700),                                     #4937
                       list(list(), list(to=27700)),                       #4936
                       list(),                                             #7405
                       list(list(), list(to=27700))                        #3035
)


.FUN_TO_4258 <-   list(list(sgs_set_gcs),
                       list(sgs_en_wgs84, sgs_set_gcs),                    #3857
                       list(sgs_lonlat_bng, sgs_bng_lonlat),               #4277
                       list(sgs_bng_lonlat),
                       NULL,                                               #4258
                       list(sgs_set_gcs),                                  #4979
                       list(sgs_cart_lonlat, sgs_set_gcs),                 #4978
                       list(sgs_set_gcs),                                  #4937
                       list(sgs_cart_lonlat, sgs_set_gcs),                 #4936
                       list(sgs_bng_lonlat),                               #7405
                       list(sgs_laea_etrs)                                 #3035
)
.ARGS_TO_4258 <-  list(list(to=4258),                                      #4326
                       list(list(to=4326), list(to=4258)),                 #3857
                       list(list(to=27700), list(to=4258)),                #4277
                       list(to=4258),                                     #27700
                       NULL,                                               #4258
                       list(to=4258),                                      #4979
                       list(list(), list(to=4258)),                        #4978
                       list(to=4258),                                      #4937
                       list(list(), list(to=4258)),                        #4936
                       list(to=4258),                                      #7405
                       list(list())                                        #3035
)

                                                                         #FROM:
.FUN_TO_4979 <- list(list(sgs_set_gcs),                                    #4326
                     list(sgs_en_wgs84, sgs_set_gcs),                      #3857
                     list(sgs_lonlat_bng, sgs_bng_lonlat),                 #4277
                     list(sgs_bng_lonlat),                                #27700
                     list(sgs_set_gcs),                                    #4258
                     NULL,                                                 #4979
                     list(sgs_cart_lonlat),                                #4978
                     list(sgs_set_gcs),                                    #4937
                     list(sgs_cart_lonlat, sgs_set_gcs),                   #4936
                     list(sgs_bng_lonlat),                                 #7405
                     list(sgs_laea_etrs, sgs_set_gcs)                      #3035

)
.ARGS_TO_4979 <- list(list(to=4979),                                       #4326
                      list(list(to=4326), list(to=4979)),                  #3857
                      list(list(to=27700), list(to=4979)),                 #4277
                      list(to=4979),                                      #27700
                      list(to=4979),                                       #4258
                      NULL,                                                #4979
                      list(),                                              #4978
                      list(to=4979),                                       #4937
                      list(list(), list(to=4979)),                         #4936
                      list(to=4979),                                       #7405
                      list(list(), list(to=4979))                          #3035
)

                                                                         #FROM:
.FUN_TO_4978 <- list(list(sgs_lonlat_cart),                                #4326
                     list(sgs_en_wgs84, sgs_lonlat_cart),                  #3857
                     list(sgs_lonlat_bng, sgs_bng_lonlat, sgs_lonlat_cart),#4277
                     list(sgs_bng_lonlat, sgs_lonlat_cart),               #27700
                     list(sgs_set_gcs, sgs_lonlat_cart),                   #4258
                     list(sgs_lonlat_cart),                                #4979
                     NULL,                                                 #4978
                     list(sgs_set_gcs, sgs_lonlat_cart),                   #4937
                     list(sgs_cart_lonlat, sgs_set_gcs, sgs_lonlat_cart),  #4936
                     list(sgs_bng_lonlat, sgs_lonlat_cart),                #7405
                     list(sgs_laea_etrs, sgs_set_gcs, sgs_lonlat_cart)     #3035
)
.ARGS_TO_4978 <- list(list(),                                              #4326
                      list(list(to=4326), list()),                         #3857
                      list(list(to=27700), list(to=4326), list()),         #4277
                      list(list(to=4326), list()),                        #27700
                      list(list(to=4326), list()),                         #4258
                      list(),                                              #4979
                      NULL,
                      list(list(to=4979), list()),                         #4937
                      list(list(), list(to=4979), list()),                 #4936
                      list(list(to=4979), list()),                         #7405
                      list(list(), list(to=4326), list())                  #3035
)


.FUN_TO_4937 <- list(list(sgs_set_gcs),
                     list(sgs_en_wgs84, sgs_set_gcs),
                     list(sgs_lonlat_bng, sgs_bng_lonlat, sgs_set_gcs),    #4277
                     list(sgs_bng_lonlat, sgs_set_gcs),                   #27700
                     list(sgs_set_gcs),                                    #4258
                     list(sgs_set_gcs),                                    #4979
                     list(sgs_cart_lonlat, sgs_set_gcs),                   #4978
                     NULL,
                     list(sgs_cart_lonlat),
                     list(sgs_bng_lonlat),                                 #7405
                     list(sgs_laea_etrs, sgs_set_gcs)                      #3035
)
.ARGS_TO_4937 <- list(list(to=4937),                                       #4326
                      list(list(to=4326), list(to=4937)),                  #3857
                      list(list(to=27700), list(to=4258), list(to=4937)),  #4277
                      list(list(), list(to=4937)),                        #27700
                      list(to=4937),                                       #4258
                      list(to=4937),                                       #4979
                      list(list(), list(to=4937)),                         #4978
                      NULL,                                                #4937
                      list(to=4937),                                       #4936
                      list(to=4937),                                       #7405
                      list(list(), list(to=4937))                          #3035
)


.FUN_TO_4936 <- list(list(sgs_set_gcs, sgs_lonlat_cart),
                     list(sgs_en_wgs84, sgs_set_gcs, sgs_lonlat_cart),     #3857
                     list(sgs_lonlat_bng, sgs_bng_lonlat, sgs_lonlat_cart),#4277
                     list(sgs_bng_lonlat, sgs_lonlat_cart),
                     list(sgs_lonlat_cart),                                #4258
                     list(sgs_set_gcs, sgs_lonlat_cart),                   #4979
                     list(sgs_cart_lonlat, sgs_set_gcs, sgs_lonlat_cart),  #4978
                     list(sgs_lonlat_cart),
                     NULL,
                     list(sgs_bng_lonlat,sgs_lonlat_cart),                 #7405
                     list(sgs_laea_etrs, sgs_lonlat_cart)                  #3035
)
.ARGS_TO_4936 <- list(list(list(to=4258), list()),
                      list(list(to=4326), list(to=4258), list()),
                      list(list(to=27700), list(to=4258), list()),
                      list(list(to=4258), list()),
                      list(),                                              #4258
                      list(list(to=4937), list()),
                      list(list(), list(to=4937), list()),
                      list(),
                      NULL,
                      list(list(to=4937), list()),                         #7405
                      list(list(), list())                                 #3035
)


.FUN_TO_7405 <- list(list(sgs_lonlat_bng),                                 #4326
                     list(sgs_en_wgs84, sgs_lonlat_bng),                   #3857
                     list(sgs_lonlat_bng),                                 #4277
                     list(.add.z),                                        #27700
                     list(sgs_lonlat_bng),                                 #4258
                     list(sgs_lonlat_bng),                                 #4979
                     list(sgs_cart_lonlat, sgs_lonlat_bng),                #4978
                     list(sgs_lonlat_bng),                                 #4937
                     list(sgs_cart_lonlat, sgs_lonlat_bng),                #4936
                     NULL,                                                 #7405
                     list(sgs_laea_etrs, sgs_lonlat_bng)                   #3035
)
.ARGS_TO_7405 <- list(list(list(to=7405)),                                 #4326
                      list(list(to=4326), list(to=7405)),
                      list(list(to=7405)),                                 #4277
                      list(),                                             #27700
                      list(list(to=7405)),                                 #4258
                      list(to=7405),                                       #4979
                      list(list(), list(to=7405)),                         #4978
                      list(to=7405),                                       #4937
                      list(list(), list(to=7405)),                         #4936
                      NULL,                                                #7405
                      list(list(), list(to=7405))                          #3035
)


.FUN_TO_3035 <- list(list(sgs_set_gcs, sgs_etrs_laea),                     #4326
                     list(sgs_en_wgs84, sgs_set_gcs, sgs_etrs_laea),       #3857
                     list(sgs_lonlat_bng, sgs_bng_lonlat, sgs_etrs_laea),  #4277
                     list(sgs_bng_lonlat, sgs_etrs_laea),                 #27700
                     list(sgs_etrs_laea),                                  #4258
                     list(sgs_set_gcs, sgs_etrs_laea),                     #4979
                     list(sgs_cart_lonlat, sgs_set_gcs, sgs_etrs_laea),    #4978
                     list(sgs_etrs_laea),                                  #4937
                     list(sgs_etrs_laea),                                  #4936
                     list(sgs_bng_lonlat, sgs_etrs_laea),                  #7405
                     NULL                                                  #3035
)
.ARGS_TO_3035 <- list(list(list(to=4258), list()),                         #4326
                      list(list(to=4326), list(to=4258), list()),          #3857
                      list(list(to=27700), list(to=4258), list()),         #4277
                      list(list(to=4258), list()),                        #27700
                      list(),                                              #4258
                      list(list(to=4937), list()),                         #4979
                      list(list(), list(to=4937), list()),                 #4978
                      list(),                                              #4937
                      list(),                                              #4936
                      list(list(to=4937), list()),                         #7405
                      NULL                                                 #3035
)


# Two dataframes containing those funtions and arguments respectively
.FUNCS <- as.data.frame(cbind("FROM"=.FROM,
                              "4326"=.FUN_TO_4326,
                              "3857"=.FUN_TO_3857,
                              "4277"=.FUN_TO_4277,
                              "27700"=.FUN_TO_27700,
                              "4258"=.FUN_TO_4258,
                              "4979"=.FUN_TO_4979,
                              "4978"=.FUN_TO_4978,
                              "4937"=.FUN_TO_4937,
                              "4936"=.FUN_TO_4936,
                              "7405"=.FUN_TO_7405,
                              "3035"=.FUN_TO_3035
))

.ARGS <- as.data.frame(cbind("FROM"=.FROM,
                             "4326"=.ARGS_TO_4326,
                             "3857"=.ARGS_TO_3857,
                             "4277"=.ARGS_TO_4277,
                             "27700"=.ARGS_TO_27700,
                             "4258"=.ARGS_TO_4258,
                             "4979"=.ARGS_TO_4979,
                             "4978"=.ARGS_TO_4978,
                             "4937"=.ARGS_TO_4937,
                             "4936"=.ARGS_TO_4936,
                             "7405"=.ARGS_TO_7405,
                             "3035"=.ARGS_TO_3035
))
