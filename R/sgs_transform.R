#' @include sgs_wgs84.R
NULL

#' @encoding UTF-8
#' @title Coordinates transformation of a set of points
#'
#' @description
#' Transforms the coordinate system of a set of points to any supported
#' coordinate system.
#'
#' @name sgs_transform
#' @usage sgs_transform(x, to = NULL)
#' @param x A \code{sgs_points} object.
#' @param to Specifies the EPSG code to convert the coordinates to. Currently it
#' can take any of the following values: \code{4326}, \code{3857}, \code{4277},
#' \code{27700} or \code{4258}.
#' @details
#' This function is a wrapper of specific transformation functions
#' (\code{\link{sgs_bng_lonlat}}, \code{\link{sgs_en_wgs84}},
#' \code{\link{sgs_lonlat_bng}}, \code{\link{sgs_wgs84_en}}) that transforms the
#' coordinate system of a set of points to any of the supported coordinate
#' systems.
#' @return
#' An object of class 'sgs_points'.
#' @seealso \code{\link{sgs_points}}, \code{\link{sgs_coordinates}},
#' \code{\link{sgs_set_gcs}}, \code{\link{sgs_bng_ngr}}.
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
sgs_transform <- function(x, to=NULL) UseMethod("sgs_transform")

#' @export
sgs_transform.sgs_points <- function(x, to=NULL) {

  if (is.null(to)) stop("Parameter 'to' must be specified")
  if (x$epsg==to) return(x)

  # Get list of functions and their arguments needed to run this transformation
  FUN_list <- FUNCS[which(FUNCS$FROM==x$epsg), as.character(to)][[1]]
  ARGS_list <- ARGS[which(ARGS$FROM==x$epsg), as.character(to)][[1]]

  # Run functions sequentially over the input
  for (i in seq_along(FUN_list)) {
    x <- do.call(FUN_list[[i]], c(list(x), ARGS_list[[i]]))
  }

  x

}

#Dataframes of operations and their arguments
#from: 4326  to:    -, 3857, 4277, 27700, 4258, 4979, 4978, 4937, 4936, 7405
#from: 3857  to: 4326,    -, 4277, 27700, 4258, 4979, 4978, 4937, 4936, 7405
#from: 4277  to: 4326, 3857,    -, 27700, 4258, 4979, 4978, 4937, 4936, 7405
#from: 27700 to: 4326, 3857, 4277,     -, 4258, 4979, 4978, 4937, 4936, 7405
#from: 4258  to: 4326, 3857, 4277, 27700,    -, 4979, 4978, 4937, 4936, 7405
#from: 4979  to: 4326, 3857, 4277, 27700, 4258,    -, 4978, 4937, 4936, 7405
#from: 4978  to: 4326, 3857, 4277, 27700, 4258, 4979,    -, 4937, 4936, 7405
#from: 4937  to: 4326, 3857, 4277, 27700, 4258, 4979, 4978,    -, 4936, 7405
#from: 4936  to: 4326, 3857, 4277, 27700, 4258, 4979, 4978, 4937,    -, 7405
#from: 7405  to: 4326, 3857, 4277, 27700, 4258, 4979, 4978, 4937, 4936,    -

FROM <-          list(4326, 3857, 4277, 27700, 4258,
                      4979 ,4978, 4937, 4936, 7405)

                                                            #FROM:
FUN_TO_4326 <-   list(NULL,                                 #4326
                      list(sgs_en_wgs84),                   #3857
                      list(sgs_lonlat_bng, sgs_bng_lonlat), #4277
                      list(sgs_bng_lonlat),                 #27700
                      list(sgs_set_gcs),                    #4258
                      list(sgs_set_gcs),                    #4979
                      list(sgs_cart_lonlat, sgs_set_gcs),   #4978
                      list(sgs_set_gcs),                    #4937
                      list(sgs_cart_lonlat, sgs_set_gcs),   #4936
                      list(sgs_bng_lonlat)                  #7405
)
ARGS_TO_4326 <-  list(NULL,                                 #4326
                      list(4326),                           #3857
                      list(list(TRUE), list(4326, TRUE)),   #4277
                      list(4326, TRUE),                     #27700
                      list(4326),                           #4258
                      list(4326),                           #4979
                      list(list(), list(4326)),             #4978
                      list(4326),                           #4937
                      list(list(), list(4326)),             #4936
                      list(4326, TRUE)                      #7405
)

                                                                          #FROM:
FUN_TO_3857 <-   list(list(sgs_wgs84_en),                                 #4326
                      NULL,                                               #3857
                      list(sgs_lonlat_bng, sgs_bng_lonlat, sgs_wgs84_en), #4277
                      list(sgs_bng_lonlat, sgs_wgs84_en),
                      list(sgs_set_gcs, sgs_wgs84_en),                    #4258
                      list(sgs_wgs84_en),                                 #4979
                      list(sgs_cart_lonlat, sgs_wgs84_en),                #4978
                      list(sgs_set_gcs, sgs_wgs84_en),                    #4937
                      list(sgs_cart_lonlat, sgs_set_gcs, sgs_wgs84_en),   #4936
                      list(sgs_bng_lonlat, sgs_wgs84_en)                  #7405
)
ARGS_TO_3857 <-  list(list(3857),
                      NULL,
                      list(list(TRUE), list(4326, TRUE), list(3857)),
                      list(list(4326, TRUE), list(3857)),
                      list(list(4326), list(3857)),
                      list(3857),                                         #4979
                      list(list(), 3857),                                 #4978
                      list(list(4326), list(3857)),                       #4937
                      list(list(), list(4326), list(3857)),               #4936
                      list(list(4326, TRUE), list(3857))                  #7405
)


FUN_TO_4277 <-   list(list(sgs_lonlat_bng, sgs_bng_lonlat),
                      list(sgs_en_wgs84, sgs_lonlat_bng, sgs_bng_lonlat),
                      NULL,
                      list(sgs_bng_lonlat),
                      list(sgs_lonlat_bng, sgs_bng_lonlat),
                      list(),
                      list(),
                      list(),
                      list(),
                      list()
)
ARGS_TO_4277 <-  list(list(list(TRUE),list(4277, TRUE)),
                      list(list(4326), list(TRUE), list(4277, TRUE)),
                      NULL,
                      list(4277, TRUE),
                      list(list(TRUE), list(4277, TRUE)),
                      list(), #4979
                      list(), #4978
                      list(), #4937
                      list(), #4936
                      list() #7405
)


FUN_TO_27700 <-  list(list(sgs_lonlat_bng),
                      list(sgs_en_wgs84, sgs_lonlat_bng),
                      list(sgs_lonlat_bng),
                      NULL,
                      list(sgs_lonlat_bng),
                      list(), #4979
                      list(), #4978
                      list(), #4937
                      list(), #4936
                      list() #7405
)
ARGS_TO_27700 <- list(list(TRUE),
                      list(list(4326), list(TRUE)),
                      list(TRUE),
                      NULL,
                      list(TRUE),
                      list(), #4979
                      list(), #4978
                      list(), #4937
                      list(), #4936
                      list() #7405
)


FUN_TO_4258 <-   list(list(sgs_set_gcs),
                      list(sgs_en_wgs84, sgs_set_gcs),
                      list(sgs_lonlat_bng, sgs_bng_lonlat),
                      list(sgs_bng_lonlat),
                      NULL,
                      list(), #4979
                      list(), #4978
                      list(), #4937
                      list(), #4936
                      list() #7405
)
ARGS_TO_4258 <-  list(list(4258),
                      list(list(4326), list(4258)),
                      list(list(TRUE), list(4258, TRUE)),
                      list(4258, TRUE),
                      NULL,
                      list(), #4979
                      list(), #4978
                      list(), #4937
                      list(), #4936
                      list() #7405
)


# Two dataframes containing those funtions and arguments respectively
FUNCS <- as.data.frame(cbind("FROM"=FROM,
                             "4326"=FUN_TO_4326,
                             "3857"=FUN_TO_3857,
                             "4277"=FUN_TO_4277,
                             "27700"=FUN_TO_27700,
                             "4258"=FUN_TO_4258
))

ARGS <- as.data.frame(cbind("FROM"=FROM,
                            "4326"=ARGS_TO_4326,
                            "3857"=ARGS_TO_3857,
                            "4277"=ARGS_TO_4277,
                            "27700"=ARGS_TO_27700,
                            "4258"=ARGS_TO_4258
))
