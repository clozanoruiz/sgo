#' @include sgs_wgs84.R
NULL

#' @encoding UTF-8
#' @title Coordinates transformation of a set of points
#'
#' @description
#' Transforms the coordinate system of a set of points to any supported coordinate system.
#'
#' @name sgs_transform
#' @usage sgs_transform(x, to = NULL)
#' @param x A \code{sgs_points} object.
#' @param to Specifies the EPSG code to convert the coordinates to. Currently it
#' can take any of the following values: \code{4326}, \code{3857}, \code{4277},
#' \code{27700} or \code{4258}.
#' @details
#' This function is a wrapper of specific transformation functions (\code{\link{sgs_bng_latlon}},
#' \code{\link{sgs_en_wgs84}}, \code{\link{sgs_latlon_bng}}, \code{\link{sgs_wgs84_en}})
#' that transforms the coordinate system of a set of points to any of the supported coordinate systems.
#' @return
#' An object of class 'sgs_points'.
#' @seealso \code{\link{sgs_points}}, \code{\link{sgs_points_xy}},
#' \code{\link{sgs_set_gcs}}, \code{\link{sgs_bng_ngr}}.
#' @examples
#' lt <- c(57.47777, 57.14965)
#' ln <- c(-4.22472, -2.09908)
#' n <- c("Inverness", "Aberdeen")
#' df <- data.frame(n, lt, ln, stringsAsFactors = FALSE)
#' locations <- sgs_points(df, coords=c("lt", "ln"), epsg=4326)
#'
#' locations.bng <- sgs_transform(locations, to=27700)
#' locations.osgb36 <- sgs_transform(locations, to=4277)
#' locations.ngr <- sgs_bng_ngr(sgs_transform(locations, to=27700))
#' locations.wgs84EN <- sgs_transform(locations.bng, to=3857)
#' @export
sgs_transform <- function(x, to=NULL) UseMethod("sgs_transform")

#' @export
sgs_transform.sgs_points <- function(x, to=NULL) {

  if (is.null(to)) stop("Paramater 'to' must be specified")
  if (x$epsg==to) return(x)

  # Get the list of functions and their arguments that need to be run for this transformation
  FUN_list <- FUNCS[which(FUNCS$FROM==x$epsg), as.character(to)][[1]]
  ARGS_list <- ARGS[which(ARGS$FROM==x$epsg), as.character(to)][[1]]

  # Run functions sequentially over the input
  for (i in 1:length(FUN_list)) {
    x <- do.call(FUN_list[[i]], c(list(x), ARGS_list[[i]]))
  }

  x

}

#Dataframes of operations and their arguments
#from: 4326  to:    -, 3857, 4277, 27700, 4258
#from: 3857  to: 4326,    -, 4277, 27700, 42584
#from: 4277  to: 4326, 3857,    -, 27700, 4258
#from: 27700 to: 4326, 3857, 4277,     -, 4258
#from: 4258  to: 4326, 3857, 4277, 27700,    -

FROM <-          list(4326,                                 3857,                                               4277,                                               27700,                              4258)

FUN_TO_4326 <-   list(NULL,                                 list(sgs_en_wgs84),                                 list(sgs_latlon_bng, sgs_bng_latlon),               list(sgs_bng_latlon),               list(sgs_set_gcs))
FUN_TO_3857 <-   list(list(sgs_wgs84_en),                   NULL,                                               list(sgs_latlon_bng, sgs_bng_latlon, sgs_wgs84_en), list(sgs_bng_latlon, sgs_wgs84_en), list(sgs_set_gcs, sgs_wgs84_en))
FUN_TO_4277 <-   list(list(sgs_latlon_bng, sgs_bng_latlon), list(sgs_en_wgs84, sgs_latlon_bng, sgs_bng_latlon), NULL,                                               list(sgs_bng_latlon),               list(sgs_latlon_bng, sgs_bng_latlon))
FUN_TO_27700 <-  list(list(sgs_latlon_bng),                 list(sgs_en_wgs84, sgs_latlon_bng),                 list(sgs_latlon_bng),                               NULL,                               list(sgs_latlon_bng))
FUN_TO_4258 <-   list(list(sgs_set_gcs),                    list(sgs_en_wgs84, sgs_set_gcs),                    list(sgs_latlon_bng, sgs_bng_latlon),               list(sgs_bng_latlon),               NULL)

ARGS_TO_4326 <-  list(NULL,                                 list(4326),                                         list(list(TRUE), list(4326, TRUE)),                 list(4326, TRUE),                   list(4326))
ARGS_TO_3857 <-  list(list(3857),                           NULL,                                               list(list(TRUE), list(4326, TRUE), list(3857)),     list(list(4326, TRUE), list(3857)), list(list(4326), list(3857)))
ARGS_TO_4277 <-  list(list(list(TRUE),list(4277, TRUE)),    list(list(4326), list(TRUE), list(4277, TRUE)),     NULL,                                               list(4277, TRUE),                   list(list(TRUE), list(4277, TRUE)))
ARGS_TO_27700 <- list(list(TRUE),                           list(list(4326), list(TRUE)),                       list(TRUE),                                         NULL,                               list(TRUE))
ARGS_TO_4258 <-  list(list(4258),                           list(list(4326), list(4258)),                       list(list(TRUE), list(4258, TRUE)),                 list(4258, TRUE),                   NULL)

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
