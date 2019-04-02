#' @encoding UTF-8
#' @title Create sf object from list of coordinates or wkb
#'
#' @description
#' Creates an sf object containing a collection of polygons (planar surfaces defined
#' by one exterior boundary and zero or more interior boundaries) from a list of
#' coordinates or WKB fields. This object facilitates all the operations with polygons
#' in this package.
#'
#' @name sgs_polygons
#' @usage sgs_polygons(x, epsg = NULL, wkb = NULL, coords = NULL, by = NULL, buffer = NULL)
#' @param x A dataframe.
#' @param epsg Numeric. Sets the \code{epsg} code of the coordinates (or WKB field)
#' present int he dataframe.
#' @param wkb Character. Name of the column containing WKB data. If this parameter is
#' informed then the parameter \code{coords} is ignored.
#' @param coords Vector of chracters containing the names of the columns with
#' coordinates information. \code{wkb} must be NULL when using this paramater.
#' @param by Character. Name of the column that will be used to group series of points
#' into a single polygon. Ignored when using the paramater \code{wkb}.
#' @param buffer Numeric or character. If numeric, then it defines the buffer to apply to each polygon.
#' If it is a string then it must contain the name of the column having buffer distances
#' to apply to each polygon. In this case every row of the column must contain a number because
#' when grouping points with \code{by} the routine will also aggregate buffer distances by the
#' same column.
#' \code{buffer} must be expressed in the same units as the coordinates (decimal degrees or meters).
#' Take into account that \code{sf} does not correctly buffer longitude/latitude data.
#' @return
#' An object of class \code{sf} containing the corresponding geometries
#' (and IDs when there are multiple polygons).
#' @seealso \code{\link{sgs_points}}.
#' @examples
#' df <- data.frame(latitude=c(55.86424,55.95325,55.87426,56.87426,55.75426),
#'                  longitude=c(-4.25181,-3.18827,-3.78690,-2.78690,-3.54690),
#'                  id = c(1,1,1,2,2))
#' # create a single polygon disregarding different id's
#' polygon <- sgs_polygons(df, epsg=4326, coords=c("latitude","longitude"))
#'
#' # create a set of polygons, identified by a column in df
#' polygons <- sgs_polygons(df, epsg=4326, coords=c("latitude","longitude"), by="id")
#' @export
sgs_polygons <- function (x, epsg=NULL, wkb=NULL, coords=NULL, by=NULL, buffer=NULL)
  UseMethod("sgs_polygons")

#' @export
sgs_polygons.data.frame <- function (x, epsg=NULL, wkb=NULL, coords=NULL,
                                     by=NULL, buffer=NULL) {

  if (is.null(epsg))
    stop("The EPSG code can not be empty.")

  if (!epsg %in% epsgs$epsg)
    stop("The EPSG code entered doesn't seem to be correct.")

  if (is.null(wkb) && is.null(coords)) return (NULL)

  if(!is.null(wkb)) {
    # When wkb is informed we ignore the 'groupby' parameter

    # Remove ‘AsIs’ class otherwise sf complains.
    # Perhaps in future versions (sf > 0.6) this become superfluous
    class(x[, wkb]) <- class(x[, wkb])[-match("AsIs", class(x[, wkb]))]

    polygons <- sf::st_sf(x, crs=epsg, stringsAsFactors = FALSE)
    if (!is.null(buffer)) {
      dist <- if (is.numeric(buffer)) buffer else x[, buffer]
      polygons <- apply_buffer(polygons, dist)
    }

    return (polygons)

  }

  if(is.null(wkb)) {

    if(is.null(by)) { #There will be only one polygon

      points <- sf::st_as_sf(x, coords=coords, crs=epsg, stringsAsFactors = FALSE)
      if (length(points$geometry) == 2) {
        polygons <- sf::st_as_sfc(sf::st_bbox(points))
      } else {
        polygons <- sf::st_convex_hull(sf::st_union(points))
      }

      if (!is.null(buffer)) {
        dist <- if (is.numeric(buffer)) buffer else x[, buffer]
        polygons <- apply_buffer(polygons, dist[1]) #There's only one polygon
      }

      polygons <- sf::st_sf(geometry=polygons, stringsAsFactors = FALSE)

    } else { # 'by' is not null, multiple polygons.

      polygons <- by(x, list(x[, by]), FUN=function(x, coords, crs) {
                                        pol <- sf::st_as_sf(x, coords=coords, crs=crs,
                                                        stringsAsFactors = FALSE)
                                        if (length(pol$geometry) == 2) {
                                          sf::st_as_sfc(sf::st_bbox(pol))
                                        } else {
                                          sf::st_convex_hull(sf::st_union(pol))
                                        }
                                      }, coords=coords, crs=epsg)
      ids <- names(polygons)
      polygons <- sf::st_sf(id=ids, geometry=sf::st_sfc(polygons), crs=epsg,
                            stringsAsFactors = FALSE)

      if (!is.null(buffer)) {
        dist <- if (is.numeric(buffer)) {
          buffer
        } else {
          stats::aggregate(x[, buffer], by=list(x[, by]), mean)$x
        }
        polygons <- apply_buffer(polygons, dist)
      }

    }

    return (polygons)

  }

}
