#' sgs: Simple GIS (In Spotfire).
#'
#' The sgs package provides a set of functions to perform spatial or geographic
#' analysis in R, or using 'TIBCO Spotfire' and 'TERR'.
#'
#' @encoding UTF-8
#' @section Object constructors:
#' \itemize{
#' \item\code{\link{sgs_points}}: 2D point coordinates
#' \item\code{\link{sgs_polygons}}: \code{sf} polygons.
#' }
#'
#' @section Transformation and Conversion functions:
#' Functions to provide coordinate transformations between:
#' \itemize{
#' \item\code{\link{sgs_bng_lonlat}}: British National Grid (E/N) and Lon/Lat
#' \item\code{\link{sgs_lonlat_bng}}: Lon/Lat and British National Grid (E/N)
#' \item\code{\link{sgs_bng_ngr}}: British National Grid (E/N) and National Grid
#' References
#' \item\code{\link{sgs_ngr_bng}}: National Grid References and British National
#' Grid (E/N)
#' \item\code{\link{sgs_wgs84_en}}: WGS84 Lon/Lat and Pseudo-Mercator (E/N)
#' \item\code{\link{sgs_en_wgs84}}: Pseudo-Mercator (E/N) and WGS84 Lon/Lat
#' \item\code{\link{sgs_transform}}: Wrapper for all the transformations above
#' }
#'
#' Functions to convert between data formats:
#' \itemize{
#' \item\code{\link{sgs_points_sf}}: Conversion from sgs_points to sf
#' }
#'
#' @section Disclaimer:
#' The OSTN15 transformation model is used in this package, and it is licensed
#' under the BSD Licence (http://opensource.org/licenses/bsd-license.php):\cr
#' \emph{© Copyright and database rights Ordnance Survey Limited 2016, © Crown
#' copyright and database rights Land & Property Services 2016 and/or © Ordnance
#' Survey Ireland, 2016. All rights reserved.}
#' @docType package
#' @name sgs
NULL
