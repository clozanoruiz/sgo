#' sgo: Simple Geographical Operations (with OSGB36).
#'
#' The sgo package aims to help with spatial or geographic analysis in Open
#' Source R and derivatives. Its main purpose is to perform OSGB36/ETRS89
#' transformations using the Ordnance Survey's OSTN15 transformation model for
#' Great Britain and the Isle of Man. It also transforms GPS ellipsoid heights
#' to orthometric (mean sea level) heights on the relevant Ordnance Survey
#' mapping datum, using the National Geoid Model OSGM15.
#'
#' Most of the functions available in this package will become much less
#' accurate if used outside the coverage of OSTN, therefore it is advised to
#' apply these functions on coordinates within Great Britain, the Isle of Man
#' and any areas of sea less than a few miles off shore.
#'
#' This package assumes that the Coordinate Reference Systems (CRS) ETRS89 and
#' WGS84 are practically the same within the UK, but this shouldn't be a
#' problem for most civilian use of GPS satellites. If a high-precision
#' transformation between WGS84 and ETRS89 is required then it is recommended
#' to use a different package to do the conversion.
#'
#' @encoding UTF-8
#' @section Object constructors:
#' \itemize{
#' \item\code{\link{sgo_points}}: 2D/3D point coordinates
#' }
#'
#' @section Transformation and Conversion functions:
#' Functions to provide coordinate transformations:
#' \itemize{
#' \item\code{\link{sgo_bng_lonlat}}: British National Grid (E/N) to Lon/Lat
#' \item\code{\link{sgo_lonlat_bng}}: Lon/Lat to British National Grid (E/N)
#' \item\code{\link{sgo_bng_ngr}}: British National Grid (E/N) to National Grid
#' References
#' \item\code{\link{sgo_ngr_bng}}: National Grid References to British National
#' Grid (E/N)
#' \item\code{\link{sgo_laea_etrs}}: ETRS89-LAEA Easting/Northing to ETRS89
#' \item\code{\link{sgo_etrs_laea}}: ETRS89 to ETRS89-LAEA Easting/Northing
#' \item\code{\link{sgo_cart_lonlat}}: 3D Earth Centred Earth Fixed (ECEF)
#' Cartesian coordinates to polar coordinates
#' \item\code{\link{sgo_lonlat_cart}}: Polar coordinates to 3D ECEF
#' Cartesian coordinates
#' \item\code{\link{sgo_wgs84_en}}: WGS84 Lon/Lat to Pseudo-Mercator (E/N)
#' \item\code{\link{sgo_en_wgs84}}: Pseudo-Mercator (E/N) to WGS84 Lon/Lat
#' \item\code{\link{sgo_transform}}: Wrapper for all the transformations above
#' \item\code{\link{sgo_coordinates}}: Extract coordinates from a
#' \code{sgo_points} object
#' }
#'
#' @section Geometric measurements:
#' \itemize{
#' \item\code{\link{sgo_area}}: Calculate area from an ordered set of points
#' \item\code{\link{sgo_distance}}: Calculate distance(s) between points
#' }
#'
#' @section Disclaimer:
#' The OSTN15 transformation model is used in this package, and it is licensed
#' under the BSD Licence (http://opensource.org/licenses/bsd-license.php):\cr
#' \emph{© Copyright and database rights Ordnance Survey Limited 2016, © Crown
#' copyright and database rights Land & Property Services 2016 and/or ©
#' Ordnance Survey Ireland, 2016. All rights reserved.}
#' @docType package
#' @name sgo-package
NULL
#> NULL
