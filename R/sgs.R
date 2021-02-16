#' sgs: Simple GIS (with OSGB).
#'
#' The sgs package aims to provide a set of functions that will help with
#' spatial or geographic analysis in Open Source R and derivatives. Its main
#' purpose is to perform OSGB36/ETRS89 transformations using the Ordnance
#' Survey's OSTN15 transformation model for Great Britain and the Isle of Man.
#' It also transforms GPS ellipsoid heights to orthometric (mean sea level)
#' heights on the relevant Ordnance Survey mapping datum, using the National
#' Geoid Model OSGM15.
#'
#' Most of the functions available in this package will become much less
#' accurate if used outside the coverage of OSTN, thus it is adviced to apply
#' the functions in this package on coordinates within Great Britain, the Isle
#' of Man and any areas of sea less than a few miles off shore.
#'
#' This package assumes that the Coordinate Reference Systems (CRS) ETRS89 and
#' WGS84 are the same within the UK, but this shouldn't be a problem for most
#' civilian use of GPS satellites. If a high-precision transformation between
#' WGS84 and ETRS89 is required then it is recommended to use a different
#' package to do the conversion.
#'
#' @encoding UTF-8
#' @section Object constructors:
#' \itemize{
#' \item\code{\link{sgs_points}}: 2D/3D point coordinates
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
#' \item\code{\link{sgs_laea_etrs}}: ETRS89-LAEA Easting/Northing to ETRS89
#' \item\code{\link{sgs_etrs_laea}}: ETRS89 to ETRS89-LAEA Easting/Northing
#' \item\code{\link{sgs_cart_lonlat}}: 3D Earth Centred Earth Fixed (ECEF)
#' Cartesian coordinates and polar coordinates
#' \item\code{\link{sgs_lonlat_cart}}: Polar and 3D Cartesian coordinates
#' \item\code{\link{sgs_wgs84_en}}: WGS84 Lon/Lat and Pseudo-Mercator (E/N)
#' \item\code{\link{sgs_en_wgs84}}: Pseudo-Mercator (E/N) and WGS84 Lon/Lat
#' \item\code{\link{sgs_transform}}: Wrapper for all the transformations above
#' \item\code{\link{sgs_coordinates}}: Extract coordinates from a
#' \code{sgs_points} object
#' }
#'
#' @section Geometric measurements:
#' \itemize{
#' \item\code{\link{sgs_area}}: Calculate area from an ordered set of points
#' \item\code{\link{sgs_distance}}: Calculate distance(s) between points
#' }
#'
#' @section Disclaimer:
#' The OSTN15 transformation model is used in this package, and it is licensed
#' under the BSD Licence (http://opensource.org/licenses/bsd-license.php):\cr
#' \emph{© Copyright and database rights Ordnance Survey Limited 2016, © Crown
#' copyright and database rights Land & Property Services 2016 and/or ©
#' Ordnance Survey Ireland, 2016. All rights reserved.}
#' @docType package
#' @name sgs
NULL
