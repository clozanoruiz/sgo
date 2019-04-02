#As of today only used when converting sf/sfc to sgs_points
sf_checks <-function (x, epsg) {

  # Check if input data is a simple feature object (sf, sfc).
  # We need sf to get the coordinates of the object
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package \"sf\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (all(!sf::st_is(x, c("POINT", "MULTIPOINT")))) stop("Only POINT and MULTIPOINT geometries are accepted")

  if (is.null(epsg)) {
    crs.epsg <- sf::st_crs(x)$epsg
    if(!crs.epsg %in% epsgs[, "epsg"]) {
      stop("The parameter epsg must be entered, or an appropiate crs defined for the sf object")
    } else {
      epsg <- crs.epsg
    }
  }

  epsg

}


# As of today: only used in sgs_polygons.data.frame
# Helper function to apply a buffer to any feature.
# d can be a scalar or a vector with the buffers to apply to each element of x
apply_buffer <- function(x, d) {

  if (length(d) == 1) {
    x <- sf::st_buffer(x, d)
  } else {
    sf::st_geometry(x) <- sf::st_sfc(mapply(sf::st_buffer, x=x$geometry,
                                            dist=d, SIMPLIFY = FALSE))
  }

  x
}
