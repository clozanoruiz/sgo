#As of today only used when converting sf/sfc to sgs_points
sf_checks <-function (x, epsg) {

  # Check if input data is a simple feature object (sf, sfc).
  # We need sf to get the coordinates of the object
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package \"sf\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (all(!sf::st_is(x, c("POINT", "MULTIPOINT"))))
    stop("Only POINT and MULTIPOINT geometries are accepted")

  if (is.null(epsg)) {
    crs.epsg <- sf::st_crs(x)$epsg
    if(!crs.epsg %in% epsgs[, "epsg"]) {
      stop("Either 'epsg' must be entered, or a crs defined for the sf object.")
    } else {
      epsg <- crs.epsg
    }
  }

  epsg

}
