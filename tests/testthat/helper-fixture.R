# Generates a small synthetic HAND raster in memory for offline tests.
# Extent: lon [-38.6, -38.5], lat [-12.0, -11.9], 10x10 pixels, WGS84.
.hand_fixture_raster <- function() {
  r <- terra::rast(
    xmin = -38.6, xmax = -38.5,
    ymin = -12.0, ymax = -11.9,
    nrows = 10, ncols = 10,
    crs = "EPSG:4326"
  )
  terra::values(r) <- seq(0, 20, length.out = terra::ncell(r))
  names(r) <- "GLO-30-HAND"
  r
}

# A polygon that fits well inside the fixture raster extent.
.hand_fixture_place <- function() {
  sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(matrix(
      c(-38.58, -11.98,
        -38.52, -11.98,
        -38.52, -11.92,
        -38.58, -11.92,
        -38.58, -11.98),
      ncol = 2, byrow = TRUE
    ))),
    crs = 4326
  ))
}

# Drop-in replacement for .hand_build_vrt() that returns the fixture raster.
mock_hand_build_vrt <- function(urls) {
  .hand_fixture_raster()
}
