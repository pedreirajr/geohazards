.hand_base_url <- "https://glo-30-hand.s3.us-west-2.amazonaws.com/v1/2021"

# Compute GLO-30 HAND tile URLs for a WGS84 bounding box.
# Tiles follow the Copernicus DSM 1x1 degree grid. The SW corner of each tile
# determines its name (e.g., S13/W039 covers lat -13 to -12, lon -39 to -38).
# @param bbox Numeric vector c(xmin, ymin, xmax, ymax) in WGS84.
# @return Character vector of HTTPS URLs.
.hand_tile_urls <- function(bbox) {
  xmin <- bbox[[1]]; ymin <- bbox[[2]]; xmax <- bbox[[3]]; ymax <- bbox[[4]]

  lons <- seq(floor(xmin), floor(xmax))
  lats <- seq(floor(ymin), floor(ymax))

  grid <- expand.grid(lat = lats, lon = lons)

  lat_str <- ifelse(
    grid$lat >= 0,
    sprintf("N%02d", grid$lat),
    sprintf("S%02d", abs(grid$lat))
  )
  lon_str <- ifelse(
    grid$lon >= 0,
    sprintf("E%03d", grid$lon),
    sprintf("W%03d", abs(grid$lon))
  )

  tile_id <- sprintf("Copernicus_DSM_COG_10_%s_00_%s_00_HAND", lat_str, lon_str)
  paste0(.hand_base_url, "/", tile_id, ".tif")
}

#' Build a virtual raster (VRT) from remote COG URLs via /vsicurl/
#' @noRd
.hand_build_vrt <- function(urls) {
  terra::vrt(paste0("/vsicurl/", urls))
}

#' Crop and mask a raster to an sfc polygon
#' @noRd
.hand_clip <- function(vrt, place_union) {
  v <- terra::vect(place_union)
  terra::mask(terra::crop(vrt, v), v)
}

#' Get GLO-30 HAND raster for a geographic area
#'
#' @description
#' Returns the GLO-30 HAND (Height Above the Nearest Drainage) raster at
#' 30 m resolution, clipped to the shape of the provided polygon(s). Data is
#' read remotely via Cloud Optimized GeoTIFF (COG) and only the portions
#' intersecting `place` are transferred over the network.
#'
#' When `place` contains multiple features, they are unioned before clipping,
#' so the returned raster covers the full combined extent
#' masked to the union boundary.
#'
#' @param place An `sf` object with POLYGON or MULTIPOLYGON geometry.
#' @param crs_output A CRS specification accepted by [sf::st_crs()] (e.g.,
#'   an EPSG integer such as `31984`, a WKT string, or a `crs` object).
#'   If `NULL` (default), the output is returned in **WGS84 (EPSG:4326)**,
#'   the native CRS of the GLO-30 HAND dataset. When a different CRS is
#'   requested, the raster is reprojected using bilinear resampling and an
#'   informative message is emitted.
#'
#' @return A `SpatRaster` (terra) with HAND values in metres, clipped to
#'   the boundary of `place`. The CRS is WGS84 unless `crs_output` is set.
#'
#' @examples
#' \donttest{
#'   muni <- geobr::read_municipality(code_muni = 2929057, year = 2022,
#'                                    simplified = FALSE, showProgress = FALSE)
#'
#'   # Returns in WGS84 (EPSG:4326) regardless of the CRS of `muni`
#'   r <- get_hand(muni)
#'
#'   # Request output in UTM zone 24S (EPSG:31984)
#'   r_utm <- get_hand(muni, crs_output = 31984)
#' }
#'
#' @export
get_hand <- function(place, crs_output = NULL) {
  # --- input validation ---
  if (!inherits(place, "sf")) {
    rlang::abort("`place` must be an `sf` object.")
  }

  if (is.na(sf::st_crs(place))) {
    rlang::abort("`place` must have a defined coordinate reference system (CRS).")
  }

  geom_types <- unique(as.character(sf::st_geometry_type(place)))
  if (!all(geom_types %in% c("POLYGON", "MULTIPOLYGON"))) {
    rlang::abort("`place` must contain only POLYGON or MULTIPOLYGON geometries.")
  }

  # --- resolve output CRS (default: WGS84, native CRS of the dataset) ---
  if (is.null(crs_output)) {
    crs_out <- sf::st_crs(4326)
  } else {
    crs_out <- tryCatch(sf::st_crs(crs_output), error = function(e) NA)
    if (anyNA(crs_out)) {
      rlang::abort("`crs_output` is not a valid CRS specification.")
    }
  }

  # --- prepare geometry in WGS84 for tile discovery and clipping ---
  place_wgs84 <- sf::st_transform(place, 4326)
  place_union <- sf::st_union(place_wgs84)
  bbox <- as.numeric(sf::st_bbox(place_union))

  # --- discover tiles ---
  urls <- .hand_tile_urls(bbox)
  cli::cli_inform(c("i" = "Fetching HAND from {length(urls)} tile{?s}."))

  # --- build VRT and clip ---
  vrt <- .hand_build_vrt(urls)
  r <- .hand_clip(vrt, place_union)

  # --- reproject only when explicitly requested ---
  if (!terra::same.crs(terra::crs(r), crs_out$wkt)) {
    cli::cli_inform(
      c("i" = "Reprojecting HAND raster to {.field {crs_out$input}}.")
    )
    r <- terra::project(r, crs_out$wkt)
  }

  r
}
