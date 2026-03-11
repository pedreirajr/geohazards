# --- Input validation (no network, no mock needed) ---------------------------

test_that("get_hand aborts if place is not an sf object", {
  expect_error(get_hand("not_sf"), "`place` must be an `sf` object")
  expect_error(get_hand(list()), "`place` must be an `sf` object")
})

test_that("get_hand aborts if place has no CRS", {
  p <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))
  ))
  expect_error(get_hand(p), "must have a defined coordinate reference system")
})

test_that("get_hand aborts if place geometry is not polygon", {
  p <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(-38.5, -12.0)), crs = 4326))
  expect_error(get_hand(p), "POLYGON or MULTIPOLYGON")
})

test_that("get_hand aborts if crs_output is invalid", {
  p <- .hand_fixture_place()
  expect_error(get_hand(p, crs_output = "not_a_crs"), "`crs_output` is not a valid CRS")
})

# --- Offline tests (network call mocked via .hand_build_vrt) -----------------

test_that("get_hand returns a SpatRaster (offline)", {
  result <- with_mocked_bindings(
    get_hand(.hand_fixture_place()),
    .hand_build_vrt = mock_hand_build_vrt,
    .package = "geohazards"
  )
  expect_s4_class(result, "SpatRaster")
})

test_that("get_hand output CRS is WGS84 by default (offline)", {
  result <- with_mocked_bindings(
    get_hand(.hand_fixture_place()),
    .hand_build_vrt = mock_hand_build_vrt,
    .package = "geohazards"
  )
  expect_true(terra::same.crs(terra::crs(result), "EPSG:4326"))
})

test_that("get_hand reprojects when crs_output is set (offline)", {
  result <- with_mocked_bindings(
    get_hand(.hand_fixture_place(), crs_output = 31984),
    .hand_build_vrt = mock_hand_build_vrt,
    .package = "geohazards"
  )
  expect_true(terra::same.crs(terra::crs(result), "EPSG:31984"))
})

test_that("get_hand accepts multiple polygons and unions them (offline)", {
  p1 <- sf::st_polygon(list(matrix(
    c(-38.58, -11.98, -38.55, -11.98, -38.55, -11.95, -38.58, -11.95, -38.58, -11.98),
    ncol = 2, byrow = TRUE
  )))
  p2 <- sf::st_polygon(list(matrix(
    c(-38.55, -11.98, -38.52, -11.98, -38.52, -11.95, -38.55, -11.95, -38.55, -11.98),
    ncol = 2, byrow = TRUE
  )))
  place_multi <- sf::st_as_sf(sf::st_sfc(p1, p2, crs = 4326))

  result <- with_mocked_bindings(
    get_hand(place_multi),
    .hand_build_vrt = mock_hand_build_vrt,
    .package = "geohazards"
  )
  expect_s4_class(result, "SpatRaster")
})

test_that("get_hand accepts place in a projected CRS (offline)", {
  place_utm <- sf::st_transform(.hand_fixture_place(), 31984)

  result <- with_mocked_bindings(
    get_hand(place_utm),
    .hand_build_vrt = mock_hand_build_vrt,
    .package = "geohazards"
  )
  expect_s4_class(result, "SpatRaster")
  expect_true(terra::same.crs(terra::crs(result), "EPSG:4326"))
})

# --- Internal helper: tile URL construction (pure function, no network) -------

test_that(".hand_tile_urls returns one URL for a single-tile bbox", {
  urls <- geohazards:::.hand_tile_urls(c(-38.6, -12.0, -38.5, -11.9))
  expect_length(urls, 1L)
  expect_match(urls, "Copernicus_DSM_COG_10_S12_00_W039_00_HAND\\.tif$")
})

test_that(".hand_tile_urls returns correct count for a multi-tile bbox", {
  # 2 lons x 3 lats = 6 tiles
  urls <- geohazards:::.hand_tile_urls(c(-39.1, -13.1, -38.4, -11.9))
  expect_length(urls, 6L)
})

test_that(".hand_tile_urls handles northern hemisphere correctly", {
  urls <- geohazards:::.hand_tile_urls(c(10.5, 51.2, 11.3, 52.1))
  expect_match(urls, "N51.*E010", all = FALSE)
  expect_match(urls, "N52.*E011", all = FALSE)
})

# --- Online tests (require network access) -----------------------------------

test_that("get_hand returns a valid SpatRaster for a real municipality", {
  skip_on_cran()
  skip_if_offline()

  muni <- geobr::read_municipality(
    code_muni = 2929057, year = 2022,
    simplified = FALSE, showProgress = FALSE
  )
  result <- get_hand(muni)

  expect_s4_class(result, "SpatRaster")
  expect_true(terra::same.crs(terra::crs(result), "EPSG:4326"))
  expect_gt(terra::ncell(result), 0L)
  expect_false(all(is.na(terra::values(result))))
})
