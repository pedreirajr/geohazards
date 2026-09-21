# HTTP helpers. The SGB publishes its geological risk cartography through an
# ArcGIS REST FeatureServer (GeoSGB) and its technical reports through a DSpace
# instance (RIGeo).

.sgb_base <- function() {
  "https://geoportal.sgb.gov.br/server/rest/services/gestaoterritorial"
}

.rigeo_base <- function() {
  "https://rigeo.sgb.gov.br"
}

# Identifies the package to the remote servers, as good citizenship requires.
.gh_user_agent <- function() {
  sprintf(
    "geohazards/%s (https://github.com/pedreirajr/geohazards)",
    utils::packageVersion("geohazards")
  )
}

# Base request shared by every remote call: user agent, retries and timeout.
#
# @param url Endpoint URL.
# @param timeout Timeout in seconds.
# @return An `httr2_request`.
.gh_request <- function(url, timeout = 40) {
  httr2::request(url) |>
    httr2::req_user_agent(.gh_user_agent()) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_timeout(timeout)
}

# An empty result with the shape callers expect, so that a municipality without
# mapping does not break a pipeline. Absence of mapping is not absence of risk.
#
# @return An `sf` object with zero rows, in SIRGAS 2000 (EPSG:4674).
.sgb_empty_sf <- function() {
  sf::st_sf(geometry = sf::st_sfc(crs = 4674))
}

# Query URL of a FeatureServer layer.
.sgb_query_url <- function(service) {
  paste0(.sgb_base(), "/", service, "/FeatureServer/0/query")
}

# Reads a GeoJSON response body into an `sf` object. Returns NULL when the
# payload holds no features, which is how the server reports an empty page.
.sgb_read_geojson <- function(resp) {
  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(httr2::resp_body_string(resp), tmp)
  tryCatch(sf::st_read(tmp, quiet = TRUE), error = function(e) NULL)
}

# Queries a FeatureServer layer, paging through the results.
#
# @param service Service name on the SGB server (e.g. "risco").
# @param where SQL WHERE clause applied server-side.
# @param n Maximum number of features to retrieve.
# @param timeout Timeout in seconds for each page.
# @param bbox Optional `bbox` (from [sf::st_bbox()]) used as a spatial envelope
#   filter, for layers that carry no municipality attribute.
# @return An `sf` object in SIRGAS 2000 (EPSG:4674), possibly with zero rows.
.sgb_fetch <- function(service, where = "1=1", n = Inf, timeout = 40, bbox = NULL) {
  url <- .sgb_query_url(service)
  page <- 2000L
  parts <- list()
  offset <- 0L
  # The server has no reliable "last page" flag, so paging stops on a short
  # page. The guard bounds the loop should the server keep repeating itself.
  guard <- 0L

  repeat {
    guard <- guard + 1L
    if (guard > 50L) break

    query <- list(
      where = where,
      outFields = "*",
      returnGeometry = "true",
      outSR = 4674,
      resultOffset = offset,
      resultRecordCount = page,
      f = "geojson"
    )
    if (!is.null(bbox)) {
      query$geometry <- paste(bbox[c("xmin", "ymin", "xmax", "ymax")], collapse = ",")
      query$geometryType <- "esriGeometryEnvelope"
      query$inSR <- 4674
      query$spatialRel <- "esriSpatialRelIntersects"
    }

    resp <- tryCatch(
      .gh_request(url, timeout = timeout) |>
        httr2::req_url_query(!!!query) |>
        httr2::req_perform(),
      error = function(e) NULL
    )
    if (is.null(resp)) {
      cli::cli_warn("The SGB server did not respond for {.val {service}}.")
      break
    }

    part <- .sgb_read_geojson(resp)
    if (!is.null(part)) part <- .sgb_drop_personal(part)
    k <- if (is.null(part)) 0L else nrow(part)
    if (k) parts[[length(parts) + 1L]] <- part

    offset <- offset + page
    if (k < page || offset >= n) break
  }

  if (!length(parts)) return(.sgb_empty_sf())

  out <- do.call(rbind, parts)
  if (is.na(sf::st_crs(out))) out <- sf::st_set_crs(out, 4674)
  out
}

# Counts the features a query would return, without transferring geometry.
# Used by the inventory functions, where a full download would be wasteful.
#
# @param service Service name on the SGB server.
# @param where SQL WHERE clause.
# @param timeout Timeout in seconds.
# @return A list with `n` (feature count) and `status` ("ok" or "unavailable").
.sgb_count <- function(service, where, timeout = 25) {
  body <- tryCatch(
    .gh_request(.sgb_query_url(service), timeout = timeout) |>
      httr2::req_url_query(where = where, returnCountOnly = "true", f = "json") |>
      httr2::req_perform() |>
      httr2::resp_body_json(),
    error = function(e) NULL
  )

  if (is.null(body) || !is.null(body$error) || is.null(body$count)) {
    return(list(n = NA_integer_, status = "unavailable"))
  }
  list(n = body$count, status = "ok")
}
