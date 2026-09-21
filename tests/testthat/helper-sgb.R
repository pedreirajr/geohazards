# Fixtures mimicking what the SGB FeatureServer returns, before harmonisation:
# Portuguese field names, dates as ArcGIS epoch milliseconds, CRS 4674.

.sgb_fixture_polygon <- function() {
  sf::st_polygon(list(matrix(
    c(
      -44.32, -23.01,
      -44.30, -23.01,
      -44.30, -22.99,
      -44.32, -22.99,
      -44.32, -23.01
    ),
    ncol = 2, byrow = TRUE
  )))
}

# Risk sectorisation of a municipality: two sectors, one of each risk level.
.sgb_fixture_risk <- function() {
  geometry <- sf::st_sfc(
    .sgb_fixture_polygon(), .sgb_fixture_polygon(),
    crs = 4674
  )
  sf::st_sf(
    cd_geocmu = c("3300100", "3300100"),
    municipio = c("Angra dos Reis", "Angra dos Reis"),
    uf = c("RJ", "RJ"),
    grau_risco = c("Alto", "Muito alto"),
    tipolo_g1 = c("Deslizamento", "Enxurrada"),
    # 2017-07-14, as the server encodes it
    data_setor = c(1500000000000, 1500000000000),
    num_pess = c(120, 80),
    num_edif = c(40, 25),
    sug_interv = c("Drenagem", "Remocao"),
    geometry = geometry
  )
}

# Citizen-reported occurrences: two points carrying the personal fields the
# package must never return. The values are made up.
.sgb_fixture_occurrence <- function() {
  sf::st_sf(
    email = c("someone@example.com", "another@example.com"),
    evento = c("Deslizamento", "Alagamento"),
    uf = c("RJ", "RJ"),
    municipio = c("Angra dos Reis", "Angra dos Reis"),
    observacoes = c("Muro cedeu", "Rua alagada"),
    created_user = c(NA_character_, NA_character_),
    last_edited_user = c("analyst.one", "analyst.two"),
    geometry = sf::st_sfc(
      sf::st_point(c(-44.31, -23.00)), sf::st_point(c(-44.30, -23.00)),
      crs = 4674
    )
  )
}

# Serialises an sf fixture to GeoJSON text, as the server would send it.
.sgb_fixture_geojson <- function(x) {
  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp), add = TRUE)
  sf::st_write(x, tmp, quiet = TRUE)
  paste(readLines(tmp, warn = FALSE), collapse = "
")
}

# Drop-in replacement for .sgb_fetch() returning the fixture.
mock_sgb_fetch <- function(service, where = "1=1", n = Inf, timeout = 40,
                           bbox = NULL) {
  .sgb_fixture_risk()
}

# Drop-in replacement for .sgb_fetch() returning nothing, as happens for a
# municipality the SGB has not mapped.
mock_sgb_fetch_empty <- function(service, where = "1=1", n = Inf, timeout = 40,
                                 bbox = NULL) {
  .sgb_empty_sf()
}

# Drop-in replacement for .sgb_count(): risk has features, flood does not.
mock_sgb_count <- function(service, where, timeout = 25) {
  switch(
    service,
    risco = list(n = 12L, status = "ok"),
    inundacao = list(n = 0L, status = "ok"),
    list(n = NA_integer_, status = "unavailable")
  )
}
