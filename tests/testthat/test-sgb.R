# --- Product catalogue (pure, no network) ------------------------------------

test_that("sgb_products lists the catalogue with both identifiers", {
  products <- sgb_products()

  expect_s3_class(products, "data.frame")
  expect_true(all(
    c("product", "service", "theme", "type", "key", "scope", "queryable") %in%
      names(products)
  ))
  expect_true(all(c("risk", "flood") %in% products$product))
  # Traceability with the official SGB service names is preserved
  expect_equal(products$service[products$product == "risk"], "risco")
})

test_that(".sgb_layer aborts on an unknown product", {
  expect_error(geohazards:::.sgb_layer("volcano"), "Unknown product")
})

test_that(".sgb_layer aborts on a product the server cannot answer", {
  expect_error(geohazards:::.sgb_layer("hazard"), "cannot be queried")
})

# --- Municipality resolution (offline, embedded table) ------------------------

test_that(".resolve_muni finds a municipality with and without accents", {
  with_accent <- geohazards:::.resolve_muni("Petrópolis", state = "RJ")
  without <- geohazards:::.resolve_muni("petropolis", state = "RJ")

  expect_equal(with_accent$code_muni, without$code_muni)
  expect_equal(without$abbrev_state, "RJ")
})

test_that(".resolve_muni accepts the IBGE code as text or number", {
  as_text <- geohazards:::.resolve_muni(code_muni = "3300100")
  as_number <- geohazards:::.resolve_muni(code_muni = 3300100)

  expect_equal(as_text$name_muni, as_number$name_muni)
  expect_equal(as_text$abbrev_state, "RJ")
})

test_that(".resolve_muni aborts on an ambiguous name and names the states", {
  # "Bom Jesus" exists in several states
  expect_error(geohazards:::.resolve_muni("Bom Jesus"), "ambiguous")
})

test_that(".resolve_muni aborts when the municipality does not exist", {
  expect_error(geohazards:::.resolve_muni("Atlantis", state = "RJ"), "not found")
})

test_that(".resolve_muni aborts when given nothing to resolve", {
  expect_error(geohazards:::.resolve_muni(), "`municipality` or `code_muni`")
})

# --- WHERE clause construction (the part the server is picky about) ----------

test_that(".muni_where quotes the IBGE code as text", {
  muni <- geohazards:::.resolve_muni(code_muni = "3300100")
  expect_equal(
    geohazards:::.muni_where("cd_geocmu", muni),
    "cd_geocmu='3300100'"
  )
})

test_that(".muni_where turns accents into single-character wildcards", {
  muni <- geohazards:::.resolve_muni("Petrópolis", state = "RJ")
  where <- geohazards:::.muni_where("municipio", muni)

  expect_match(where, "uf='RJ'")
  expect_match(where, "PETR_POLIS")
})

test_that(".muni_where does not break the SQL on an apostrophe", {
  muni <- geohazards:::.resolve_muni("Alta Floresta d'Oeste", state = "RO")
  where <- geohazards:::.muni_where("municipio", muni)

  # The apostrophe becomes a wildcard instead of closing the string literal
  expect_false(grepl("D'OESTE", where, fixed = TRUE))
  expect_match(where, "D_OESTE")
})

# --- Municipality search ------------------------------------------------------

test_that("sgb_municipalities searches the embedded table without network", {
  found <- sgb_municipalities(search = "petropolis")

  expect_s3_class(found, "data.frame")
  expect_equal(names(found), c("code_muni", "name_muni", "abbrev_state"))
  expect_true(any(found$abbrev_state == "RJ"))
})

test_that("sgb_municipalities filters by state", {
  found <- sgb_municipalities(search = "bom jesus", state = "RS")
  expect_true(all(found$abbrev_state == "RS"))
})

# --- Field harmonisation ------------------------------------------------------

test_that(".sgb_rename translates the known fields and keeps the rest", {
  renamed <- geohazards:::.sgb_rename(.sgb_fixture_risk())

  expect_true(all(
    c("risk_level", "process_type", "n_people", "name_muni", "code_muni") %in%
      names(renamed)
  ))
  expect_false(any(c("grau_risco", "num_pess") %in% names(renamed)))
  # Unknown fields are never dropped
  expect_true("sug_interv" %in% names(renamed))
})

# --- Personal data ------------------------------------------------------------

test_that(".sgb_drop_personal removes the fields that identify people", {
  occurrences <- .sgb_fixture_occurrence()
  cleaned <- geohazards:::.sgb_drop_personal(occurrences)

  expect_false(any(geohazards:::.sgb_personal_fields() %in% names(cleaned)))
  expect_true(all(c("evento", "observacoes") %in% names(cleaned)))
  expect_s3_class(cleaned, "sf")
  expect_equal(nrow(cleaned), nrow(occurrences))
})

test_that(".sgb_drop_personal leaves layers without personal fields alone", {
  risk <- .sgb_fixture_risk()
  expect_identical(geohazards:::.sgb_drop_personal(risk), risk)
})

test_that(".sgb_fetch never returns personal fields (offline, HTTP mocked)", {
  body <- .sgb_fixture_geojson(.sgb_fixture_occurrence())
  mock <- function(req) {
    httr2::response(
      status_code = 200L,
      headers = list("Content-Type" = "application/geo+json"),
      body = charToRaw(body)
    )
  }

  result <- httr2::with_mocked_responses(
    mock,
    geohazards:::.sgb_fetch("risco_mobile_google")
  )

  expect_equal(nrow(result), 2L)
  expect_false(any(geohazards:::.sgb_personal_fields() %in% names(result)))
  expect_true("observacoes" %in% names(result))
})

test_that(".sgb_parse_dates converts ArcGIS epoch milliseconds to Date", {
  parsed <- geohazards:::.sgb_parse_dates(
    geohazards:::.sgb_rename(.sgb_fixture_risk())
  )

  expect_s3_class(parsed$survey_date, "Date")
  expect_equal(format(parsed$survey_date[1], "%Y"), "2017")
})

test_that(".sgb_parse_dates leaves plain numeric columns alone", {
  parsed <- geohazards:::.sgb_parse_dates(
    geohazards:::.sgb_rename(.sgb_fixture_risk())
  )
  expect_type(parsed$n_people, "double")
})

# --- read_sgb (offline, .sgb_fetch mocked) ------------------------------------

test_that("read_sgb returns an sf with harmonised fields", {
  result <- with_mocked_bindings(
    read_sgb("risk", code_muni = 3300100),
    .sgb_fetch = mock_sgb_fetch,
    .package = "geohazards"
  )

  expect_s3_class(result, "sf")
  expect_equal(sf::st_crs(result)$epsg, 4674L)
  expect_true("risk_level" %in% names(result))
  expect_s3_class(result$survey_date, "Date")
})

test_that("read_sgb warns and returns an empty sf when nothing is mapped", {
  with_mocked_bindings(
    {
      expect_warning(
        result <- read_sgb("risk", code_muni = 3300100),
        "No .* features returned"
      )
      expect_s3_class(result, "sf")
      expect_equal(nrow(result), 0L)
    },
    .sgb_fetch = mock_sgb_fetch_empty,
    .package = "geohazards"
  )
})

test_that("read_sgb aborts before reaching the network on a bad product", {
  expect_error(read_sgb("volcano", code_muni = 3300100), "Unknown product")
  expect_error(read_sgb("relief_pattern", code_muni = 3300100), "cannot be queried")
})

test_that("read_sgb defaults to the risk sectorisation", {
  result <- with_mocked_bindings(
    read_sgb(code_muni = 3300100),
    .sgb_fetch = mock_sgb_fetch,
    .package = "geohazards"
  )

  expect_equal(nrow(result), 2L)
  expect_true("risk_level" %in% names(result))
})

# --- Online test (requires network access) ------------------------------------

test_that("read_sgb downloads real risk sectors from the SGB", {
  skip_if_no_network_tests()

  result <- read_sgb("risk", municipality = "Angra dos Reis", state = "RJ")

  expect_s3_class(result, "sf")
  expect_gt(nrow(result), 0L)
  expect_equal(sf::st_crs(result)$epsg, 4674L)
  expect_true("risk_level" %in% names(result))
})
