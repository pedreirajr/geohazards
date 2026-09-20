# --- Argument handling (no network) -------------------------------------------

test_that("sgb_inventory requires a municipality when by = 'municipality'", {
  expect_error(sgb_inventory(), "needs a `municipality` or a `code_muni`")
})

test_that("sgb_inventory aborts on an unknown product", {
  expect_error(
    sgb_inventory(code_muni = 3300100, products = "volcano"),
    "Unknown product"
  )
})

test_that(".sgb_inventory_layers keeps only queryable municipal layers", {
  layers <- geohazards:::.sgb_inventory_layers()

  expect_true(all(layers$queryable))
  expect_true(all(layers$scope == "municipality"))
  expect_false("occurrence_mobile" %in% layers$product)
})

# --- Inventory by municipality (offline, .sgb_count mocked) -------------------

test_that("sgb_inventory reports the products a municipality has", {
  result <- with_mocked_bindings(
    sgb_inventory(code_muni = 3300100),
    .sgb_count = mock_sgb_count,
    .package = "geohazards"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(
    names(result),
    c("name_muni", "abbrev_state", "theme", "product", "type", "n_features")
  )
  # Only risk has features in the mock; flood returns zero and is left out
  expect_equal(result$product, "risk")
  expect_equal(result$n_features, 12L)
})

test_that("sgb_inventory includes empty products when complete = TRUE", {
  result <- with_mocked_bindings(
    sgb_inventory(code_muni = 3300100, complete = TRUE),
    .sgb_count = mock_sgb_count,
    .package = "geohazards"
  )

  expect_true("flood" %in% result$product)
  expect_gt(nrow(result), 1L)
})

test_that("sgb_inventory stacks the results of several municipalities", {
  result <- with_mocked_bindings(
    sgb_inventory(c("Angra dos Reis", "Petrópolis"), state = "RJ"),
    .sgb_count = mock_sgb_count,
    .package = "geohazards"
  )

  expect_equal(nrow(result), 2L)
  expect_setequal(result$name_muni, c("Angra dos Reis", "Petrópolis"))
})

test_that("sgb_inventory restricts the inventory to the requested products", {
  result <- with_mocked_bindings(
    sgb_inventory(code_muni = 3300100, products = "flood", complete = TRUE),
    .sgb_count = mock_sgb_count,
    .package = "geohazards"
  )

  expect_equal(result$product, "flood")
})

# --- Online tests (require network access) ------------------------------------

test_that("sgb_inventory finds real products for a mapped municipality", {
  skip_if_no_network_tests()

  result <- sgb_inventory("Angra dos Reis", state = "RJ")

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0L)
  expect_true(all(result$n_features > 0L))
})

test_that("sgb_inventory lists the mapped municipalities of a state", {
  skip_if_no_network_tests()

  result <- sgb_inventory(state = "BA", by = "state")

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0L)
  expect_true(all(result$abbrev_state == "BA"))
})
