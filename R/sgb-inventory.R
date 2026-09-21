# What the SGB holds, without downloading geometry. Every function here asks
# the server for counts or distinct values only, so they stay usable even for
# a whole state.

#' Inventory of SGB products
#'
#' @description
#' Reports what the Geological Survey of Brazil has mapped, before any geometry
#' is downloaded. Three levels of detail are available through `by`:
#'
#' * `"municipality"` — which products exist for one or more municipalities,
#'   with the number of features of each.
#' * `"state"` — which municipalities of a state (or of the whole country, if
#'   `state` is `NULL`) have each product.
#' * `"country"` — one row per municipality in the country, with one logical
#'   column per product, for a coverage overview.
#'
#' @details
#' Only the queryable layers are inventoried; see [sgb_products()]. The
#' national occurrence layers carry no municipality attribute and therefore
#' never appear in the inventory, even though they can be downloaded with
#' [read_sgb()].
#'
#' @inheritParams read_sgb
#' @param municipality Municipality name, or a character vector of names, when
#'   `by = "municipality"`.
#' @param by Level of detail: `"municipality"` (default), `"state"` or
#'   `"country"`.
#' @param products Optional character vector restricting the products that are
#'   inventoried. Defaults to every queryable product.
#' @param complete When `by = "municipality"`, whether to include products with
#'   zero features. Defaults to `FALSE`.
#'
#' @return A `data.frame`, whose columns depend on `by`:
#'   \describe{
#'     \item{`"municipality"`}{`name_muni`, `abbrev_state`, `theme`, `product`,
#'       `type`, `n_features`.}
#'     \item{`"state"`}{`product`, `theme`, `abbrev_state`, `name_muni`,
#'       `code_muni`.}
#'     \item{`"country"`}{`code_muni`, `name_muni`, `abbrev_state`, one logical
#'       column per product, and `n_products`.}
#'   }
#'
#' @seealso [sgb_products()], [read_sgb()]
#'
#' @examples
#' \dontrun{
#'   # What exists for one municipality
#'   sgb_inventory("Angra dos Reis", state = "RJ")
#'
#'   # ... and for several at once
#'   sgb_inventory(c("Angra dos Reis", "Petrópolis"), state = "RJ")
#'
#'   # Every municipality of Bahia with mapping
#'   sgb_inventory(state = "BA", by = "state")
#'
#'   # National coverage: municipalities with both risk and flood mapping
#'   coverage <- sgb_inventory(by = "country")
#'   subset(coverage, risk & flood)
#' }
#'
#' @export
sgb_inventory <- function(municipality = NULL, state = NULL, code_muni = NULL,
                          by = c("municipality", "state", "country"),
                          products = NULL, complete = FALSE) {
  by <- match.arg(by)

  switch(
    by,
    municipality = .sgb_inventory_muni(
      municipality, state, code_muni, products, complete
    ),
    state = .sgb_inventory_state(state, products),
    country = .sgb_inventory_country(products)
  )
}

# Queryable layers that can be filtered by municipality, optionally restricted
# to a set of products.
.sgb_inventory_layers <- function(products = NULL) {
  layers <- .sgb_layers()
  layers <- layers[layers$queryable & layers$scope == "municipality", ]

  if (!is.null(products)) {
    unknown <- setdiff(products, .sgb_layers()$product)
    if (length(unknown)) {
      rlang::abort(c(
        sprintf("Unknown product(s): %s.", paste(unknown, collapse = ", ")),
        "i" = "See `sgb_products()` for the catalogue."
      ))
    }
    layers <- layers[layers$product %in% products, ]
  }

  layers
}

.sgb_inventory_muni <- function(municipality, state, code_muni, products,
                                complete) {
  if (is.null(municipality) && is.null(code_muni)) {
    rlang::abort(c(
      "`by = \"municipality\"` needs a `municipality` or a `code_muni`.",
      "i" = "Use `by = \"state\"` or `by = \"country\"` for a broader inventory."
    ))
  }

  # Several municipalities at once: one inventory each, stacked.
  if (length(municipality) > 1) {
    rows <- lapply(municipality, function(one) {
      .sgb_inventory_muni(one, state, NULL, products, complete)
    })
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    return(out)
  }

  muni <- .resolve_muni(municipality, state, code_muni)
  layers <- .sgb_inventory_layers(products)

  rows <- list()
  for (i in seq_len(nrow(layers))) {
    layer <- layers[i, ]
    count <- .sgb_count(layer$service, .muni_where(layer$key, muni))
    n <- count$n
    found <- count$status == "ok" && !is.na(n) && n > 0
    if (!complete && !found) next

    rows[[length(rows) + 1L]] <- data.frame(
      name_muni = muni$name_muni,
      abbrev_state = muni$abbrev_state,
      theme = layer$theme,
      product = layer$product,
      type = layer$type,
      n_features = as.integer(n),
      stringsAsFactors = FALSE
    )
  }

  if (!length(rows)) {
    cli::cli_warn(
      "No SGB product found for {.val {muni$name_muni}}/{.val {muni$abbrev_state}}."
    )
    return(data.frame(
      name_muni = character(), abbrev_state = character(), theme = character(),
      product = character(), type = character(), n_features = integer(),
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.sgb_inventory_state <- function(state, products) {
  layers <- .sgb_inventory_layers(products)
  where <- if (is.null(state)) "1=1" else sprintf("uf='%s'", toupper(state))

  rows <- list()
  for (i in seq_len(nrow(layers))) {
    layer <- layers[i, ]
    # One distinct-values request per layer: fast even country-wide.
    fields <- if (layer$key == "cd_geocmu") "uf,municipio,cd_geocmu" else "uf,municipio"

    body <- tryCatch(
      .gh_request(.sgb_query_url(layer$service)) |>
        httr2::req_url_query(
          where = where,
          outFields = fields,
          returnDistinctValues = "true",
          returnGeometry = "false",
          orderByFields = "uf,municipio",
          f = "json"
        ) |>
        httr2::req_perform() |>
        httr2::resp_body_json(),
      error = function(e) NULL
    )

    if (is.null(body) || !length(body$features)) {
      cli::cli_warn("No response from the SGB for {.val {layer$product}}.")
      next
    }

    part <- do.call(rbind, lapply(body$features, function(feature) {
      data.frame(
        product = layer$product,
        theme = layer$theme,
        abbrev_state = as.character(.nz(feature$attributes$uf)),
        name_muni = as.character(.nz(feature$attributes$municipio)),
        code_muni = as.character(.nz(feature$attributes$cd_geocmu)),
        stringsAsFactors = FALSE
      )
    }))
    rows[[length(rows) + 1L]] <- unique(part)
  }

  if (!length(rows)) {
    return(data.frame(
      product = character(), theme = character(), abbrev_state = character(),
      name_muni = character(), code_muni = character(),
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.sgb_inventory_country <- function(products) {
  inventory <- .sgb_inventory_state(state = NULL, products = products)
  if (!nrow(inventory)) return(inventory)

  table <- .load_municipalities()

  # Layers keyed by name return no IBGE code, so it is recovered from the
  # embedded table through the accent-free name plus state.
  code <- inventory$code_muni
  missing <- is.na(code) | code %in% c("NA", "")
  key_inventory <- paste(.norm_text(inventory$name_muni), inventory$abbrev_state)
  key_table <- paste(table$name_norm, table$abbrev_state)
  code[missing] <- table$code_muni[match(key_inventory[missing], key_table)]

  inventory$code_muni <- code
  inventory <- inventory[!is.na(inventory$code_muni), ]

  found <- sort(unique(inventory$product))
  out <- data.frame(
    code_muni = sort(unique(inventory$code_muni)),
    stringsAsFactors = FALSE
  )
  out$name_muni <- table$name_muni[match(out$code_muni, table$code_muni)]
  out$abbrev_state <- table$abbrev_state[match(out$code_muni, table$code_muni)]
  for (product in found) {
    out[[product]] <- out$code_muni %in% inventory$code_muni[inventory$product == product]
  }
  out$n_products <- rowSums(out[found])

  out <- out[order(-out$n_products, out$abbrev_state, out$name_muni), ]
  rownames(out) <- NULL
  out
}
