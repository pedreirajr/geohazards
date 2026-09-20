# Access to the geological risk cartography of the Geological Survey of Brazil
# (SGB/CPRM), served as ArcGIS REST FeatureServer layers on GeoSGB.

#' Read a geological risk product from the SGB
#'
#' @description
#' Downloads one of the geological risk products published by the Geological
#' Survey of Brazil (SGB/CPRM) as an `sf` object. See [sgb_products()] for the
#' catalogue of products and [sgb_inventory()] for what exists in a given
#' municipality or state before downloading it.
#'
#' The area is selected by municipality, either by name (`municipality`, best
#' combined with `state` to disambiguate) or by IBGE code (`code_muni`). Giving
#' neither downloads the whole `state`, and giving neither `state` nor a
#' municipality downloads the whole country, which can be large.
#'
#' @details
#' The SGB mapping is not universal. A municipality with no features returns an
#' `sf` object with zero rows and a warning: absence of mapping is not absence
#' of risk.
#'
#' The national occurrence layers (`"occurrence_mobile"`,
#' `"occurrence_pending"`) carry no municipality attribute. Asking for one of
#' them by municipality clips them spatially against the official IBGE
#' boundary, which requires the \pkg{geobr} package.
#'
#' @param product Product identifier, as listed by [sgb_products()]. Defaults
#'   to `"risk"` (risk sectorisation).
#' @param municipality Municipality name. Matching is case-insensitive and
#'   tolerant to accents, so `"Petropolis"` finds "Petrópolis". Combine with
#'   `state` when the name occurs in more than one state.
#' @param state Two-letter state abbreviation (e.g. `"RJ"`).
#' @param code_muni IBGE municipality code, as text or number. Takes precedence
#'   over `municipality` and is never ambiguous.
#'
#' @return An `sf` object in SIRGAS 2000 (EPSG:4674), possibly with zero rows.
#'
#'   Field names are translated to English. The mapping from the original SGB
#'   field names is:
#'   \describe{
#'     \item{common}{`municipio`/`munic` -> `name_muni`, `uf` ->
#'       `abbrev_state`, `cd_geocmu` -> `code_muni`}
#'     \item{risk, risk_amazonas}{`grau_risco` -> `risk_level`, `tipolo_g1` ->
#'       `process_type`, `tipolo_e1` -> `process_subtype`, `cobrade_01` ->
#'       `cobrade`, `data_setor` -> `survey_date`, `num_pess` -> `n_people`,
#'       `num_edif` -> `n_buildings`, `num_domi` -> `n_households`, `local` ->
#'       `locality`}
#'     \item{flood}{`classe` -> `flood_class`, `processo` -> `process`, `ano`
#'       -> `year`}
#'     \item{occurrence_*}{`evento` -> `event`, `evento_data` -> `event_date`,
#'       `chovia` -> `was_raining`, `qtd_imoveis_atingidos` ->
#'       `n_affected_buildings`, `observacoes` -> `notes`, `analisado` ->
#'       `reviewed`}
#'   }
#'   Fields absent from this mapping are returned under their original names.
#'   Date fields, which the server encodes as epoch milliseconds, are returned
#'   as `Date`.
#'
#'   Attribute *values* remain as published by the SGB, in Portuguese: for
#'   example `risk_level` takes `"Alto"` and `"Muito alto"`. `n_people`,
#'   `n_buildings` and `n_households` are the SGB's own per-sector estimates.
#'   The occurrence layers include an `email` field from the original report,
#'   which is personal data and should not be redistributed.
#'
#' @seealso [sgb_products()], [sgb_inventory()], [read_sgb_risk()]
#'
#' @examples
#' \dontrun{
#'   # Flood mapping for a municipality
#'   read_sgb("flood", municipality = "Nova Viçosa", state = "BA")
#'
#'   # By IBGE code, which never needs disambiguation
#'   read_sgb("risk", code_muni = 3300100)
#'
#'   # Every mapped risk sector in a state
#'   read_sgb("risk", state = "RJ")
#' }
#'
#' @export
read_sgb <- function(product = "risk", municipality = NULL, state = NULL,
                     code_muni = NULL) {
  layer <- .sgb_layer(product)

  out <- if (layer$scope == "national") {
    .read_sgb_national(layer, municipality, state, code_muni)
  } else {
    .read_sgb_municipal(layer, municipality, state, code_muni)
  }

  if (!nrow(out)) {
    cli::cli_warn(c(
      "No {.val {product}} features returned by the SGB.",
      "i" = "The SGB mapping is not universal; absence of mapping is not
             absence of risk."
    ))
    return(out)
  }

  .sgb_harmonise(out)
}

# Layers filterable by municipality attribute.
.read_sgb_municipal <- function(layer, municipality, state, code_muni) {
  if (is.null(municipality) && is.null(code_muni)) {
    if (is.null(state)) {
      cli::cli_inform(c(
        "i" = "Downloading {.val {layer$product}} for the whole country;
               this may be large."
      ))
      return(.sgb_fetch(layer$service))
    }
    cli::cli_inform(c(
      "i" = "Downloading {.val {layer$product}} for {.val {toupper(state)}}."
    ))
    return(.sgb_fetch(layer$service, where = sprintf("uf='%s'", toupper(state))))
  }

  muni <- .resolve_muni(municipality, state, code_muni)
  .sgb_fetch(layer$service, where = .muni_where(layer$key, muni))
}

# National point layers, which carry no municipality attribute and so can only
# be narrowed down spatially.
.read_sgb_national <- function(layer, municipality, state, code_muni) {
  if (is.null(municipality) && is.null(code_muni)) {
    cli::cli_inform(c(
      "i" = "{.val {layer$product}} is a national layer; downloading all
             features."
    ))
    return(.sgb_fetch(layer$service))
  }

  polygon <- .muni_polygon(municipality, state, code_muni)
  if (is.null(polygon)) {
    cli::cli_warn(c(
      "Clipping a national layer to a municipality needs the {.pkg geobr} package.",
      "i" = "Downloading {.val {layer$product}} for the whole country instead."
    ))
    return(.sgb_fetch(layer$service))
  }

  # Envelope filter on the server keeps the transfer small; the exact clip is
  # then done locally against the boundary.
  out <- .sgb_fetch(layer$service, bbox = sf::st_bbox(polygon))
  if (!nrow(out)) return(out)

  inside <- sf::st_within(
    sf::st_transform(out, sf::st_crs(polygon)),
    polygon,
    sparse = FALSE
  )
  out[apply(inside, 1, any), ]
}

#' Read the SGB risk sectorisation of a municipality
#'
#' @description
#' Shortcut for `read_sgb("risk", ...)`, the most used product of the SGB, with
#' an optional filter by risk level.
#'
#' @inheritParams read_sgb
#' @param risk_level Optional character vector of risk levels to keep, as
#'   published by the SGB: `"Alto"`, `"Muito alto"`.
#'
#' @inherit read_sgb return
#'
#' @seealso [read_sgb()], [sgb_inventory()]
#'
#' @examples
#' \dontrun{
#'   read_sgb_risk("Angra dos Reis", state = "RJ")
#'
#'   # Only the most critical sectors
#'   read_sgb_risk("Angra dos Reis", state = "RJ", risk_level = "Muito alto")
#' }
#'
#' @export
read_sgb_risk <- function(municipality = NULL, state = NULL, code_muni = NULL,
                          risk_level = NULL) {
  out <- read_sgb(
    "risk",
    municipality = municipality, state = state, code_muni = code_muni
  )

  if (nrow(out) && !is.null(risk_level)) {
    out <- out[out$risk_level %in% risk_level, ]
  }
  out
}
