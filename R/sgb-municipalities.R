# Resolution of Brazilian municipalities: name <-> IBGE code. Every SGB query
# is keyed by one or the other, so this has to be robust to accents, case and
# apostrophes.

# Reads the embedded IBGE municipality table, once per session.
#
# @return A data.frame with code_muni, name_muni, abbrev_state and name_norm.
.load_municipalities <- function() {
  if (!is.null(.geohazards_cache$municipalities)) {
    return(.geohazards_cache$municipalities)
  }

  path <- system.file("extdata", "br_municipalities.csv", package = "geohazards")
  if (!nzchar(path) || !file.exists(path)) {
    rlang::abort("Municipality table missing from the package installation.")
  }

  table <- utils::read.csv(path, colClasses = "character", fileEncoding = "UTF-8")
  .geohazards_cache$municipalities <- table
  table
}

# Resolves a municipality to a single row of the IBGE table.
#
# @param municipality Municipality name, tolerant to accents and case.
# @param state Two-letter state abbreviation, used to disambiguate.
# @param code_muni IBGE code, as text or number.
# @return A one-row data.frame.
.resolve_muni <- function(municipality = NULL, state = NULL, code_muni = NULL) {
  table <- .load_municipalities()

  if (!is.null(code_muni)) {
    hit <- table[table$code_muni == as.character(code_muni), ]
    if (!nrow(hit)) {
      rlang::abort(sprintf("No municipality with IBGE code %s.", code_muni))
    }
    return(hit[1, ])
  }

  if (is.null(municipality)) {
    rlang::abort("Supply either `municipality` or `code_muni`.")
  }

  keep <- table$name_norm == .norm_text(municipality)
  if (!is.null(state)) keep <- keep & table$abbrev_state == toupper(state)
  hit <- table[keep, ]

  if (!nrow(hit)) {
    rlang::abort(sprintf("Municipality not found: %s.", municipality))
  }
  if (nrow(hit) > 1) {
    rlang::abort(c(
      sprintf("Municipality name is ambiguous: %s.", municipality),
      "i" = paste0(
        "Set `state` to one of: ",
        paste(hit$abbrev_state, collapse = ", "), "."
      )
    ))
  }

  hit[1, ]
}

# Builds the server-side WHERE clause for a municipality, according to how the
# layer is keyed.
#
# `cd_geocmu` layers are keyed by the IBGE code, which the server stores as
# text and therefore must be quoted. Name-keyed layers are matched with LIKE,
# replacing every non-ASCII or punctuation character with the single-character
# wildcard `_`: this makes "PETROPOLIS" match "PETRÓPOLIS" and keeps an
# apostrophe in "D'OESTE" from breaking the SQL. The uf filter guards against
# collisions between similar names in different states.
#
# @param key Either "cd_geocmu" or "municipio".
# @param muni A one-row data.frame from [.resolve_muni()].
# @return A character scalar with the WHERE clause.
.muni_where <- function(key, muni) {
  if (key == "cd_geocmu") {
    sprintf("cd_geocmu='%s'", as.character(muni$code_muni))
  } else {
    pattern <- gsub("[^A-Z0-9 -]", "_", toupper(as.character(muni$name_muni)))
    sprintf("uf='%s' AND municipio LIKE '%s'", muni$abbrev_state, pattern)
  }
}

# Official municipality boundary from IBGE, via geobr, cached per session.
# Used to clip the national layers, which carry no municipality attribute.
#
# @return An `sf` polygon, or NULL when geobr is unavailable or the download
#   fails.
.muni_polygon <- function(municipality = NULL, state = NULL, code_muni = NULL) {
  if (!requireNamespace("geobr", quietly = TRUE)) return(NULL)

  muni <- .resolve_muni(municipality, state, code_muni)
  key <- muni$code_muni
  if (!is.null(.geohazards_cache$polygons[[key]])) {
    return(.geohazards_cache$polygons[[key]])
  }

  polygon <- tryCatch(
    suppressMessages(geobr::read_municipality(
      code_muni = as.numeric(key), year = 2022, showProgress = FALSE
    )),
    error = function(e) NULL
  )
  if (is.null(polygon)) return(NULL)

  if (is.na(sf::st_crs(polygon))) polygon <- sf::st_set_crs(polygon, 4674)
  if (is.null(.geohazards_cache$polygons)) .geohazards_cache$polygons <- list()
  .geohazards_cache$polygons[[key]] <- polygon
  polygon
}

#' List Brazilian municipalities known to the SGB
#'
#' @description
#' Two catalogues in one function. With `source = "local"` (the default) it
#' searches the IBGE municipality table embedded in the package, which works
#' offline and is the way to find the `code_muni` of a municipality. With
#' `source = "server"` it asks the SGB which municipalities actually have risk
#' sectorisation mapped, which requires network access.
#'
#' @param search Optional fragment of a municipality name. Matching is
#'   case-insensitive and tolerant to accents, so `"petropolis"` finds
#'   "Petrópolis".
#' @param state Optional two-letter state abbreviation (e.g. `"RJ"`).
#' @param source `"local"` to search the embedded IBGE table, or `"server"` to
#'   list the municipalities covered by the SGB risk sectorisation.
#'
#' @return A `data.frame` with `code_muni`, `name_muni` and `abbrev_state`.
#'
#' @examples
#' # Offline search of the embedded IBGE table
#' sgb_municipalities(search = "petropolis")
#'
#' \dontrun{
#'   # Municipalities with risk sectorisation mapped by the SGB, in Bahia
#'   sgb_municipalities(state = "BA", source = "server")
#' }
#'
#' @export
sgb_municipalities <- function(search = NULL, state = NULL,
                               source = c("local", "server")) {
  source <- match.arg(source)

  table <- if (source == "local") {
    .load_municipalities()
  } else {
    .sgb_covered_municipalities()
  }

  if (!is.null(search)) {
    table <- table[grepl(.norm_text(search), table$name_norm, fixed = TRUE), ]
  }
  if (!is.null(state)) {
    table <- table[table$abbrev_state == toupper(state), ]
  }

  out <- table[, c("code_muni", "name_muni", "abbrev_state")]
  out <- out[order(out$abbrev_state, out$name_muni), ]
  rownames(out) <- NULL
  out
}

# Distinct municipalities present in the risk sectorisation layer. One request,
# no geometry: the server answers this quickly even for the whole country.
.sgb_covered_municipalities <- function() {
  body <- tryCatch(
    .gh_request(.sgb_query_url("risco")) |>
      httr2::req_url_query(
        where = "1=1",
        outFields = "uf,munic,cd_geocmu",
        returnDistinctValues = "true",
        returnGeometry = "false",
        orderByFields = "uf,munic",
        f = "json"
      ) |>
      httr2::req_perform() |>
      httr2::resp_body_json(),
    error = function(e) NULL
  )

  if (is.null(body) || !length(body$features)) {
    cli::cli_warn("The SGB server returned no municipality list.")
    return(data.frame(
      code_muni = character(), name_muni = character(),
      abbrev_state = character(), name_norm = character(),
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, lapply(body$features, function(feature) {
    data.frame(
      code_muni = as.character(.nz(feature$attributes$cd_geocmu)),
      name_muni = as.character(.nz(feature$attributes$munic)),
      abbrev_state = as.character(.nz(feature$attributes$uf)),
      stringsAsFactors = FALSE
    )
  }))
  out$name_norm <- .norm_text(out$name_muni)
  rownames(out) <- NULL
  out
}
