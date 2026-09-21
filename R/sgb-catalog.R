# Curated catalogue of the SGB risk layers.
#
# The `queryable` column records an empirical finding, not a property the
# server advertises: four of the nine layers exist in the SGB holdings but
# their FeatureServer endpoint times out instead of answering. Asking for one
# of them fails fast, with an explanation, rather than hanging.
#
# `key` is the attribute each layer is filtered by, and it is not uniform:
# some layers carry the IBGE code (`cd_geocmu`), others only the municipality
# name (`municipio`). `scope` separates the layers that can be filtered by
# municipality at all from the national point layers, which carry no
# municipality attribute and must be clipped spatially.
.sgb_layers <- function() {
  data.frame(
    product = c(
      "risk", "risk_amazonas", "flood", "occurrence_pending",
      "occurrence_mobile", "hazard", "debris_flow", "relief_pattern",
      "occurrence_approved"
    ),
    service = c(
      "risco", "risco_am", "inundacao", "not_homolog_desastre_google",
      "risco_mobile_google", "perigo", "corrida_de_massa", "padrao_relevo",
      "homolog_desastre_google"
    ),
    theme = c(
      "risk", "risk", "flood", "occurrence", "occurrence",
      "susceptibility", "debris flow", "relief pattern", "occurrence"
    ),
    type = c(
      "cartography", "cartography", "cartography", "occurrence", "occurrence",
      "cartography", "cartography", "cartography", "occurrence"
    ),
    key = c(
      "cd_geocmu", "cd_geocmu", "municipio", "municipio", "municipio",
      "cd_geocmu", "municipio", "municipio", "municipio"
    ),
    scope = c(
      "municipality", "municipality", "municipality", "national", "national",
      "municipality", "municipality", "municipality", "national"
    ),
    queryable = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
}

# Looks a product up in the catalogue, aborting with actionable messages.
#
# @param product Product identifier, as accepted by [read_sgb()].
# @return A one-row data.frame from [.sgb_layers()].
.sgb_layer <- function(product) {
  layers <- .sgb_layers()
  layer <- layers[layers$product == product, ]

  if (!nrow(layer)) {
    rlang::abort(c(
      sprintf("Unknown product: %s.", product),
      "i" = paste0(
        "Available products: ",
        paste(layers$product[layers$queryable], collapse = ", "), "."
      ),
      "i" = "See `sgb_products()` for the full catalogue."
    ))
  }

  if (!layer$queryable) {
    rlang::abort(c(
      sprintf("The product %s cannot be queried on the SGB server.", product),
      "i" = "It exists in the SGB holdings, but the endpoint does not respond.",
      "i" = "See `sgb_products()` for the products that are queryable."
    ))
  }

  layer
}

#' Catalogue of SGB risk products
#'
#' @description
#' Lists the geological risk layers published by the Geological Survey of
#' Brazil (SGB/CPRM) on its GeoSGB server, with the identifier each one is
#' requested by in [read_sgb()].
#'
#' Four layers are listed but flagged as not queryable: they exist in the SGB
#' holdings, yet their endpoint does not answer queries on the public server.
#' They are kept in the catalogue so that their absence is documented rather
#' than silent.
#'
#' @return A `data.frame` with one row per product and the columns:
#'   \describe{
#'     \item{product}{Identifier to pass to [read_sgb()].}
#'     \item{service}{Name of the service on the SGB server, for traceability
#'       with the official documentation.}
#'     \item{theme}{Hazard theme covered by the layer.}
#'     \item{type}{`"cartography"` for mapped polygons, `"occurrence"` for
#'       reported disaster events.}
#'     \item{key}{Attribute the layer is filtered by on the server.}
#'     \item{scope}{`"municipality"` for layers that can be filtered by
#'       municipality, `"national"` for point layers that carry no
#'       municipality attribute and are clipped spatially.}
#'     \item{queryable}{Whether the SGB server answers queries for the layer.}
#'   }
#'
#' @examples
#' sgb_products()
#'
#' # Only the products that can actually be downloaded
#' subset(sgb_products(), queryable)
#'
#' @export
sgb_products <- function() {
  .sgb_layers()
}
