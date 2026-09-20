# Technical reports from RIGeo, the institutional repository of the SGB, which
# runs on DSpace. Reaching a file takes three steps: handle -> item UUID ->
# bitstreams (the files attached to the item).

# Resolves a RIGeo handle to its DSpace item.
#
# @param handle Handle such as "doc/17701", "/handle/doc/17701" or the full URL.
# @return A list with uuid, title and handle.
.rigeo_resolve_handle <- function(handle) {
  id <- sub(".*/handle/", "", handle)

  item <- .gh_request(.rigeo_base()) |>
    httr2::req_url_path_append("server", "api", "pid", "find") |>
    httr2::req_url_query(id = paste0("hdl:", id)) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  list(uuid = item$uuid, title = item$name, handle = id)
}

# Lists the files attached to a RIGeo item.
#
# @param uuid Item UUID, from [.rigeo_resolve_handle()].
# @return A data.frame with name, uuid, bytes and url.
.rigeo_list_bitstreams <- function(uuid) {
  bundles <- .gh_request(.rigeo_base()) |>
    httr2::req_url_path_append("server", "api", "core", "items", uuid, "bundles") |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  listed <- bundles[["_embedded"]][["bundles"]]
  if (is.null(listed)) listed <- list()

  # The deposited files live in the ORIGINAL bundle; the others hold thumbnails
  # and licence text.
  original <- Filter(function(b) identical(b$name, "ORIGINAL"), listed)
  if (!length(original)) return(data.frame())

  bitstreams <- .gh_request(.rigeo_base()) |>
    httr2::req_url_path_append(
      "server", "api", "core", "bundles", original[[1]]$uuid, "bitstreams"
    ) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  files <- bitstreams[["_embedded"]][["bitstreams"]]
  if (is.null(files)) files <- list()

  data.frame(
    name = vapply(files, function(f) as.character(.nz(f$name, NA_character_)), ""),
    uuid = vapply(files, function(f) as.character(.nz(f$uuid, NA_character_)), ""),
    bytes = vapply(files, function(f) as.numeric(.nz(f$sizeBytes, NA_real_)), numeric(1)),
    url = vapply(files, function(f) {
      paste0(.rigeo_base(), "/server/api/core/bitstreams/", f$uuid, "/content")
    }, ""),
    stringsAsFactors = FALSE
  )
}

#' Read a technical report from the SGB repository (RIGeo)
#'
#' @description
#' Downloads and extracts the package deposited for a RIGeo item: the technical
#' report itself, usually as PDF, and, when present, the shapefiles that
#' accompany it. RIGeo is the institutional repository of the Geological Survey
#' of Brazil, and the risk sectorisation of a municipality is normally
#' documented there.
#'
#' @param handle RIGeo handle of the item, such as `"doc/17701"`. The full URL
#'   (`"https://rigeo.sgb.gov.br/handle/doc/17701"`) is also accepted.
#' @param dir Directory to extract into. Defaults to a subdirectory of the
#'   session's temporary directory.
#'
#' @return A list with:
#'   \describe{
#'     \item{item}{The item's `uuid`, `title` and `handle`.}
#'     \item{bitstreams}{A `data.frame` of every file attached to the item,
#'       with `name`, `uuid`, `bytes` and download `url`.}
#'     \item{files}{Paths of the files extracted into `dir`.}
#'   }
#'
#' @section Dependencies:
#' Requires the \pkg{zip} package, which is a suggested dependency.
#'
#' @examples
#' \dontrun{
#'   report <- read_sgb_report("doc/17701")
#'   report$item$title
#'   report$files
#' }
#'
#' @export
read_sgb_report <- function(handle, dir = NULL) {
  if (!requireNamespace("zip", quietly = TRUE)) {
    rlang::abort(c(
      "The {.pkg zip} package is required to extract RIGeo packages.",
      "i" = 'Install it with `install.packages("zip")`.'
    ))
  }

  item <- .rigeo_resolve_handle(handle)
  bitstreams <- .rigeo_list_bitstreams(item$uuid)

  archives <- bitstreams[grepl("\\.zip$", bitstreams$name, ignore.case = TRUE), ,
                         drop = FALSE]
  target <- if (is.null(dir)) {
    file.path(tempdir(), paste0("rigeo_", item$uuid))
  } else {
    dir
  }
  dir.create(target, showWarnings = FALSE, recursive = TRUE)

  if (!nrow(archives)) {
    cli::cli_warn("No archive attached to RIGeo item {.val {item$handle}}.")
  }

  for (i in seq_len(nrow(archives))) {
    cli::cli_inform(c("i" = "Downloading {.val {archives$name[i]}}."))
    destination <- tempfile(fileext = ".zip")
    .gh_request(archives$url[i], timeout = 300) |>
      httr2::req_perform(path = destination)
    zip::unzip(destination, exdir = target)
    unlink(destination)
  }

  list(
    item = item,
    bitstreams = bitstreams,
    files = list.files(target, recursive = TRUE, full.names = TRUE)
  )
}
