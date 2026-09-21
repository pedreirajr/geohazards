# Harmonisation of the attributes returned by the SGB server.
#
# The server returns Portuguese field names and encodes dates as ArcGIS epoch
# milliseconds. The package exposes English names and real Date columns, while
# the original names stay documented in the @return section of every function
# so that users can still follow the official SGB documentation.

# Source field -> exported field. Fields absent from this dictionary are
# returned unchanged; the only columns ever dropped are the personal ones listed
# in .sgb_personal_fields().
.sgb_field_dict <- function() {
  c(
    # shared across layers
    municipio = "name_muni",
    munic = "name_muni",
    uf = "abbrev_state",
    cd_geocmu = "code_muni",
    # risk sectorisation
    grau_risco = "risk_level",
    tipolo_g1 = "process_type",
    tipolo_e1 = "process_subtype",
    cobrade_01 = "cobrade",
    data_setor = "survey_date",
    num_pess = "n_people",
    num_edif = "n_buildings",
    num_domi = "n_households",
    local = "locality",
    # flood
    classe = "flood_class",
    processo = "process",
    ano = "year",
    # disaster occurrences
    evento = "event",
    evento_data = "event_date",
    chovia = "was_raining",
    qtd_imoveis_atingidos = "n_affected_buildings",
    observacoes = "notes",
    analisado = "reviewed"
  )
}

# Fields that identify a person, and are therefore never returned. The
# occurrence layers carry the e-mail of whoever reported the event (`email`)
# and the ArcGIS account, in "name.surname" form, of whoever created or last
# edited the record (`created_user`, `last_edited_user`).
.sgb_personal_fields <- function() {
  c("email", "created_user", "last_edited_user")
}

# Drops the personal fields. Applied to every page .sgb_fetch() reads, so that
# no personal data leaves the download step, whichever function called it.
#
# @param x An `sf` object.
# @return `x` without the columns listed in .sgb_personal_fields().
.sgb_drop_personal <- function(x) {
  personal <- intersect(names(x), .sgb_personal_fields())
  if (!length(personal)) return(x)
  x[, setdiff(names(x), personal)]
}

# Renames the known source fields to their exported names.
#
# @param x An `sf` object as returned by [.sgb_fetch()].
# @return `x` with renamed columns.
.sgb_rename <- function(x) {
  dict <- .sgb_field_dict()
  current <- names(x)
  hits <- match(current, names(dict))

  for (i in which(!is.na(hits))) {
    target <- unname(dict[hits[i]])
    # Two source fields can map to the same exported name (municipio/munic);
    # keep the first and leave any later one under its original name.
    if (target %in% names(x)) next
    names(x)[i] <- target
  }

  x
}

# Converts the numeric date fields to Date.
#
# ArcGIS returns dates as epoch milliseconds, which arrive as large numerics.
# The magnitude test guards against renaming a genuine numeric column that
# happens to be named after a date.
#
# @param x An `sf` object.
# @return `x` with date columns as `Date`.
.sgb_parse_dates <- function(x) {
  for (nm in names(x)) {
    v <- x[[nm]]
    if (!is.numeric(v)) next
    if (!grepl("date|data|hora|^dt_", tolower(nm))) next
    if (!any(!is.na(v) & abs(v) > 1e11)) next
    x[[nm]] <- as.Date(as.POSIXct(v / 1000, origin = "1970-01-01", tz = "UTC"))
  }
  x
}

# Full harmonisation applied to every downloaded layer.
.sgb_harmonise <- function(x) {
  if (is.null(x) || !nrow(x)) return(x)
  .sgb_parse_dates(.sgb_rename(x))
}
