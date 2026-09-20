# Provenance of inst/extdata/br_municipalities.csv.
#
# This table resolves municipality name <-> IBGE code for the SGB functions.
# It ships ready to use, so this script is NOT needed to install or use the
# package; it is kept to document where the data came from and to allow the
# table to be refreshed when the IBGE mesh changes.
#
# Source: IBGE municipal mesh, retrieved through geobr.

library(geobr)

mun <- sf::st_drop_geometry(read_municipality(code_muni = "all", year = 2020))

municipalities <- data.frame(
  code_muni = as.character(mun$code_muni),
  name_muni = mun$name_muni,
  abbrev_state = mun$abbrev_state,
  stringsAsFactors = FALSE
)

# Accent-free upper case key used for tolerant name matching (see .norm_text()).
municipalities$name_norm <- toupper(
  iconv(municipalities$name_muni, to = "ASCII//TRANSLIT")
)

utils::write.csv(
  municipalities,
  "inst/extdata/br_municipalities.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
