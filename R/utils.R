# Shared internal utilities.

# In-memory cache for objects that are expensive to build but cheap to keep
# around for the rest of the session (municipality lookup table, boundary
# polygons fetched from geobr).
.geohazards_cache <- new.env(parent = emptyenv())

# Returns `default` when `x` is NULL, zero-length or NA. Used to fill the gaps
# in the JSON payloads returned by the SGB server, where an absent attribute
# may come back as any of the three.
#
# @param x Value to test.
# @param default Value returned when `x` carries no information.
# @return `x`, or `default`.
.nz <- function(x, default = NA) {
  if (is.null(x) || length(x) == 0 || is.na(x[[1]])) default else x
}

# Normalises a place name for matching: trimmed, upper case, accents stripped.
# chartr() is used instead of iconv(to = "ASCII//TRANSLIT") because the
# transliteration tables are not portable across platforms (notably Windows).
#
# @param x Character vector.
# @return Character vector of the same length, upper case and unaccented.
.norm_text <- function(x) {
  x <- toupper(trimws(as.character(x)))
  chartr(
    paste0(
      "ÁÀÂÃÄÉÊÈËÍÎ",
      "ÌÏÓÔÒÕÖÚÛÙÜÇ"
    ),
    "AAAAAEEEEIIIIOOOOOUUUUC",
    x
  )
}
