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
# The accent table is built from code points rather than written out, so that
# the source file stays pure ASCII, as portable packages must.
#
# @param x Character vector.
# @return Character vector of the same length, upper case and unaccented.
.norm_text <- function(x) {
  x <- toupper(trimws(as.character(x)))
  accented <- intToUtf8(c(
    193, 192, 194, 195, 196,   # A
    201, 202, 200, 203,        # E
    205, 206, 204, 207,        # I
    211, 212, 210, 213, 214,   # O
    218, 219, 217, 220,        # U
    199                        # C
  ))
  chartr(accented, "AAAAAEEEEIIIIOOOOOUUUUC", x)
}
