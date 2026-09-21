# Read the GLO-30 HAND raster for a geographic area

Returns the GLO-30 HAND (Height Above the Nearest Drainage) raster at 30
m resolution, clipped to the shape of the provided polygon(s). Data is
read remotely via Cloud Optimized GeoTIFF (COG) and only the portions
intersecting `place` are transferred over the network.

When `place` contains multiple features, they are unioned before
clipping, so the returned raster covers the full combined extent masked
to the union boundary.

## Usage

``` r
read_hand(place, crs_output = NULL)
```

## Arguments

- place:

  An `sf` object with POLYGON or MULTIPOLYGON geometry.

- crs_output:

  A CRS specification accepted by
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
  (e.g., an EPSG integer such as `31984`, a WKT string, or a `crs`
  object). If `NULL` (default), the output is returned in **WGS84
  (EPSG:4326)**, the native CRS of the GLO-30 HAND dataset. When a
  different CRS is requested, the raster is reprojected using bilinear
  resampling and an informative message is emitted.

## Value

A single-layer `SpatRaster` (terra) named `hand`, with HAND values in
metres, clipped to the boundary of `place`. The CRS is WGS84 unless
`crs_output` is set.

## Examples

``` r
if (FALSE) { # \dontrun{
  muni <- geobr::read_municipality(code_muni = 2919207, year = 2022,
                                   simplified = FALSE, showProgress = FALSE)

  # Returns in WGS84 (EPSG:4326) regardless of the CRS of `muni`
  r <- read_hand(muni)

  # Request output in UTM zone 24S (EPSG:31984)
  r_utm <- read_hand(muni, crs_output = 31984)
} # }
```
