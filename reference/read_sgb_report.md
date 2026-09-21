# Read a technical report from the SGB repository (RIGeo)

Downloads and extracts the package deposited for a RIGeo item: the
technical report itself, usually as PDF, and, when present, the
shapefiles that accompany it. RIGeo is the institutional repository of
the Geological Survey of Brazil, and the risk sectorisation of a
municipality is normally documented there.

## Usage

``` r
read_sgb_report(handle, dir = NULL)
```

## Arguments

- handle:

  RIGeo handle of the item, such as `"doc/17701"`. The full URL
  (`"https://rigeo.sgb.gov.br/handle/doc/17701"`) is also accepted.

- dir:

  Directory to extract into. Defaults to a subdirectory of the session's
  temporary directory.

## Value

A list with:

- item:

  The item's `uuid`, `title` and `handle`.

- bitstreams:

  A `data.frame` of every file attached to the item, with `name`,
  `uuid`, `bytes` and download `url`.

- files:

  Paths of the files extracted into `dir`.

## Dependencies

Requires the zip package, which is a suggested dependency.

## Examples

``` r
if (FALSE) { # \dontrun{
  report <- read_sgb_report("doc/17701")
  report$item$title
  report$files
} # }
```
