# List Brazilian municipalities known to the SGB

Two catalogues in one function. With `source = "local"` (the default) it
searches the IBGE municipality table embedded in the package, which
works offline and is the way to find the `code_muni` of a municipality.
With `source = "server"` it asks the SGB which municipalities actually
have risk sectorisation mapped, which requires network access.

## Usage

``` r
sgb_municipalities(search = NULL, state = NULL, source = c("local", "server"))
```

## Arguments

- search:

  Optional fragment of a municipality name. Matching is case-insensitive
  and tolerant to accents, so `"petropolis"` finds "Petrópolis".

- state:

  Optional two-letter state abbreviation (e.g. `"RJ"`).

- source:

  `"local"` to search the embedded IBGE table, or `"server"` to list the
  municipalities covered by the SGB risk sectorisation.

## Value

A `data.frame` with `code_muni`, `name_muni` and `abbrev_state`.

## Examples

``` r
# Offline search of the embedded IBGE table
sgb_municipalities(search = "petropolis")
#>   code_muni       name_muni abbrev_state
#> 1   3303906      Petrópolis           RJ
#> 2   4313201 Nova Petrópolis           RS

if (FALSE) { # \dontrun{
  # Municipalities with risk sectorisation mapped by the SGB, in Bahia
  sgb_municipalities(state = "BA", source = "server")
} # }
```
