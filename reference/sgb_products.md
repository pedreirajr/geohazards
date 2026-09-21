# Catalogue of SGB risk products

Lists the geological risk layers published by the Geological Survey of
Brazil (SGB/CPRM) on its GeoSGB server, with the identifier each one is
requested by in
[`read_sgb()`](https://pedreirajr.github.io/geohazards/reference/read_sgb.md).

Four layers are listed but flagged as not queryable: they exist in the
SGB holdings, yet their endpoint does not answer queries on the public
server. They are kept in the catalogue so that their absence is
documented rather than silent.

## Usage

``` r
sgb_products()
```

## Value

A `data.frame` with one row per product and the columns:

- product:

  Identifier to pass to
  [`read_sgb()`](https://pedreirajr.github.io/geohazards/reference/read_sgb.md).

- service:

  Name of the service on the SGB server, for traceability with the
  official documentation.

- theme:

  Hazard theme covered by the layer.

- type:

  `"cartography"` for mapped polygons, `"occurrence"` for reported
  disaster events.

- key:

  Attribute the layer is filtered by on the server.

- scope:

  `"municipality"` for layers that can be filtered by municipality,
  `"national"` for point layers that carry no municipality attribute and
  are clipped spatially.

- queryable:

  Whether the SGB server answers queries for the layer.

## Examples

``` r
sgb_products()
#>               product                     service          theme        type
#> 1                risk                       risco           risk cartography
#> 2       risk_amazonas                    risco_am           risk cartography
#> 3               flood                   inundacao          flood cartography
#> 4  occurrence_pending not_homolog_desastre_google     occurrence  occurrence
#> 5   occurrence_mobile         risco_mobile_google     occurrence  occurrence
#> 6              hazard                      perigo susceptibility cartography
#> 7         debris_flow            corrida_de_massa    debris flow cartography
#> 8      relief_pattern               padrao_relevo relief pattern cartography
#> 9 occurrence_approved     homolog_desastre_google     occurrence  occurrence
#>         key        scope queryable
#> 1 cd_geocmu municipality      TRUE
#> 2 cd_geocmu municipality      TRUE
#> 3 municipio municipality      TRUE
#> 4 municipio     national      TRUE
#> 5 municipio     national      TRUE
#> 6 cd_geocmu municipality     FALSE
#> 7 municipio municipality     FALSE
#> 8 municipio municipality     FALSE
#> 9 municipio     national     FALSE

# Only the products that can actually be downloaded
subset(sgb_products(), queryable)
#>              product                     service      theme        type
#> 1               risk                       risco       risk cartography
#> 2      risk_amazonas                    risco_am       risk cartography
#> 3              flood                   inundacao      flood cartography
#> 4 occurrence_pending not_homolog_desastre_google occurrence  occurrence
#> 5  occurrence_mobile         risco_mobile_google occurrence  occurrence
#>         key        scope queryable
#> 1 cd_geocmu municipality      TRUE
#> 2 cd_geocmu municipality      TRUE
#> 3 municipio municipality      TRUE
#> 4 municipio     national      TRUE
#> 5 municipio     national      TRUE
```
