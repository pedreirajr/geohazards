# Mapping geological risk with the SGB

The Geological Survey of Brazil (SGB/CPRM) maps, municipality by
municipality, the sectors where people live under geological risk
(landslides, debris flows, flash floods). `geohazards` reads that
cartography directly into R.

This article goes through a full workflow, finding out what exists,
downloading it, summarising the exposure, mapping it interactively and
exporting it.

``` r

library(geohazards)
library(dplyr)
```

## What exists before you download it

The SGB mapping is not universal, and the server can be slow enough that
it pays to ask before downloading.
[`sgb_inventory()`](https://pedreirajr.github.io/geohazards/reference/sgb_inventory.md)
counts features without transferring any geometry.

``` r

# The catalogue of products and the identifier each one is requested by
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

# What the SGB holds for Angra dos Reis-RJ municipality
sgb_inventory("Angra dos Reis", state = "RJ")
#>        name_muni abbrev_state theme product        type n_features
#> 1 Angra dos Reis           RJ  risk    risk cartography         75
#> 2 Angra dos Reis           RJ flood   flood cartography        198

# ... or for a whole state, one row per product and municipality
rj <- sgb_inventory(state = "RJ", by = "state")

rj |>
  count(product, name = "municipalities")
#>   product municipalities
#> 1   flood             91
#> 2    risk              5

rj |>
  filter(product == "risk")
#>   product theme abbrev_state            name_muni code_muni
#> 1    risk  risk           RJ       ANGRA DOS REIS   3300100
#> 2    risk  risk           RJ            CANTAGALO   3301108
#> 3    risk  risk           RJ        NOVA FRIBURGO   3303401
#> 4    risk  risk           RJ SANTA MARIA MADALENA   3304607
#> 5    risk  risk           RJ            SUMIDOURO   3305703
```

If a municipality name is ambiguous, `state` disambiguates it. The IBGE
code is never ambiguous, and
[`sgb_municipalities()`](https://pedreirajr.github.io/geohazards/reference/sgb_municipalities.md)
finds it offline:

``` r

sgb_municipalities(search = "angra")
#>   code_muni      name_muni abbrev_state
#> 1   3300100 Angra dos Reis           RJ
```

## Reading the risk sectorisation

``` r

angra <- read_sgb("risk", municipality = "Angra dos Reis", state = "RJ")

nrow(angra)
#> [1] 75
names(angra)
#>  [1] "objectid"                           "abbrev_state"                      
#>  [3] "name_muni"                          "code_muni"                         
#>  [5] "num_setor"                          "survey_date"                       
#>  [7] "locality"                           "process_type"                      
#>  [9] "process_subtype"                    "cobrade"                           
#> [11] "tipolo_g2"                          "tipolo_e2"                         
#> [13] "cobrade_02"                         "tipolo_g3"                         
#> [15] "tipolo_e3"                          "cobrade_03"                        
#> [17] "tipolo_g4"                          "tipolo_e4"                         
#> [19] "cobrade_04"                         "tipolo_g5"                         
#> [21] "tipolo_e5"                          "cobrade_05"                        
#> [23] "descricao"                          "obs_ocup"                          
#> [25] "grau_vulne"                         "risk_level"                        
#> [27] "n_buildings"                        "n_households"                      
#> [29] "n_people"                           "sug_interv"                        
#> [31] "orgao_exec"                         "Shape__Length"                     
#> [33] "Shape__Area"                        "gestao_territorial.risco.risco.fid"
#> [35] "geometry"
sf::st_crs(angra)$epsg  # 4674, SIRGAS 2000
#> [1] 4674
```

Important: Field names come back in English, while the values remain as
the SGB publishes them. The mapping from the original SGB field names is
documented in
[`?read_sgb`](https://pedreirajr.github.io/geohazards/reference/read_sgb.md).

## Exposure in risk sectors

`n_people`, `n_buildings` and `n_households` are the SGB’s own
per-sector estimates. Summarising them takes a short **dplyr** pipeline,
so the package does not wrap it:

``` r

exposure <- sf::st_drop_geometry(angra)

# Total exposure
exposure |>
  summarise(across(c(n_people, n_buildings, n_households),
                   \(x) sum(x, na.rm = TRUE)))
#>   n_people n_buildings n_households
#> 1    44848       11211        16168

# Broken down by risk level
exposure |>
  summarise(
    sectors = n(),
    across(c(n_people, n_buildings, n_households), \(x) sum(x, na.rm = TRUE)),
    .by = risk_level
  )
#>   risk_level sectors n_people n_buildings n_households
#> 1       Alto      44    31944        7986        10742
#> 2 Muito alto      31    12904        3225         5426
```

The same works per process type, which is often the more actionable cut:

``` r

exposure |>
  summarise(sectors = n(), n_people = sum(n_people, na.rm = TRUE),
            .by = c(process_type, risk_level)) |>
  arrange(desc(n_people))
#>       process_type risk_level sectors n_people
#> 1     Deslizamento       Alto      38    22424
#> 2     Deslizamento Muito alto      26    11144
#> 3 Corrida de massa       Alto       4     6920
#> 4          Rastejo       Alto       1     2104
#> 5        Enxurrada Muito alto       2     1060
#> 6            Queda       Alto       1      496
#> 7            Queda Muito alto       2      420
#> 8 Corrida de massa Muito alto       1      280
```

## An interactive map

`geohazards` does not depend on a mapping package: the sectors are a
plain `sf` object, so any of them will do. The examples below use
**mapview**.

### A readable popup

By default, clicking a feature shows every field of the layer, a few
dozen of them, many of them administrative metadata. Selecting the
fields that matter and labelling them makes the map far easier to read,
so build the popup first:

``` r

library(mapview)
library(leafpop)

fields <- c("locality", "process_type", "process_subtype", "risk_level",
            "survey_date", "n_people")
labels <- c("Locality", "Process", "Specific type", "Risk level",
            "Surveyed", "People")

popup <- angra |>
  select(all_of(setNames(fields, labels))) |>
  popupTable(feature.id = FALSE, row.numbers = FALSE)
```

### Colouring by risk level

Colouring the sectors by risk level over satellite imagery then takes a
single call:

``` r

mapview(
  angra,
  zcol = "risk_level",
  col.regions = c("Alto" = "#F59E0B", "Muito alto" = "#DC2626"),
  map.types = c("Esri.WorldImagery", "OpenStreetMap", "CartoDB.Positron"),
  layer.name = "Risk sectors",
  popup = popup
)
```

### Colouring by a combination of fields

To colour by risk level *and* process type, build the combined category
first. The popup still applies, because the rows of `angra` stay in the
same order:

``` r

angra <- angra |>
  mutate(risk_process = paste(risk_level, process_type, sep = " | "))

mapview(
  angra,
  zcol = "risk_process",
  map.types = "Esri.WorldImagery",
  layer.name = "Risk level | process",
  popup = popup
)
```

## Exporting

We can simply use
[`sf::st_write()`](https://r-spatial.github.io/sf/reference/st_write.html)
that handles every format.

``` r

# GeoPackage preserves long field names and accents
sf::st_write(angra, "angra_risk.gpkg", delete_dsn = TRUE)

# KML expects WGS84
sf::st_write(sf::st_transform(angra, 4326), "angra_risk.kml", delete_dsn = TRUE)
```

Avoid shapefile if you can: it truncates field names to ten characters,
which turns `n_affected_buildings` into something unreadable.

## Technical reports

The sectorisation of a municipality is usually documented in a technical
report, deposited in RIGeo, the SGB’s institutional repository. Given
its handle,
[`read_sgb_report()`](https://pedreirajr.github.io/geohazards/reference/read_sgb_report.md)
downloads and extracts the whole package:

``` r

report <- read_sgb_report("doc/17701")

report$item$title
#> [1] "Ação emergencial para delimitação de áreas em alto e muito alto risco a enchentes, Inundações e movimentos de massa: Acrelândia, AC"
report$bitstreams[c("name", "bytes")]
#>                               name  bytes
#> 1 produtos_acrelandia_ac_risco.zip 531553
```

The example handle is the report for Acrelândia (AC); any other RIGeo
item works the same way.

## Source

Geological Survey of Brazil (SGB/CPRM) — GeoSGB and RIGeo. The data is
public; please cite the source when using it. Municipality boundaries
and codes come from the IBGE mesh.
