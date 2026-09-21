# Read a geological risk product from the SGB

Downloads one of the geological risk products published by the
Geological Survey of Brazil (SGB/CPRM) as an `sf` object. See
[`sgb_products()`](https://pedreirajr.github.io/geohazards/reference/sgb_products.md)
for the catalogue of products and
[`sgb_inventory()`](https://pedreirajr.github.io/geohazards/reference/sgb_inventory.md)
for what exists in a given municipality or state before downloading it.

The area is selected by municipality, either by name (`municipality`,
best combined with `state` to disambiguate) or by IBGE code
(`code_muni`). Giving neither downloads the whole `state`, and giving
neither `state` nor a municipality downloads the whole country, which
can be large.

## Usage

``` r
read_sgb(product = "risk", municipality = NULL, state = NULL, code_muni = NULL)
```

## Arguments

- product:

  Product identifier, as listed by
  [`sgb_products()`](https://pedreirajr.github.io/geohazards/reference/sgb_products.md).
  Defaults to `"risk"` (risk sectorisation).

- municipality:

  Municipality name. Matching is case-insensitive and tolerant to
  accents, so `"Petropolis"` finds "Petrópolis". Combine with `state`
  when the name occurs in more than one state.

- state:

  Two-letter state abbreviation (e.g. `"RJ"`).

- code_muni:

  IBGE municipality code, as text or number. Takes precedence over
  `municipality` and is never ambiguous.

## Value

An `sf` object in SIRGAS 2000 (EPSG:4674), possibly with zero rows.
Field names are translated to English, as listed under *Field names*
below. Date fields, which the server encodes as epoch milliseconds, are
returned as `Date`.

## Details

The SGB mapping is not universal. A municipality with no features
returns an `sf` object with zero rows and a warning: absence of mapping
is not absence of risk.

The national occurrence layers (`"occurrence_mobile"`,
`"occurrence_pending"`) carry no municipality attribute. Asking for one
of them by municipality clips them spatially against the official IBGE
boundary, which requires the geobr package.

## Field names

The SGB publishes its attributes in Portuguese. This package exposes
them in English, and the table below is the full mapping. Fields absent
from it are returned under their original names. The only columns ever
dropped are the personal ones described below.

|  |  |  |
|----|----|----|
| SGB field | Field returned | Products |
| `cd_geocmu` | `code_muni` | all |
| `municipio`, `munic` | `name_muni` | all |
| `uf` | `abbrev_state` | all |
| `local` | `locality` | risk, risk_amazonas |
| `grau_risco` | `risk_level` | risk, risk_amazonas |
| `tipolo_g1` | `process_type` | risk, risk_amazonas |
| `tipolo_e1` | `process_subtype` | risk, risk_amazonas |
| `cobrade_01` | `cobrade` | risk, risk_amazonas |
| `data_setor` | `survey_date` | risk, risk_amazonas |
| `num_pess` | `n_people` | risk, risk_amazonas |
| `num_edif` | `n_buildings` | risk, risk_amazonas |
| `num_domi` | `n_households` | risk, risk_amazonas |
| `classe` | `flood_class` | flood |
| `processo` | `process` | flood |
| `ano` | `year` | flood |
| `evento` | `event` | occurrence_mobile, occurrence_pending |
| `evento_data` | `event_date` | occurrence_mobile, occurrence_pending |
| `chovia` | `was_raining` | occurrence_mobile, occurrence_pending |
| `qtd_imoveis_atingidos` | `n_affected_buildings` | occurrence_mobile, occurrence_pending |
| `observacoes` | `notes` | occurrence_mobile, occurrence_pending |
| `analisado` | `reviewed` | occurrence_mobile, occurrence_pending |

Attribute *values* are left exactly as the SGB publishes them, in
Portuguese: `risk_level`, for instance, takes `"Alto"` and
`"Muito alto"`. `n_people`, `n_buildings` and `n_households` are the
SGB's own per-sector estimates.

## Personal data

The occurrence layers, fed by public reports, carry the e-mail of
whoever reported the event (`email`) and the account name of whoever
created or last edited the record (`created_user`, `last_edited_user`).
These fields identify people, so `read_sgb()` removes them as each page
is downloaded and never returns them. The free-text `notes` field is
kept as published.

## See also

[`sgb_products()`](https://pedreirajr.github.io/geohazards/reference/sgb_products.md),
[`sgb_inventory()`](https://pedreirajr.github.io/geohazards/reference/sgb_inventory.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # Risk sectorisation, the default product
  read_sgb(municipality = "Angra dos Reis", state = "RJ")

  # Flood mapping for a municipality
  read_sgb("flood", municipality = "Nova Vicosa", state = "BA")

  # By IBGE code, which never needs disambiguation
  read_sgb("risk", code_muni = 3300100)

  # Every mapped risk sector in a state, keeping only the critical ones
  sectors <- read_sgb("risk", state = "RJ")
  sectors[sectors$risk_level == "Muito alto", ]
} # }
```
