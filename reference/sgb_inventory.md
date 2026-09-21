# Inventory of SGB products

Reports what the Geological Survey of Brazil has mapped, before any
geometry is downloaded. Three levels of detail are available through
`by`:

- `"municipality"` — which products exist for one or more
  municipalities, with the number of features of each.

- `"state"` — which municipalities of a state (or of the whole country,
  if `state` is `NULL`) have each product.

- `"country"` — one row per municipality in the country, with one
  logical column per product, for a coverage overview.

## Usage

``` r
sgb_inventory(
  municipality = NULL,
  state = NULL,
  code_muni = NULL,
  by = c("municipality", "state", "country"),
  products = NULL,
  complete = FALSE
)
```

## Arguments

- municipality:

  Municipality name, or a character vector of names, when
  `by = "municipality"`.

- state:

  Two-letter state abbreviation (e.g. `"RJ"`).

- code_muni:

  IBGE municipality code, as text or number. Takes precedence over
  `municipality` and is never ambiguous.

- by:

  Level of detail: `"municipality"` (default), `"state"` or `"country"`.

- products:

  Optional character vector restricting the products that are
  inventoried. Defaults to every queryable product.

- complete:

  When `by = "municipality"`, whether to include products with zero
  features. Defaults to `FALSE`.

## Value

A `data.frame`, whose columns depend on `by`:

- `"municipality"`:

  `name_muni`, `abbrev_state`, `theme`, `product`, `type`, `n_features`.

- `"state"`:

  `product`, `theme`, `abbrev_state`, `name_muni`, `code_muni`.

- `"country"`:

  `code_muni`, `name_muni`, `abbrev_state`, one logical column per
  product, and `n_products`.

## Details

Only the queryable layers are inventoried; see
[`sgb_products()`](https://pedreirajr.github.io/geohazards/reference/sgb_products.md).
The national occurrence layers carry no municipality attribute and
therefore never appear in the inventory, even though they can be
downloaded with
[`read_sgb()`](https://pedreirajr.github.io/geohazards/reference/read_sgb.md).

## See also

[`sgb_products()`](https://pedreirajr.github.io/geohazards/reference/sgb_products.md),
[`read_sgb()`](https://pedreirajr.github.io/geohazards/reference/read_sgb.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # What exists for one municipality
  sgb_inventory("Angra dos Reis", state = "RJ")

  # ... and for several at once
  sgb_inventory(c("Angra dos Reis", "Petrópolis"), state = "RJ")

  # Every municipality of Bahia with mapping
  sgb_inventory(state = "BA", by = "state")

  # National coverage: municipalities with both risk and flood mapping
  coverage <- sgb_inventory(by = "country")
  subset(coverage, risk & flood)
} # }
```
