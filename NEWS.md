# geohazards 0.2.0

## Breaking changes

* `get_hand()` has been renamed to `read_hand()`. Functions that return data
  now consistently use the `read_*` prefix.

## New features

Access to the geological risk cartography of the Geological Survey of Brazil
(SGB/CPRM), incorporating the work of Julio Pedrassoli on the `sgbrisco`
package:

* `read_sgb()` downloads a risk product (risk sectorisation, flood mapping,
  disaster occurrences) for a municipality, a state or the whole country, as an
  `sf` object in SIRGAS 2000 (EPSG:4674).
* `sgb_products()` lists the catalogue of SGB products, including the four
  layers that exist in the SGB holdings but that the public server does not
  answer queries for.
* `sgb_inventory()` reports what is mapped for a municipality, a state or the
  whole country, without downloading geometry.
* `sgb_municipalities()` searches the embedded IBGE municipality table, or the
  municipalities the SGB has actually mapped.
* `read_sgb_report()` downloads and extracts a technical report from RIGeo, the
  institutional repository of the SGB.

Field names returned by the SGB are translated to English, and the dates the
server encodes as epoch milliseconds are returned as `Date`. The original SGB
field names are documented in each function's `Value` section.

# geohazards 0.1.0

* Initial version.
