# The default test suite runs entirely offline, against fixtures: the remote
# servers this package talks to (the SGB FeatureServer in particular) are slow
# enough that hitting them on every run makes the suite unusable.
#
# The tests that do hit the network are kept — they are the only thing that
# catches a change in a remote schema — but they run only when explicitly
# asked for:
#
#   Sys.setenv(GEOHAZARDS_TEST_NETWORK = "true"); devtools::test()
#
# or, from the shell:
#
#   GEOHAZARDS_TEST_NETWORK=true Rscript -e 'devtools::test()'
skip_if_no_network_tests <- function() {
  testthat::skip_on_cran()

  if (!isTRUE(as.logical(Sys.getenv("GEOHAZARDS_TEST_NETWORK", "false")))) {
    testthat::skip("Network tests disabled; set GEOHAZARDS_TEST_NETWORK=true.")
  }

  testthat::skip_if_offline()
}
