# Pre-render the pkgdown articles.
#
# The articles read data over the network (GLO-30 HAND tiles, the SGB servers),
# which neither the pkgdown workflow nor a CRAN check should depend on. The
# editable sources are therefore the `.Rmd.orig` files; this script executes
# them and writes `.Rmd` files that already carry every output, so pkgdown only
# has to convert them to HTML.
#
# Run from the package root, as a script:
#   Rscript vignettes/articles/_build.R
# (`source()`-ing it from `Rscript -e` exits after `load_all()` without
# rendering anything.)

devtools::load_all(quiet = TRUE)

owd <- setwd("vignettes/articles")
on.exit(setwd(owd), add = TRUE)

# Tell knitr the output target is HTML, so htmlwidgets (mapview) are embedded
# as HTML instead of being screenshotted to PNG through webshot.
knitr::opts_knit$set(rmarkdown.pandoc.to = "html")

# The chunk that makes pkgdown attach the htmlwidget JS/CSS. The pre-rendered
# `.Rmd` contains the widgets as raw HTML, so without a chunk that actually
# builds a widget pkgdown never learns it needs leaflet, and the maps render
# as blank boxes.
dependency_chunk <- c(
  "",
  "```{r include = FALSE}",
  "# Hidden chunk that makes pkgdown include the htmlwidget dependencies",
  "library(mapview)",
  "mapview::mapview()",
  "```",
  ""
)

sources <- list.files(pattern = "\\.Rmd\\.orig$")
for (source in sources) {
  output <- sub("\\.orig$", "", source)
  message("Knitting ", source, " -> ", output)
  knitr::knit(source, output, quiet = TRUE, envir = new.env())

  if (any(grepl("mapview(", readLines(source), fixed = TRUE))) {
    lines <- readLines(output)
    yaml_end <- which(lines == "---")[2]
    lines <- c(lines[seq_len(yaml_end)], dependency_chunk,
               lines[-seq_len(yaml_end)])
    writeLines(lines, output)
    message("  Injected the htmlwidget dependency chunk into ", output)
  }
}
