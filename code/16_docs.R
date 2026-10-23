## Web page of the WHO data harmonisation: renders docs/who_mortality_harmonisation.Rmd to docs/index.html (self-contained,
## theme of the report) for GitHub Pages (branch main, folder docs/): Section 1 of the report, the data and how the WHO
## Mortality Database was processed, from the children it shares with the report (code/08_data.Rmd,
## code/08_who_processing.Rmd), with links to GitHub, the code lines of the data build inside the page, and an appendix with
## the R code of the data build (code/functions_pm.R, 01-04; folded, numbered, not run) and the dictionaries. Every number is
## read from the outputs at render time; no table is saved (the report alone writes output/report_tables/). Run after 08,
## and again after any change to the scripts it shows or links to (line numbers). NOTES item 112.
## Usage (project root): Rscript code/16_docs.R
## Writes docs/index.html, docs/.nojekyll, docs/dictionary.csv

source(here::here("code", "functions_pm.R"))
d <- here::here("docs")
lf <- function(f) { x <- readBin(f, "raw", file.size(f)); i <- which(x == as.raw(13) & c(x[-1], as.raw(0)) == as.raw(10))   # CRLF to LF, as in Git
  if (length(i)) writeBin(x[-i], f) }
rmarkdown::render(file.path(d, "who_mortality_harmonisation.Rmd"), output_file = "index.html", output_dir = d, envir = new.env(), quiet = TRUE)
invisible(file.create(file.path(d, ".nojekyll")))                                   # GitHub Pages: serve docs/ as is (no Jekyll)
dict_update(d, "16_docs.R", source = paste0(pm_src, "; via output/data_build/, output/sdg34/, Study/output/replication/, data/dict/, data/derived/, LICENSE ",
                                            "and the scripts it links to and shows; docs/who_mortality_harmonisation.Rmd with code/08_data.Rmd, code/08_who_processing.Rmd and code/08_helpers.R"),
  data.table(file = "index.html", variable = "(page)", unit = "HTML (self-contained)",
             description = paste("Web page (GitHub Pages: branch main, folder docs/; the empty docs/.nojekyll serves the folder as is): how the WHO Mortality Database",
                                 "was harmonised and cleaned, Section 1 of report/premature_global.html (Tables 1.1-1.8, Figure D1) from the same children, with links to",
                                 "the files of the public repository on GitHub (main branch; only the files listed in docs/published.csv) and the code lines of the",
                                 "data build inside the page; appendix: R code of the data build (code/functions_pm.R, 01-04; folded, numbered, not run) and the",
                                 "columns of data/dict/who_cause_groups.csv and data/dict/areas.csv (a CSV is embedded for download only if published)")))
invisible(lapply(file.path(d, c("index.html", "dictionary.csv")), lf))
