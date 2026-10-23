## Session information: R, operating system, versions of the packages used by the project scripts (matisseR = New APC
## model; rmarkdown, knitr and kableExtra for the report; pdftools and qpdf for the manuscript files of 14) and of pandoc.
## Usage (project root): Rscript code/00_session.R
## Writes output/session_info.txt, output/dictionary.csv

source(here::here("code", "functions_pm.R"))
pk <- c("data.table", "here", "countrycode", "readxl", "ggplot2", "patchwork", "matisseR", "rmarkdown", "knitr", "kableExtra", "pdftools", "qpdf")
si <- sessionInfo()
writeLines(c(sprintf("Written by code/00_session.R on %s", Sys.Date()), R.version.string, sprintf("Platform: %s", R.version$platform),
             sprintf("OS: %s", si$running), sprintf("Locale (collate): %s", Sys.getlocale("LC_COLLATE")), "Packages:",
             sprintf("  %s %s", pk, vapply(pk, function(p) as.character(packageVersion(p)), "")), sprintf("Pandoc: %s", rmarkdown::pandoc_version())),
           here::here("output", "session_info.txt"))
dict_update(here::here("output"), "00_session.R", source = "R sessionInfo(), packageVersion() and rmarkdown::pandoc_version()", data.table(
  file = "session_info.txt", variable = "(text)", description = "R version, platform, operating system, collation locale, package versions and pandoc version", unit = "text"))
cat(readLines(here::here("output", "session_info.txt")), sep = "\n")
