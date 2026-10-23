## Public repository: copies the files listed in docs/published.csv (path -> public_path) from this project into a clone of
## https://github.com/filhoalm/premature-mortality-cohorts, published one component at a time (component 1: the data
## harmonisation code and the page docs/index.html; no data and no outputs). Checks: every listed file exists, none is under
## data/, output/, report/ or manuscript/, and no file of the clone is left that is not listed (README.md is the public one).
## Commit and push in the clone are done by hand. NOTES item 113.
## Usage (project root): Rscript code/17_publish.R [to=../premature-mortality-cohorts]   (after 16)
## Writes the listed files in the clone

source(here::here("code", "functions_pm.R"))
arg <- function(nm, def) { a <- grep(paste0("^", nm, "="), commandArgs(TRUE), value = TRUE); if (length(a)) sub(".*=", "", a) else def }
to  <- normalizePath(arg("to", file.path(here::here(), "..", "premature-mortality-cohorts")), mustWork = TRUE)
pb  <- fread(here::here("docs", "published.csv"))
stopifnot(dir.exists(file.path(to, ".git")), file.exists(here::here(pb$path)), !grepl("^(data|output|report|manuscript)/", pb$public_path))
invisible(lapply(unique(dirname(file.path(to, pb$public_path))), dir.create, showWarnings = FALSE, recursive = TRUE))
stopifnot(file.copy(here::here(pb$path), file.path(to, pb$public_path), overwrite = TRUE))
ex  <- setdiff(sub("^\\./", "", list.files(to, recursive = TRUE, all.files = TRUE)), pb$public_path); ex <- ex[!grepl("^\\.git/", ex)]
pb[, kb := round(file.size(file.path(to, public_path)) / 1024, 1)]
options(width = 180); print(pb[, .(component, public_path, kb, description)], row.names = FALSE)
if (length(ex)) cat("\nIn the clone but not listed (remove or list them):", ex, sep = "\n  ")
cat(sprintf("\nCopied %d files (%.0f kB) to %s\n", nrow(pb), sum(pb$kb), to))
