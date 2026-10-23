## Prem_mortality helpers: paths, WHO raw-file harmonisation (ages, gaps), outputs (dictionary, figures, theme).
## Adapted from Eu40_APC code/functions_competing.R and code/35_who_causes_full_history.R. data.table.

library(data.table)
eu40_root    <- file.path(here::here(), "..", "Eu40_APC")                  # read-only sources: projects next to this one
estican_root <- file.path(here::here(), "..", "estican")
who_raw   <- file.path(eu40_root, "data", "who_mortality", "raw")           # WHO Mortality Database, February 2026 (read-only)
who_src   <- "WHO Mortality Database, February 2026 release (raw files downloaded from cdn.who.int on 10 October 2026, Eu40_APC NOTES item 2)"
who_files <- c("morticd07/MortIcd7", "morticd08/Morticd8", "morticd09/Morticd9", sprintf("morticd10_part%d/Morticd10_part%d", 1:6, 1:6))
who_xl    <- c(Germany = "mortICD_DEU", Norway = "mortICD_NOR")             # morticd10_add workbooks (file = sheet)
wpp_rds   <- file.path(estican_root, "data", "UNDP", "pop_UN.rds")          # UN WPP population (estican, read-only)
wpp_src   <- "UN World Population Prospects (revision to be confirmed), estican data/UNDP/pop_UN.rds of 2 June 2026 (as used for GLOBOCAN 2024; read-only)"
py_src    <- "person-years WHO, else UN WPP calibrated to WHO by sex and age (NOTES item 84)"
pm_src    <- sprintf("%s; %s; UN WPP: %s", who_src, py_src, wpp_src)                       # provenance of files with WHO deaths and person-years
cap_src   <- sprintf("Source: %s; %s.", sub(" \\(.*", "", who_src), py_src)               # figure captions
who_q     <- file.path(estican_root, "data", "WHO_mortality_data_quality.csv")   # WHO data-quality assessment (estican, read-only)
ac        <- paste0("a", 1:18)                                              # 5-year age groups: 1 = 0-4, ..., 17 = 80-84, 18 = 85+
a3069     <- 7:14                                                           # ages 30-69
okabe     <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#000000")

## WHO columns <p>2..<p>25 to the 18 age groups by Frmat (PDF p.8, Annex 1; NS = Norway workbook, ages 0-14 merged).
## Blank cells are merged into the preceding group; groups merged across ages are NA (03/04 75+, 05 70+, 06 65+,
## 07/08 10-year groups, 09 none)
age18 <- function(d, p) {
  stopifnot(all(d$Frmat %in% c(sprintf("%02d", 0:9), "NS")))
  x <- as.matrix(d[, paste0(p, 2:25), with = FALSE]); x[is.na(x)] <- 0
  a <- cbind(rowSums(x[, 1:5, drop = FALSE]), x[, 6:21, drop = FALSE], rowSums(x[, 22:24, drop = FALSE])); f <- d$Frmat
  a[f %in% c("03", "04"), 16:18] <- NA; a[f == "05", 15:18] <- NA; a[f == "06", 14:18] <- NA
  a[f %in% c("07", "08", "09"), ] <- NA; a[f == "NS", 1:3] <- NA
  d[, (ac) := as.data.table(a)][, known := rowSums(x)][]
}

## missing years inside a series in runs of at most `gap` years, interpolated linearly between the bounding years
gaps <- function(y, gap = 3) {
  m <- setdiff(seq(min(y), max(y)), y)
  if (!length(m)) return(NULL)
  r <- data.table(year = m, run = cumsum(c(1L, diff(m) > 1L)))[, n := .N, by = run][n <= gap]
  if (nrow(r)) r[, .(year, lo = min(year) - 1L, hi = max(year) + 1L), by = run][, .(year, lo, hi)]
}
interp <- function(d, id, by, v, gap = 3) {
  g <- d[, gaps(unique(year), gap), by = id]
  if (!nrow(g)) return(NULL)
  b <- d[, c(id, by, "year", v), with = FALSE]
  x <- merge(g, setnames(copy(b), c("year", v), c("lo", "v0")), by = c(id, "lo"), allow.cartesian = TRUE)
  x <- merge(x, setnames(copy(b), c("year", v), c("hi", "v1")), by = c(id, by, "hi"))
  x[, (v) := v0 + (year - lo) / (hi - lo) * (v1 - v0)][, c("v0", "v1") := NULL][]
}

## sources of person-years ('+'-joined labels, e.g. composites) as one sorted '+'-joined label; "none" if empty
src_join <- function(x) if (length(x)) paste(sort(unique(unlist(strsplit(x, "+", fixed = TRUE)))), collapse = "+") else "none"

## md5 of an R object (cache keys; data.tables as lists, functions deparsed)
digest_txt <- function(x) { f <- tempfile(); on.exit(unlink(f)); writeLines(capture.output(dput(x)), f); unname(tools::md5sum(f)) }

## runs of consecutive years as text ("1969-1985, 1987") and the longest run of TRUE values of `ok` over consecutive years
runs_txt <- function(y) {
  y <- sort(unique(y)); if (!length(y)) return(NA_character_)
  r <- split(y, cumsum(c(1L, diff(y) > 1L)))
  paste(vapply(r, function(z) if (length(z) > 1) paste(range(z), collapse = "-") else as.character(z), ""), collapse = ", ")
}
longest_run <- function(y, ok) {
  d <- data.table(y, ok)[order(y)][, k := rleid(ok, cumsum(c(1L, diff(y) > 1L)))][ok == TRUE]
  if (!nrow(d)) return(list(y0 = NA_integer_, y1 = NA_integer_, n = 0L))
  r <- d[, .(y0 = min(y), y1 = max(y), n = .N), by = k][order(-n, -y1)][1]
  list(y0 = r$y0, y1 = r$y1, n = r$n)
}

## Outputs: dictionary rows of one script replaced in <dir>/dictionary.csv (file, variable, description, unit; a
## '(provenance)' row per file gives the source and the script); figures saved as PDF (cairo) and PNG (300 dpi)
dict_update <- function(dir, script, rows, source = who_src) {
  f  <- file.path(dir, "dictionary.csv")
  pr <- data.table(file = unique(rows$file), variable = "(provenance)", description = sprintf("Source: %s. Written by code/%s", source, script), unit = "text")
  rows <- rbind(pr, rows)[order(match(file, pr$file))]
  old <- if (file.exists(f)) fread(f)[!file %like% script] else NULL
  fwrite(rbind(old, rows[, file := sprintf("%s: %s", script, file)], use.names = TRUE, fill = TRUE), f)
}
dict_figures <- function(dir, script, figs) {
  f <- file.path(dir, "dictionary.csv")
  fwrite(rbind(fread(f), data.table(file = sprintf("%s: figures/%s.{pdf,png}", script, names(figs)), variable = "(figure)", description = figs,
                                    unit = "PDF (vector) and PNG (300 dpi)")), f)
}
save_both <- function(p, f, w, h) {
  dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(paste0(f, ".pdf"), p, width = w, height = h, device = cairo_pdf)
  ggplot2::ggsave(paste0(f, ".png"), p, width = w, height = h, dpi = 300)
}
th_comp <- function(base = 8) ggplot2::theme_bw(base_size = base, base_family = "Arial") +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), panel.grid.major = ggplot2::element_line(colour = "grey92", linewidth = 0.25),
                 strip.background = ggplot2::element_blank(), strip.text = ggplot2::element_text(face = "bold"), legend.position = "top",
                 plot.title = ggplot2::element_text(size = base + 1, face = "bold"), plot.subtitle = ggplot2::element_text(size = base - 1, colour = "grey30"),
                 plot.caption = ggplot2::element_text(size = base - 1.5, colour = "grey30", hjust = 0))
