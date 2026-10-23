## Report helpers: formatters, tables (kt() also saves each printed table when td is set), links to project files and the
## data-build objects used by Section 1, the data (code/08_data.Rmd), and Section 1.1, how the WHO Mortality Database was
## processed (code/08_who_processing.Rmd), for the report (code/08_report.Rmd) and the web page of Section 1
## (docs/who_mortality_harmonisation.Rmd). The parent document sets link_base ("../": links relative to report/; a GitHub
## blob URL: links to the repository, code lines inside the page or as #L anchors) and td (folder of the saved tables;
## NULL: none saved) before sourcing.
## Usage: source(here::here("code", "08_helpers.R"), local = knitr::knit_global()), after code/functions_pm.R
## Writes <td>/table_<number>.csv and <td>/index.csv through kt() (the report only: output/report_tables/)

library(knitr); library(kableExtra)
options(knitr.kable.NA = "")
rd   <- function(...) fread(here::here("output", ...), na.strings = "")
kt   <- function(x, caption, ..., head = NULL, g = NULL, html = NULL) { ksv(x, caption, list(...)$col.names, head, g)   # html: x with links, printed unescaped
  k <- kable_styling(kbl(if (is.null(html)) x else html, caption = caption, escape = is.null(html), ...), bootstrap_options = c("condensed", "striped", "hover"), full_width = FALSE, position = "left", font_size = 13)
  if (is.null(head)) k else add_header_above(k, head) }
kp   <- function(x, by, caption, ..., head = NULL) { r <- rle(as.character(x[[by]])); e <- cumsum(r$lengths); k <- kt(x[, setdiff(names(x), by), with = FALSE], caption, ..., head = head, g = x[[by]])
  for (i in seq_along(e)) k <- pack_rows(k, r$values[i], e[i] - r$lengths[i] + 1, e[i]); k }
## every printed table also saved as printed to <td>/table_<number>.csv (spanning header 'header: column', row groups in
## 'Group') and listed in index.csv in print order (supplementary tables, code/14; NOTES item 105); not saved when td is NULL
ksv  <- function(x, caption, cn, head, g) { if (is.null(td)) return(invisible()); n <- sub("^Table ([0-9.]+)\\. .*", "\\1", caption); v <- if (is.null(cn)) names(x) else cn
  if (!is.null(head)) { h <- trimws(rep(names(head), head)); v <- ifelse(h == "", v, paste0(h, ": ", v)) }
  y <- setnames(copy(as.data.table(x)), replace(v, v == "", "Row")); if (!is.null(g)) y <- cbind(Group = g, y)
  fwrite(y, file.path(td, sprintf("table_%s.csv", n))); tb <<- rbind(tb, data.table(number = n, caption = caption, file = sprintf("table_%s.csv", n)))
  fwrite(tb, file.path(td, "index.csv")) }
fig  <- function(d, f) include_graphics(here::here("output", d, "figures", f))
num1 <- function(x) as.numeric(regmatches(x, regexpr("[0-9.]+", x)))                # first number of a label
dsc  <- function(d, f, v) rd(d, "dictionary.csv")[file %like% f & variable == v, description[1]]
andl <- function(x) if (length(x) == 0) "" else if (length(x) == 1) paste(x) else paste(paste(head(x, -1), collapse = ", "), "and", tail(x, 1))
fx   <- function(x, d) fifelse(is.na(x), "", sprintf(paste0("%.", d, "f"), round(x, d) + 0)); f1 <- function(x) fx(x, 1); f2 <- function(x) fx(x, 2); f3 <- function(x) fx(x, 3)
p1   <- function(x) fx(100 * x, 1)
fn   <- function(x) format(round(x), big.mark = ",", trim = TRUE)
rng  <- function(x) paste(unique(range(x)), collapse = "-")
ci   <- function(r, l, h) sprintf("%s (%s-%s)", f2(r), f2(l), f2(h))
sx   <- c(`1` = "male", `2` = "female"); Sx <- c(`1` = "Male", `2` = "Female")
cl   <- c(cancer = "cancer", cvd = "cardiovascular diseases", external = "external causes", covid = "COVID-19", other = "other causes", ill_defined = "ill-defined causes")
cs6  <- c(cancer = "Cancer", cvd = "Cardiovascular", external = "External", covid = "COVID-19", other = "Other", ill_defined = "Ill-defined")
cn   <- c(lung = "lung cancer", copd = "COPD", ihd = "IHD", diabetes = "diabetes", crd = "chronic respiratory diseases", stroke = "stroke", cancer = "cancer", cvd = "cardiovascular diseases")
src  <- sub(" \\(.*", "", who_src)

## links to project files: link_base + path (report: relative to report/, which works while report/ sits next to code/; page:
## GitHub, where only the files of the public repository, pub_files from docs/published.csv, are linked; without pub_files,
## the files not ignored by Git). lnr(): lines of a block in a script, read at
## render time: the first line containing a, to the first line containing b, or the closing "}", from there. lnl() (Markdown)
## and lnh() (HTML): line text linked to the lines, on the page only: inside the page for the data-build scripts shown in its
## appendix (ap; code block cid(f), lines cid(f)-<n>, class cl and data-to = last line for its script), else on GitHub
## (.Rmd and .md files with ?plain=1, so that the #L anchors work)
gh   <- function() grepl("^https?://", link_base)
gi0  <- new.env()                                                                   # git check-ignore, once per path
ign  <- function(f) vapply(f, function(z) { if (is.null(gi0[[z]])) gi0[[z]] <- system2("git", c("-C", shQuote(here::here()), "check-ignore", "-q", z)) == 0; gi0[[z]] }, NA, USE.NAMES = FALSE)
lko  <- function(f) if (!gh()) rep(TRUE, length(f)) else if (exists("pub_files")) f %in% pub_files else !ign(f)   # linked: always in the report; on the page the published files (docs/published.csv), else files in Git
lk   <- function(f, t = f) ifelse(lko(f), sprintf("[%s](%s%s)", t, link_base, f), t)
ha   <- function(f, t = f, l = "") ifelse(lko(f), sprintf('<a href="%s%s%s">%s</a>', link_base, f, l, t), t)   # HTML link (tables)
lkd  <- function(f) paste0(lk(f), ifelse(grepl("[.]rds$", f), if (gh()) ifelse(ign(f), " (R data, not in the repository)", " (R data)") else " (R data)", ""))   # .rds: R data, downloaded by the browser
lnr  <- function(f, a, b = NA) { x <- readLines(here::here(f)); i <- grep(a, x, fixed = TRUE)[1]; stopifnot(!is.na(i)); r <- x[i:length(x)]
  j <- i - 1L + (if (is.na(b)) 1L else if (b == "}") match("}", r) else grep(b, r, fixed = TRUE)[1]); stopifnot(!is.na(j)); c(i, j) }
ap   <- c("code/functions_pm.R", "code/01_areas.R", "code/02_cause_groups.R", "code/03_who_deaths.R", "code/04_area_series.R")   # data build
cid  <- function(f) paste0("code-", sub("[.][^.]*$", "", basename(f)))
lnu  <- function(f, i, j) { f <- rep_len(f, length(i))
  ifelse(f %in% ap, sprintf("#%s-%d", cid(f), i), sprintf("%s%s%s#L%d-L%d", link_base, f, ifelse(grepl("[.]R?md$", f), "?plain=1", ""), i, j)) }
lnok <- function(f, i) { f <- rep_len(f, length(i)); gh() & (f %in% ap | lko(f)) }   # line links on the page: appendix scripts and published files
lnl  <- function(f, i, j, t) ifelse(lnok(f, i), sprintf("[%s](%s)%s", t, lnu(f, i, j), ifelse(rep_len(f, length(i)) %in% ap, sprintf('{.cl data-to="%d"}', j), "")), t)
lnh  <- function(f, i, j, t) ifelse(lnok(f, i), sprintf('<a href="%s"%s>%s</a>', lnu(f, i, j), ifelse(rep_len(f, length(i)) %in% ap, sprintf(' class="cl" data-to="%d"', j), ""), t), t)

## data build: objects used by Section 1 and by the rest of the report
ad <- fread(here::here("data", "dict", "areas.csv"), na.strings = "")
ai <- rd("data_build", "areas_included.csv"); wt <- rd("data_build", "window_transitions.csv")
cc <- rd("data_build", "country_coverage.csv"); cy <- cc[level == "country_year"]; c1 <- cc[level == "country"]
ill_lab <- cy[reason %like% "ill-defined", reason[1]]; min_run <- num1(c1[status %like% "run <", status[1]])
y0 <- function(w) as.integer(sub("-.*", "", w)); y1 <- function(w) as.integer(sub(".*-", "", w))
aq <- ad[include == TRUE, .(iso3, name, m49_code, who_quality)]
acn <- ai[, .(iso3 = strsplit(countries, " ")[[1]], y0 = y0(window), y1 = y1(window)), by = area]
ps  <- function(x) fifelse(x %like% "\\+", "mixed (WHO and WPP calibrated)", x)
base <- rd("sdg34", "gap.csv")$year[1]                                              # base year of Result 1 (Summary, Section 1)
csi <- readRDS(here::here("data", "derived", "country_series.rds"))[in_window == TRUE]; csr <- csi[group %in% c("all", "covid")]
## population seams at 30-69 (Section 1 and Section 7): r = calibrated/WPP in the WPP year, r0 = WHO/WPP in the adjacent WHO
## year, over the cells inside the area windows (person-years at 30-69 as weights)
sm  <- rd("data_build", "population_seams.csv")[age %in% a3069]
pyc <- csr[group == "all" & age %in% a3069, .(area, iso3, year, sex, age, py)]
sw  <- merge(merge(sm, pyc[, .(iso3, year_wpp = year, sex, age, p1 = py)], by = c("iso3", "year_wpp", "sex", "age")), pyc[, .(iso3, year_who = year, sex, age, p0 = py)],
             by = c("iso3", "year_who", "sex", "age"), all.x = TRUE)[, .(r = sum(p1) / sum(p1 / ratio_wpp), r0 = sum(p0) / sum(p0 / ratio_who)), by = .(iso3, country, year_who, year_wpp)]
## COVID-19 deaths interpolated into years before 2020 (gaps of 1-3 years; known issue, NOTES item 92)
cvp <- csr[group == "covid" & year < 2020 & deaths > 0, .(d = sum(deaths)), by = .(area, country, year)]
cvi <- csr[country %in% cvp$country & group == "all" & interpolated == TRUE, .(y = runs_txt(year), lo = min(year) - 1L, hi = max(year) + 1L), by = country]
cva <- csr[area %in% cvp$area & year %in% cvp$year, .(s = sum(deaths[group == "covid"], na.rm = TRUE) / sum(deaths[group == "all"], na.rm = TRUE)), by = .(area, year)]
pe  <- rd("sdg34", "period.csv")                                                    # F1 period indicator (Result 1; peaks of Section 1.1)
## NA cells of the area series at 20-84 (ages merged in one country): interpolated over gaps of 1-3 years (07, 11, 12) or
## dropped at a window end
asa <- readRDS(here::here("data", "derived", "area_series.rds")); asr <- asa[group == "all" & age %in% 5:17, .(area, sex, age, year, deaths, py)]
ina <- interp(asr[!is.na(deaths) & !is.na(py)], c("area", "sex", "age"), NULL, "deaths")
iyr <- ina[, .(t = sprintf("%s %s (ages %d-%d)", area, runs_txt(year), 5L * (min(age) - 1L), 5L * max(age) - 1L)), by = area]
eyr <- fsetdiff(unique(asr[is.na(deaths) | is.na(py), .(area, year)]), unique(ina[, .(area, year)]))[, .(t = sprintf("%s %s", area, runs_txt(year))), by = area]
