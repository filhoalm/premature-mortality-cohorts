## Quality and area series (Step 4). Usable country-year (Results 1-2): person-years (WHO, else UN WPP calibrated to WHO;
## py_source) and deaths of every needed group (all, cancer, cvd, ihd, stroke, external, ill_defined, covid, other, lung,
## diabetes, crd) present at every age 30-69 for both sexes, the List's top-level codes and the groups adding up to the
## all-cause code (unreported items fall in 'other', e.g. suicide in the GDR 1977-1988), and ill-defined causes <= ill_max
## of deaths at 30-69 (both sexes; NOTES item 20); countries rated Low by WHO are not eligible (data/dict/areas.csv).
## The exclusive components of 'other' (oth_*, ICD-10 detailed Lists only) are kept in the series.
## copd (Result 3 only) is flagged: copd_ok per country-year (isolable at every age 30-69, both sexes) and per area-year
## (all countries of the area). Sub-groups are clamped to their parent group (copd <= crd, copd44 <= copd, ihd + stroke <=
## cvd, lung <= cancer, ncd4 <= all). Usable series = longest run of consecutive usable years (interpolated years count,
## flagged); included countries (data/dict/areas.csv) with >= min_run usable years enter the areas. Area = UN M49
## sub-region: the window (>= min_win years where possible) and the set of countries covering all of it that maximise
## person-years at 30-69 (population x years; balanced composition), with alternatives (all candidates, each country left
## out, best window covering the 2015 base year); deaths and person-years summed by year, sex, age and group.
## Usage (project root): Rscript code/04_area_series.R [ill_max=0.20] [min_run=20] [min_win=25] [drop=lung] [write=FALSE]
##   (write=FALSE prints the area table only, for sensitivity runs)
## Writes data/derived/{country_series,area_series}.rds, data/derived/dictionary.csv, output/data_build/{areas_included,
##        area_windows,window_transitions,country_coverage,dictionary}.csv, output/data_build/figures/D1_coverage.{pdf,png}

source(here::here("code", "functions_pm.R")); library(ggplot2)

arg <- function(nm, def) { a <- grep(paste0("^", nm, "="), commandArgs(TRUE), value = TRUE); if (length(a)) type.convert(sub(".*=", "", a), as.is = TRUE) else def }
ill_max <- arg("ill_max", 0.20)
min_run <- arg("min_run", 20L)
min_win <- arg("min_win", 25L)
write   <- arg("write", TRUE)
base    <- 2015L                                                            # SDG 3.4 base year (Result 1)
need    <- setdiff(c("all", "cancer", "cvd", "ihd", "stroke", "external", "ill_defined", "covid", "other", "lung", "diabetes", "crd"), strsplit(arg("drop", ""), ",")[[1]])
oth     <- c("oth_alcohol", "oth_drug", "oth_hiv", "oth_liver", "oth_diabetes", "oth_copd", "oth_infectious", "oth_residual")   # components of other (Eu40_APC B5)
keep    <- c("all", "cancer", "lung", "cvd", "ihd", "stroke", "diabetes", "crd", "copd", "copd44", "ncd4", "external", "ill_defined", "covid", "other", oth)
res     <- c("ncd4", "cancer", "cvd", "ihd", "stroke", "diabetes", "crd", "lung", "copd")   # groups of Results 1-3
dir_out <- here::here("output", "data_build")
dir_d   <- here::here("data", "derived")
cat(sprintf("Parameters: ill_max = %.2f, min_run = %d, min_win = %d; needed groups: %s (copd flagged)\n", ill_max, min_run, min_win, paste(need, collapse = ", ")))

areas <- fread(here::here("data", "dict", "areas.csv"), na.strings = "")
dt    <- readRDS(file.path(dir_d, "who_deaths.rds"))[group %chin% keep]
cys   <- readRDS(file.path(dir_d, "who_checks.rds"))

## ----- sub-groups clamped to their parent group (unknown age is spread within each group) -----
w  <- dcast(dt, who_code + year + sex + age ~ group, value.var = "deaths")
cl <- function(x, p) fifelse(!is.na(p) & x > p, p, x)
w[, `:=`(copd = cl(copd, crd), lung = cl(lung, cancer), ncd4 = cl(ncd4, all))][, copd44 := cl(copd44, copd)]
w[, f := cl(1, cvd / (ihd + stroke))][, `:=`(ihd = ihd * f, stroke = stroke * f)]
w  <- melt(w[, .(who_code, year, sex, age, copd, copd44, lung, ncd4, ihd, stroke)], id.vars = 1:4, variable.name = "group", value.name = "d", variable.factor = FALSE)
dt[w, on = .(who_code, year, sex, age, group), d := i.d]
cat("\nSub-groups clamped to their parent group (cells, of which at 30-69; largest reduction in deaths):\n")
print(dt[d < deaths, .(cells = .N, cells_3069 = sum(age %in% a3069), max_reduction = max(deaths - d)), by = group])
dt[!is.na(d), deaths := d][, d := NULL]

## ----- usable country-years -----
a <- dt[age %in% a3069]
u <- merge(a[group %chin% c(need, "copd"), .(miss = paste(sort(unique(group[is.na(deaths) & group %chin% need])), collapse = ","), copd_ok = !anyNA(deaths[group == "copd"])), by = .(who_code, year)],
           a[group == "all", .(py_ok = !anyNA(py) & uniqueN(sex) == 2, py = sum(py), deaths = sum(deaths), py_source = src_join(py_source[!is.na(py)]),
                               interpolated = interpolated[1], composite = composite[1], list = list[1]), by = .(who_code, year)], by = c("who_code", "year"))
k <- cys[sex > 0, .(check_ok = all(sum_diff < 1e-6 & other_min >= -1e-9 & fcoalesce(other_raw_min, 0) >= 0) & all(chapters_equal %in% TRUE)), by = .(who_code, year)]
u <- merge(merge(u, k, by = c("who_code", "year"), all.x = TRUE), cys[sex == 0, .(who_code, year, ill = ill_share_3069)], by = c("who_code", "year"), all.x = TRUE)
u[, reason := fcase(!py_ok, "population missing", miss != "", "group not isolable", !check_ok %in% TRUE, "codes do not add up",
                    is.na(ill) | ill > ill_max, sprintf("ill-defined > %.0f%%", 100 * ill_max), default = "usable")]
u[, usable := reason == "usable"]
u <- merge(areas[, .(who_code, iso3, m49_code, country = name, region, region_code, area = subregion, area_code = subregion_code, include, excl = reason)], u, by = "who_code")

## ----- usable series per country -----
lr <- u[, longest_run(year, usable), by = who_code]
cr <- merge(u[, .(first = min(year), last = max(year), n_obs = .N, n_usable = sum(usable)), by = .(who_code, iso3, m49_code, country, region, region_code, area, area_code, include)], lr, by = "who_code")
cr <- merge(cr, u[cr, on = "who_code", allow.cartesian = TRUE][year >= y0 & year <= y1, .(n_interp = sum(interpolated), n_composite = sum(composite)), by = who_code], by = "who_code", all.x = TRUE)
rl <- c("population missing", "group not isolable", sprintf("ill-defined > %.0f%%", 100 * ill_max), "codes do not add up")
rs <- dcast(u[usable == FALSE, .N, by = .(who_code, reason = factor(reason, rl))], who_code ~ reason, value.var = "N", fill = 0, drop = FALSE)
setnames(rs, rl, c("n_population", "n_groups", "n_ill", "n_sum"))
cr <- merge(cr, rs, by = "who_code", all.x = TRUE)
for (v in c("n_population", "n_groups", "n_ill", "n_sum", "n_interp", "n_composite")) set(cr, which(is.na(cr[[v]])), v, 0L)
cr[, candidate := include & n >= min_run]

## ----- area windows: maximise person-years at 30-69 over countries covering the whole window; alternatives -----
pc <- u[usable == TRUE, .(who_code, year, py)][order(who_code, year)][, cum := cumsum(py), by = who_code]
cw <- function(cc, L) {                                                 # (country, window) pairs: windows of >= L years inside the usable run
  w <- CJ(a = min(cc$y0):max(cc$y1), b = min(cc$y0):max(cc$y1))[b - a + 1 >= L]
  x <- cc[w, on = .(y0 <= a, y1 >= b), nomatch = NULL, .(area = x.area, who_code = x.who_code, iso3 = x.iso3, a = i.a, b = i.b)]
  x[pc, on = .(who_code, b = year), py := i.cum][pc, on = .(who_code, a = year), py := py - i.cum + i.py][]
}
best <- function(x, alt) if (nrow(x)) x[, .(set = list(who_code), countries = paste(sort(iso3), collapse = " "), py = sum(py)), by = .(area, a, b)][
  order(-py, -(b - a), -b)][1][, alternative := alt][]
alts <- function(cc) {
  L <- min(min_win, max(cc$n)); x <- cw(cc, L); r <- best(x, "chosen")[, min_len := L]; s <- r$set[[1]]
  rbind(r, if (length(s) < nrow(cc)) best(cw(cc, 1L)[, if (.N == nrow(cc)) .SD, by = .(a, b)], "all candidates"),
        rbindlist(lapply(if (length(s) > 1) s, function(k) best(x[who_code != k], paste("without", cc[who_code == k, iso3])))),
        if (r$a > base | r$b < base) best(x[a <= base & b >= base], sprintf("covers %d", base)), fill = TRUE)
}
al <- rbindlist(lapply(split(cr[candidate == TRUE], by = "area"), alts), fill = TRUE)
aw <- al[alternative == "chosen", .(who_code = unlist(set)), by = .(area, y0 = a, y1 = b, min_len)]
cr <- merge(cr, aw[, .(who_code, in_area = TRUE, win0 = y0, win1 = y1)], by = "who_code", all.x = TRUE)[is.na(in_area), in_area := FALSE]
ww <- unique(aw[, .(area, wy0 = y0, wy1 = y1)])
cr <- merge(cr, ww, by = "area", all.x = TRUE)
cr[, status := fcase(in_area, "in area", !include, "excluded code", n == 0, "no usable year", n < min_run, sprintf("usable run < %d years", min_run),
                     default = "run does not cover the area window")]
cr[status == "run does not cover the area window", status := sprintf("run %d-%d does not cover the area window %d-%d", y0, y1, wy0, wy1)]

## ----- area and country series -----
ca <- merge(dt, cr[candidate == TRUE, .(who_code, iso3, m49_code, area, area_code, y0, y1, in_area, win0, win1)], by = "who_code")[year >= y0 & year <= y1]
ca <- merge(ca[, in_window := in_area & year >= win0 & year <= win1][, c("y0", "y1", "in_area", "win0", "win1") := NULL], u[, .(who_code, year, copd_ok)], by = c("who_code", "year"))
ca[, src := fifelse(py_source %like% "^WPP", py_source, "WHO")]                # WHO (incl. interpolated and composites), WPP calibrated, WPP
cs <- ca[, .(who_code, iso3, m49_code, country, area, area_code, year, sex, age, group, deaths, py, py_source, list, interpolated, composite, copd_ok, in_window)]
ay <- unique(cs[in_window == TRUE, .(area, who_code, year, copd_ok)])[, .(copd_ok = all(copd_ok)), by = .(area, year)]
pys <- ca[in_window == TRUE & group == "all", .(py_source = src_join(src[!is.na(py)])), by = .(area, year, sex, age)]
as <- ca[in_window == TRUE, .(deaths = sum(deaths), py = sum(py), n_countries = uniqueN(who_code), n_interpolated = uniqueN(who_code[interpolated]),
                              n_composite = uniqueN(who_code[composite])), by = .(area, year, sex, age, group)]
as <- merge(merge(merge(unique(areas[, .(area = subregion, area_code = subregion_code, region, region_code)]), as, by = "area"), ay, by = c("area", "year")), pys, by = c("area", "year", "sex", "age"))[
  order(region, area, year, sex, match(group, keep), age), .(area, area_code, region, region_code, year, sex, age, group, deaths, py, py_source, n_countries, n_interpolated, n_composite, copd_ok)]
stopifnot(!as[age %in% a3069 & group %chin% need, anyNA(deaths) | anyNA(py)], !as[copd_ok == TRUE & age %in% a3069 & group == "copd", anyNA(deaths)])

## ----- territory changes (data/dict/areas.csv change_years) inside windows: person-years at 30-69 vs the last observed year -----
pa <- as[group == "all" & age %in% a3069, .(py = sum(py)), by = .(area, year)]
tc <- merge(areas[!is.na(change_years), .(year = as.integer(strsplit(change_years, ";")[[1]])), by = who_code], cr[in_area == TRUE, .(who_code, iso3, area, win0, win1)], by = "who_code")[year > win0 & year <= win1]
tc[, y_prev := u[who_code == .BY$who_code & year < .BY$year & interpolated == FALSE, max(year)], by = .(who_code, year)]
tc[u, on = .(who_code, year), p1 := i.py][u, on = .(who_code, y_prev = year), p0 := i.py][pa, on = .(area, year), a1 := i.py][pa, on = .(area, y_prev = year), a0 := i.py]
tc[, `:=`(country_step = p1 / p0 - 1, area_step = a1 / a0 - 1)]

## ----- List transitions inside the windows (output/data_build/list_transitions.csv), groups of Results 1-3 -----
wt <- merge(cr[in_area == TRUE, .(area, area_code, who_code, iso3, m49_code, win0, win1)], fread(file.path(dir_out, "list_transitions.csv"))[group %chin% res], by = "who_code")[year_old >= win0 & year_new <= win1]
wt <- wt[order(area, iso3, year_old, match(group, res)), .(area, area_code, who_code, iso3, m49_code, country, list_old, list_new, year_old, year_new, group, deaths_old, deaths_new,
                                                          share_old, share_new, jump_share, flag, count_jump)]

## ----- area table and alternatives -----
s  <- as[age %in% a3069, .(py = sum(py[group == "all"]), deaths = sum(deaths[group == "all"]), ill = sum(deaths[group == "ill_defined"]),
                           ncd4 = sum(deaths[group == "ncd4"])), by = .(region, area)]
at <- merge(aw[, .(window = sprintf("%d-%d", y0[1], y1[1]), n_years = y1[1] - y0[1] + 1L, min_len = min_len[1], n_countries = .N, covers_base_year = y0[1] <= base & y1[1] >= base), by = area],
            cr[in_area == TRUE, .(countries = paste(sort(iso3), collapse = " ")), by = area], by = "area")
at <- merge(merge(s, at, by = "area"), cr[candidate == TRUE & in_area == FALSE, .(excluded = paste(sprintf("%s (%d-%d)", iso3, y0, y1), collapse = "; ")), by = area], by = "area", all.x = TRUE)
at <- merge(at, cr[include == TRUE & n > 0 & n < min_run, .(short_runs = paste(sprintf("%s (%d)", iso3, n), collapse = "; ")), by = area], by = "area", all.x = TRUE)
at <- merge(at, tc[, .(territory_changes = paste(sprintf("%s %d (person-years 30-69 vs %d: country %+.1f%%, area %+.1f%%)", iso3, year, y_prev, 100 * country_step, 100 * area_step), collapse = "; ")), by = area],
            by = "area", all.x = TRUE)
pw <- ca[in_window == TRUE & group == "all" & age %in% a3069]
at <- merge(at, pw[, .(wpp_cal = sum(py[src == "WPP calibrated"]) / sum(py), wpp = sum(py[src == "WPP"]) / sum(py)), by = area], by = "area")
at <- merge(at, pw[src != "WHO", .(y = runs_txt(year)), by = .(area, iso3, src)][, .(wpp_years = paste(sprintf("%s %s (%s)", iso3, y, src), collapse = "; ")), by = area], by = "area", all.x = TRUE)
at <- merge(at, areas[reason %like% "^WHO data-quality" & mort_years > 0, .(excluded_quality = paste(sprintf("%s (usability %.1f%%)", iso3, who_usability), collapse = "; ")), by = .(area = subregion)],
            by = "area", all.x = TRUE)
at <- merge(unique(areas[, .(area = subregion, area_code = subregion_code, region_code)]), at, by = "area")
at <- at[order(region, area), .(area, area_code, region, region_code, n_countries, countries, window, n_years, min_len, covers_base_year, py_3069 = py,
                                share_py_wpp_calibrated = wpp_cal, share_py_wpp = wpp, wpp_years, deaths_3069 = deaths, deaths_ncd4_3069 = ncd4, ill_share_3069 = ill / deaths,
                                excluded_covering_short = excluded, short_runs, excluded_quality, territory_changes)]
av <- merge(unique(areas[, .(area = subregion, area_code = subregion_code, region, region_code)]), al[, .(area, i = .I, alternative, window = sprintf("%d-%d", a, b), n_years = b - a + 1L,
                                                                  n_countries = lengths(set), countries, py_3069 = py, covers_base_year = a <= base & b >= base)], by = "area")
av <- av[, py_vs_chosen := py_3069 / py_3069[alternative == "chosen"], by = area][order(region, area, i), .(area, area_code, region, region_code, alternative, window, n_years,
                                                                                                              n_countries, countries, py_3069, py_vs_chosen, covers_base_year)]
cat(sprintf("\nUsable country-years: %d of %d with deaths (%d countries with >= 1); candidates (included, >= %d usable years in a run): %d countries\n",
            u[, sum(usable)], nrow(u), u[usable == TRUE, uniqueN(who_code)], min_run, cr[candidate == TRUE, .N]))
print(u[, .N, by = reason][order(-N)])
cat("Usable country-years by source of person-years at 30-69:\n"); print(u[usable == TRUE, .(country_years = .N, countries = uniqueN(who_code)), by = py_source][order(-country_years)])
cat("Ill-defined:", u[reason %like% "^ill", .N, by = iso3][order(-N)][1:6, paste(sprintf("%s %d", iso3, N), collapse = ", ")], "\n")
cat("Groups not isolable (country-years):\n"); print(head(u[reason == "group not isolable", .(.N, lists = substr(paste(sort(unique(list)), collapse = ","), 1, 50)), by = .(miss)][order(-N)], 12))
cat("copd isolable (copd_ok) among usable country-years, by List:\n"); print(dcast(u[usable == TRUE, .N, by = .(list, copd_ok)], list ~ copd_ok, value.var = "N", fill = 0))
cat("\nArea table:\n")
print(at[, .(area, n = n_countries, countries, window, base = covers_base_year, py_3069_m = round(py_3069 / 1e6), wpp = sprintf("%.1f%%", 100 * (share_py_wpp_calibrated + share_py_wpp)),
             deaths_3069_k = round(deaths_3069 / 1e3), ill = sprintf("%.1f%%", 100 * ill_share_3069))], nrows = 30)
cat("WPP person-years in the windows (calibrated unless stated):\n"); writeLines(at[!is.na(wpp_years), sprintf("  %s: %s", area, wpp_years)])
cat("Not eligible (WHO data-quality assessment Low):\n"); writeLines(at[!is.na(excluded_quality), sprintf("  %s: %s", area, excluded_quality)])
cat("\nCandidates not in their area (usable run does not cover the window):\n")
print(cr[candidate == TRUE & in_area == FALSE, .(area, iso3, country, run = sprintf("%d-%d", y0, y1), window = sprintf("%d-%d", wy0, wy1))], nrows = 60)
cat("\nAlternative windows (area_windows.csv; person-years at 30-69, million):\n")
print(av[, .(area, alternative, window, countries, py_m = round(py_3069 / 1e6), rel = round(py_vs_chosen, 3), base = covers_base_year)], nrows = 80)
cat("\nTerritory changes inside windows:\n"); print(tc[, .(area, iso3, year, y_prev, country_step = round(country_step, 3), area_step = round(area_step, 3))])
cat("\ncopd_ok by area (years of the window):\n"); print(ay[, .(copd_ok = runs_txt(year[copd_ok]), not_ok = runs_txt(year[!copd_ok])), by = area])
cat("\nList transitions inside the windows with a flag (|change in the share of deaths at 30-69| > 15%), groups of Results 1-3 (* count also > 15%; (s) < 100 deaths on one side):\n")
writeLines(wt[flag == TRUE, sprintf("  %s: %s %s>%s %d-%d: %s", area[1], iso3[1], list_old[1], list_new[1], year_old[1], year_new[1],
                                    paste(sprintf("%s %+.0f%%%s%s", group, 100 * jump_share, fifelse(count_jump, "*", ""), fifelse(pmin(deaths_old, deaths_new) < 100, " (s)", "")), collapse = ", ")),
              by = .(area, who_code, year_old)]$V1)
cat(sprintf("\nNorthern Europe present: %s; Eastern Asia present: %s\n", "Northern Europe" %in% at$area, "Eastern Asia" %in% at$area))
if (!write) quit(save = "no")

saveRDS(cs, file.path(dir_d, "country_series.rds"))
saveRDS(as, file.path(dir_d, "area_series.rds"))
fwrite(at, file.path(dir_out, "areas_included.csv"))
fwrite(av, file.path(dir_out, "area_windows.csv"))
fwrite(wt, file.path(dir_out, "window_transitions.csv"))
cov <- rbind(cr[, .(level = "country", who_code, iso3, m49_code, country, region, region_code, area, area_code, include, first, last, n_obs, n_usable,
                    run = fifelse(n > 0, sprintf("%d-%d", y0, y1), NA_character_), n_run = n, n_interp, n_composite, n_population, n_groups, n_ill, n_sum, status,
                    window = fifelse(is.na(wy0), NA_character_, sprintf("%d-%d", wy0, wy1)))],
             u[, .(level = "country_year", who_code, iso3, m49_code, country, region, region_code, area, area_code, include, year, list, interpolated, composite,
                   py_3069 = py, py_source, deaths_3069 = deaths, ill_share_3069 = ill, missing_groups = miss, check_ok, copd_ok, usable, reason)], fill = TRUE)
fwrite(cov[order(level, region, area, who_code, year)], file.path(dir_out, "country_coverage.csv"))

## ----- figure D1: years by country, grouped by area -----
f <- merge(u[include == TRUE], cr[, .(who_code, in_area, win0, win1, n)], by = "who_code")[n > 0]
f[, st := fcase(usable & in_area & year >= win0 & year <= win1, "In area series", usable, "Usable", default = paste("Not usable:", reason))]
lv <- c("In area series", "Usable", "Not usable: population missing", "Not usable: group not isolable", sprintf("Not usable: ill-defined > %.0f%%", 100 * ill_max), "Not usable: codes do not add up")
f[, st := factor(st, lv)]
f[, lab := sprintf("%s %s", iso3, fifelse(n >= min_run, "", "*"))]
f[, area := factor(area, unique(areas[order(region, subregion), subregion]))]
p <- ggplot(f, aes(year, reorder(lab, -who_code), fill = st)) + geom_tile(height = 0.85) +
  geom_point(data = f[interpolated == TRUE | composite == TRUE | py_source %like% "WPP"], aes(year, reorder(lab, -who_code),
             shape = fcase(composite, "Composite year", interpolated, "Interpolated year", default = "WPP person-years")), size = 0.5, colour = "black", inherit.aes = FALSE) +
  facet_grid(area ~ ., scales = "free_y", space = "free_y", labeller = label_wrap_gen(16)) +
  scale_fill_manual(NULL, values = setNames(okabe[c(5, 2, 1, 6, 4, 8)], lv), drop = TRUE) +
  scale_shape_manual(NULL, values = c(`Composite year` = 4, `Interpolated year` = 16, `WPP person-years` = 1)) +
  scale_x_continuous(NULL, breaks = seq(1950, 2025, 10), expand = c(0.005, 0)) + labs(y = NULL,
    title = "D1. WHO mortality data by country and year: usable years and area series",
    subtitle = sprintf("Usable: person-years (WHO, else UN WPP calibrated to WHO) and %d cause groups at every age 30-69, both sexes; ill-defined <= %.0f%% at 30-69. Area = UN M49 sub-region; window and countries maximise person-years", length(need), 100 * ill_max),
    caption = paste(strwrap(c(sprintf("Included codes with at least one usable year; * longest usable run < %d years (not a candidate).", min_run),
      "Not shown: codes excluded in data/dict/areas.csv (former entities overlapping their successors, parts of countries, countries rated Low by the WHO data-quality assessment and others, reasons in the file); the Netherlands Antilles, a former entity overlapping no other code, is included."), 190), collapse = "\n")) +
  th_comp(7) + theme(strip.text.y = element_text(angle = 0, hjust = 0), axis.text.y = element_text(size = 4.5), panel.spacing = unit(1.5, "pt"), legend.key.size = unit(8, "pt")) +
  guides(fill = guide_legend(nrow = 2))
save_both(p, file.path(dir_out, "figures", "D1_coverage"), 8.5, 13)

## ----- dictionaries -----
cmp <- areas[!is.na(composite), paste(sprintf("%s %s", name, composite_years), collapse = ", ")]
src <- paste0(pm_src, "; via data/derived/who_deaths.rds and who_checks.rds (code/03_who_deaths.R)")
grp <- paste0("Cause group: ", paste(keep, collapse = ", "), " (data/derived/who_deaths.rds; sub-groups clamped to their parent group; oth_* = exclusive components of other, ",
              "ICD-10 detailed Lists 103/104/10M only, NA otherwise, adding up to other)")
dict_update(dir_d, "04_area_series.R", source = src, data.table(
  file = c(rep("country_series.rds", ncol(cs)), rep("area_series.rds", ncol(as))),
  variable = c(names(cs), names(as)),
  description = c("WHO country code", "ISO3", "UN M49 numeric country code", "WHO name", "UN M49 sub-region", "UN M49 numeric sub-region code",
                  "Calendar year (longest usable run of the country)", "1 = male, 2 = female", "5-year age group (1 = 0-4, ..., 18 = 85+)", grp,
                  "Deaths (NA outside ages 30-69 where ages are merged)", "Person-years (py_source)",
                  "Source of py: WHO, WHO (interpolated), WPP calibrated, WPP (uncalibrated), '+'-joined for composites (data/derived/who_deaths.rds)",
                  "WHO List", "Interpolated year", "Composite year", "copd isolable at every age 30-69, both sexes (Result 3)", "Year inside the area window and country in the area set",
                  "UN M49 sub-region", "UN M49 numeric sub-region code", "UN M49 region", "UN M49 numeric region code", "Calendar year", "1 = male, 2 = female",
                  "5-year age group (1 = 0-4, ..., 18 = 85+)", grp, "Deaths summed over the area's countries", "Person-years summed over the area's countries",
                  "Sources of py among the area's countries: WHO (incl. interpolated and composites), WPP calibrated, WPP ('+'-joined)",
                  "Countries summed", "Of which with an interpolated year", sprintf("Of which composite (%s)", cmp),
                  "copd isolable at every age 30-69 in every country of the area that year (Result 3)"),
  unit = c("code", "code", "code", "text", "text", "code", "calendar year", "code", "code", "text", "deaths", "person-years", "text", "code", "logical", "logical", "logical", "logical",
           "text", "code", "text", "code", "calendar year", "code", "code", "text", "deaths", "person-years", "text", "count", "count", "count", "logical")))
dict_update(dir_out, "04_area_series.R", source = src, data.table(
  file = c(rep("areas_included.csv", ncol(at)), rep("area_windows.csv", ncol(av)), rep("window_transitions.csv", ncol(wt)), rep("country_coverage.csv", ncol(cov))),
  variable = c(names(at), names(av), names(wt), names(cov)),
  description = c("UN M49 sub-region", "UN M49 numeric sub-region code", "UN M49 region", "UN M49 numeric region code", "Countries in the area series", "ISO3 codes",
                  "Window (first-last year)", "Years in the window",
                  sprintf("Minimum window length used (min_win = %d, or the longest usable run when shorter)", min_win), sprintf("Window includes the SDG 3.4 base year %d", base),
                  "Person-years at 30-69, both sexes, window", "Share of py_3069 from UN WPP calibrated to WHO", "Share of py_3069 from UN WPP uncalibrated",
                  "Countries and years with WPP person-years at 30-69 in the window (source)",
                  "All-cause deaths at 30-69, window", "NCD4 deaths (cancer + cvd + diabetes + crd) at 30-69, window",
                  "Ill-defined share of deaths at 30-69, window", sprintf("Countries with >= %d usable years whose run does not cover the window (run)", min_run),
                  sprintf("Included countries with a usable run < %d years (years)", min_run),
                  "Countries of the sub-region with mortality data not eligible because rated Low by the WHO data-quality assessment (usability, data/dict/areas.csv)",
                  "Territory changes inside the window (data/dict/areas.csv): person-years at 30-69 vs the last observed year before, country and area",
                  "UN M49 sub-region", "UN M49 numeric sub-region code", "UN M49 region", "UN M49 numeric region code",
                  sprintf("chosen (maximum person-years at 30-69, >= min_len years); all candidates (every country with >= %d usable years); without <ISO3> (best window with that country left out); covers %d (best window including the base year, where the chosen does not)", min_run, base),
                  "Window (first-last year)", "Years in the window", "Countries", "ISO3 codes", "Person-years at 30-69, both sexes, window", "py_3069 / py_3069 of the chosen window",
                  sprintf("Window includes the SDG 3.4 base year %d", base),
                  "UN M49 sub-region", "UN M49 numeric sub-region code", "WHO country code", "ISO3", "UN M49 numeric country code", "WHO name", "List before the change", "List after the change",
                  "Last year of the old List", "First year of the new List",
                  "Cause group (Results 1-3)", "Deaths 30-69, both sexes, old year", "Deaths 30-69, new year", "Share of all deaths 30-69, old year", "Share of all deaths 30-69, new year",
                  "Relative change in the 30-69 share", "|jump_share| > 15%", "|change in the 30-69 count| > 15%",
                  "country: one row per WHO code with deaths; country_year: one row per WHO code and year", "WHO country code", "ISO3", "UN M49 numeric country code", "WHO name",
                  "UN M49 region", "UN M49 numeric region code", "UN M49 sub-region", "UN M49 numeric sub-region code",
                  "Eligible code (data/dict/areas.csv)", "First year with deaths", "Last year with deaths", "Years with deaths", "Usable years", "Longest run of usable years",
                  "Years in that run", "Of which interpolated", "Of which composite", "Unusable years: population missing at 30-69", "Unusable years: needed group not isolable",
                  sprintf("Unusable years: ill-defined > %.0f%% at 30-69", 100 * ill_max), "Unusable years: the List's top-level codes (or the groups) do not add up to the all-cause code (unreported items, e.g. GDR 1977-1988)",
                  "in area, excluded code, no usable year, short run, or run does not cover the area window", "Area window",
                  "Year", "WHO List", "Interpolated year", "Composite year", "Person-years 30-69 (both sexes)",
                  "Sources of person-years at 30-69 (data/derived/who_deaths.rds py_source, '+'-joined; none: no person-years)", "All-cause deaths 30-69", "Ill-defined share at 30-69",
                  "Needed groups NA at 30-69", "Groups add up to all-cause (both sexes)", "copd isolable at every age 30-69, both sexes (Result 3)", "Usable country-year", "First failing criterion"),
  unit = c("text", "code", "text", "code", "count", "code", "calendar years", "count", "years", "logical", "person-years", "proportion", "proportion", "text", "deaths", "deaths",
           "proportion", "text", "text", "text", "text",
           "text", "code", "text", "code", "text", "calendar years", "count", "count", "code", "person-years", "ratio", "logical",
           "text", "code", "code", "code", "code", "text", "code", "code", "calendar year", "calendar year", "text", "deaths", "deaths", "proportion", "proportion", "relative change",
           "logical", "logical",
           "text", "code", "code", "code", "text", "text", "code", "text", "code", "logical", "calendar year", "calendar year", "count", "count", "calendar years", "count", "count",
           "count", "count", "count", "count", "count", "text", "calendar years",
           "calendar year", "code", "logical", "logical", "person-years", "text", "deaths", "proportion", "text", "logical", "logical", "logical", "text")))
dict_figures(dir_out, "04_area_series.R", c(D1_coverage = "Years by country (included codes with >= 1 usable year), grouped by area: usable, in area series, or reason not usable; interpolated, composite and WPP person-years marked"))
cat(sprintf("\nWrote %s (%s rows), %s (%s rows)\n", "data/derived/country_series.rds", format(nrow(cs), big.mark = ","), "data/derived/area_series.rds", format(nrow(as), big.mark = ",")))
