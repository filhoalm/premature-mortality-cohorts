## WHO deaths and population (Step 3): deaths by country (WHO code), year, sex, 5-year age group (1 = 0-4, ..., 18 = 85+)
## and cause group (data/dict/who_cause_groups.csv; ncd4 = cancer + cvd + diabetes + crd) for every country and year of
## the WHO Mortality Database raw files (national level: Admin1 and SubDiv blank; ICD-7 to ICD-10, Germany 2021-22 and
## Norway 2017-24 workbooks), with the method of Eu40_APC code/35_who_causes_full_history.R: the all-cause code of each
## List is identified empirically and must be the documented one; ages harmonised row by row from Frmat; unknown age
## redistributed proportionally within each group (all-cause by age = sum of its exclusive parts); interior gaps of up
## to 3 years interpolated linearly. Person-years: WHO population (mort_pop), age formats harmonised, gaps of up to 3
## years interpolated; other cells of country-years with deaths (year missing, ages merged) take UN WPP (estican
## pop_UN.rds, by UN M49 code) calibrated to WHO by sex and age (WHO/WPP ratio averaged over the 3 nearest years with both
## on each side: held constant beyond the first or last WHO year, interpolated linearly between two), uncalibrated where
## a country has no WHO population.
## Composites (data/dict/areas.csv): United Kingdom 2000 = 4310 + 4320 + 4330, Germany 1969-1989 = FRG 4100 + GDR 4090.
## Validation: List transitions (share of all deaths at 30-69 changing by more than 15%), group overlaps, respiratory
## diagnostics (NOTES items 10-13), WPP seams (ratios outside 0.9-1.1, step at the seam), and deaths identical to
## Eu40_APC causes_full_history.rds where both exist.
## Usage (project root): Rscript code/03_who_deaths.R   (national raw extract and one chunk per country and List cached
## in data/derived/cache/03/: the extract keyed on the md5 of the raw files and of keep(), chunks on the md5 of the
## expanded cause groups and of the chunk code)
## Writes data/derived/{who_deaths,who_checks}.rds, data/derived/dictionary.csv, output/data_build/{list_transitions,
##        group_overlap,respiratory_checks,population_sources,population_seams,dictionary}.csv

source(here::here("code", "functions_pm.R")); library(readxl)

t0      <- proc.time()
dir_out <- here::here("output", "data_build")
dir_d   <- here::here("data", "derived")
dir_c   <- file.path(dir_d, "cache", "03")
dir.create(dir_c, showWarnings = FALSE, recursive = TRUE)
core    <- c("cancer", "cvd", "external", "ill_defined", "covid")         # with other: mutually exclusive, sum to all
n4      <- c("cancer", "cvd", "diabetes", "crd")                           # ncd4 (SDG 3.4)
show    <- c("cancer", "cvd", "ihd", "stroke", "external", "ill_defined", "diabetes", "crd", "copd", "lung", "ncd4")
dc      <- paste0("Deaths", 1:26)
areas   <- fread(here::here("data", "dict", "areas.csv"), na.strings = "")
ctry    <- areas[, .(who_code, country = name)]

## cause groups: ";"-separated codes or ranges (A044-A059, 1027-1046, CH01-CH17; prefix3 ranges may span letters, V01-Y89)
expand <- function(x, match) unlist(lapply(strsplit(x, ";")[[1]], function(r) {
  b <- strsplit(r, "-")[[1]]
  if (length(b) == 1) return(r)
  if (match == "prefix3") {
    k <- function(z) (utf8ToInt(substr(z, 1, 1)) - 65L) * 100L + as.integer(substr(z, 2, 3))
    s <- k(b[1]):k(b[2])
    return(sprintf("%s%02d", LETTERS[s %/% 100 + 1], s %% 100))
  }
  n <- sub("^[A-Z]+", "", b)
  sprintf(paste0(sub("[0-9]+$", "", b[1]), "%0", nchar(n[1]), "d"), as.integer(n[1]):as.integer(n[2]))
}))
f_grp <- here::here("data", "dict", "who_cause_groups.csv")
grp   <- fread(f_grp, colClasses = c(list = "character", codes = "character"))
gexp  <- grp[, .(code = expand(codes, match)), by = .(id = seq_len(nrow(grp)), list, group, match, s = sign, required)]
gl    <- c(setdiff(unique(grp$group), "chapters"), "other", "oth_residual", "ncd4")
oth   <- grep("^oth_", setdiff(gl, "oth_residual"), value = TRUE)          # exclusive components of other (with oth_residual)

## ----- raw files (national level, every country), cached -----
keep <- function(d, src) {
  d <- d[fcoalesce(Admin1, "") == "" & fcoalesce(SubDiv, "") == ""]
  d[, c("Country", "Year", "Sex") := lapply(.SD, as.integer), .SDcols = c("Country", "Year", "Sex")]
  d[, (dc) := lapply(.SD, as.numeric), .SDcols = dc][, c("Country", "Year", "List", "Cause", "Sex", "Frmat", dc), with = FALSE][, src := src][]
}
fx    <- c(file.path(who_raw, who_files), file.path(who_raw, "morticd10_add", paste0(names(who_xl), ".xlsx")))
f_raw <- file.path(dir_c, sprintf("raw_national_%s.rds", digest_txt(list(unname(tools::md5sum(fx)), deparse(keep), who_xl))))
unlink(setdiff(list.files(dir_c, "^raw_national_", full.names = TRUE), f_raw))   # stale extract
if (file.exists(f_raw)) dr <- readRDS(f_raw) else {
  dr <- rbindlist(c(lapply(who_files, function(f) { message("reading ", f); keep(fread(file.path(who_raw, f), colClasses = list(character = 1:9), na.strings = ""), basename(f)) }),
                    lapply(names(who_xl), function(f) keep(as.data.table(read_excel(file.path(who_raw, "morticd10_add", paste0(f, ".xlsx")), sheet = who_xl[f], col_types = "text")), paste0(f, ".xlsx")))))
  stopifnot(!nrow(dr[, uniqueN(src), by = .(Country, Year)][V1 > 1]), !nrow(dr[, uniqueN(List), by = .(Country, Year)][V1 > 1]))
  saveRDS(dr, f_raw)
}
stopifnot(all(dr$List %chin% grp$list))

## ----- one (country, List) chunk: all-cause check, groups by age, unknown age, NA rules -----
chunk <- function(d, L) {
  d  <- age18(d[Sex %in% 1:2], "Deaths")[, unk := fcoalesce(Deaths26, 0)]
  g  <- gexp[list == L]
  cz <- unique(d$Cause)
  m  <- rbind(g[match == "exact" & code %chin% cz, .(Cause = code, group, s)],
              merge(data.table(Cause = cz, code = substr(cz, 1, 3)), g[match == "prefix3", .(code, group, s)], by = "code", allow.cartesian = TRUE)[, .(Cause, group, s)])
  ch  <- d[Cause %chin% m[group == "chapters", Cause], .(chap = sum(Deaths1, na.rm = TRUE)), by = .(Year, Sex)]
  chk <- merge(d[, .(Year, Sex, Cause, Deaths1)], ch, by = c("Year", "Sex"), all.x = TRUE)[
    , .(total = paste(Cause[which(Deaths1 == max(Deaths1, na.rm = TRUE) & Deaths1 >= fcoalesce(chap, 0))], collapse = ";"),
        chapters_equal = isTRUE(abs(max(Deaths1, na.rm = TRUE) - chap[1]) <= 0.5)), by = .(Year, Sex)]
  fmt <- d[Cause == g[group == "all", code], .(frmat = paste(sort(unique(Frmat)), collapse = "/"), src = src[1]), by = Year]
  vv  <- c(ac, "unk", "known", "total")
  x <- merge(d[, c("Cause", "Year", "Sex", ac, "unk", "known"), with = FALSE][, total := d$Deaths1], m[group != "chapters"], by = "Cause", allow.cartesian = TRUE)
  x <- x[, (vv) := lapply(.SD, `*`, s), .SDcols = vv][, lapply(.SD, sum), by = .(Year, Sex, group), .SDcols = vv]
  x <- merge(CJ(Year = unique(d$Year), Sex = 1:2, group = setdiff(gl, c("other", "oth_residual", "ncd4"))), x, by = c("Year", "Sex", "group"), all.x = TRUE)
  x[group %chin% g$group & is.na(known), (vv) := 0]                   # top-level codes absent in a year = 0 deaths
  if (!"covid" %chin% g$group) x[group == "covid" & Year < 2020, (vv) := 0]   # Lists without a COVID-19 code end before 2020
  x <- merge(x, x[group == "all", c("Year", "Sex", ac), with = FALSE], by = c("Year", "Sex"), suffixes = c("", "_all"))
  for (a in ac) set(x, which(is.na(x[[paste0(a, "_all")]])), a, NA_real_)
  x[, paste0(ac, "_all") := NULL]
  rq <- g[required == TRUE]                                             # sub-items: absent = not reported
  for (i in seq_len(nrow(rq))) x[group == rq$group[i] & !Year %in% d[Cause == rq$code[i], Year], (vv) := NA]
  x[group == "hiv" & Year < 1987, (vv) := NA]                            # no comparable HIV code before 1987
  x[group == "infectious" & ((L == "08A" & Year >= 1985) | (L %in% c("09A", "09B") & Year >= 1987 & !Year %in% d[Cause == "B184", Year])), (vv) := NA]   # HIV not separable
  ## Ireland 2000-06: B184 reported as 0 in both sexes between 18 HIV deaths at 20-59 in 1999 and 15 in 2007 (HIV coded elsewhere)
  if (L == "09B" && d$Country[1] == 4170L) x[group %chin% c("hiv", "infectious") & Year %in% 2000:2006, (vv) := NA]
  if (L %in% c("07A", "08A", "09A", "09B")) x[group %chin% c("suicide", "undetermined") & Year %in% d[Cause == "CH17" & Deaths1 > 0, Year], (vv) := NA]
  if (L %in% c("103", "104", "10M")) {
    e <- merge(d, m[group == "external"], by = "Cause")[, .(y34 = sum(Deaths1[substr(Cause, 1, 3) == "Y34"], na.rm = TRUE), ext = sum(Deaths1, na.rm = TRUE)), by = Year]
    x[group %chin% c("suicide", "undetermined", "drug_related", "alcohol_specific") & Year %in% e[ext > 0 & y34 >= 0.98 * ext, Year], (vv) := NA]
  }
  x[group != "all" & Year %in% d[src == "Germany.xlsx", Year], (vv) := NA]     # cells < 3 suppressed: all-cause only
  o <- copy(x[group %chin% c("all", core)])[group != "all", (vv) := lapply(.SD, `-`), .SDcols = vv][
    , lapply(.SD, sum), by = .(Year, Sex), .SDcols = vv]
  x <- rbind(x, o[, group := "other"], use.names = TRUE)                  # other from the raw counts (every column)
  r <- copy(x[group %chin% c("other", oth)])[group != "other", (vv) := lapply(.SD, `-`), .SDcols = vv][
    , lapply(.SD, sum), by = .(Year, Sex), .SDcols = vv]
  x <- rbind(x, r[, group := "oth_residual"], use.names = TRUE)
  rmin <- dcast(x[group %chin% c("other", "oth_residual"), .(v = suppressWarnings(min(unlist(.SD), na.rm = TRUE))), by = .(Year, Sex, group), .SDcols = c(ac, "unk", "total")
                  ][, group := paste0(group, "_raw_min")], Year + Sex ~ group, value.var = "v")
  x[, (ac) := lapply(.SD, `*`, fifelse(known > 0, 1 + unk / known, fifelse(unk == 0, 1, NA_real_))), .SDcols = ac]   # unknown age, by group
  x[group == "dementia", (ac[1:12]) := NA_real_]                        # dementia: ages 60+ only
  ro <- merge(x[group == "other", c("Year", "Sex", ac), with = FALSE], x[group %chin% c(oth, "oth_residual"), lapply(.SD, sum), by = .(Year, Sex), .SDcols = ac],
              by = c("Year", "Sex"), suffixes = c("", ".s"))
  for (a in ac) set(ro, j = a, value = fifelse(ro[[paste0(a, ".s")]] > 0, ro[[a]] / ro[[paste0(a, ".s")]], 1))
  i  <- x[, which(group %chin% c(oth, "oth_residual"))]
  f  <- ro[x[i, .(Year, Sex)], on = c("Year", "Sex")]
  for (a in ac) set(x, i, a, x[[a]][i] * f[[a]])                         # oth_* scaled to other by age (sum = other)
  pt <- x[group %chin% c(core, "other"), lapply(.SD, sum), by = .(Year, Sex), .SDcols = ac]
  al <- merge(x[group == "all"], pt, by = c("Year", "Sex"), suffixes = c("", ".p"))           # all-cause by age = sum of its parts
  for (a in ac) { p <- al[[paste0(a, ".p")]]; set(al, which(!is.na(p)), a, p[!is.na(p)]) }
  x <- rbind(x[group != "all"], al[, names(x), with = FALSE])[, `:=`(who_code = d$Country[1], list = L)]
  n <- x[group %chin% n4, c(lapply(.SD, sum), list(group = "ncd4")), by = .(Year, Sex, who_code, list), .SDcols = c(ac, "total")]
  x <- rbind(x, n, fill = TRUE)                                          # ncd4 = cancer + cvd + diabetes + crd (NA if any is NA)
  setnames(x, c("Year", "Sex"), c("year", "sex"))
  list(age = melt(x[, c("who_code", "list", "year", "sex", "group", ac), with = FALSE], measure.vars = ac, variable.name = "age", value.name = "deaths")[, age := as.integer(sub("a", "", age))],
       tot = x[, .(who_code, list, year, sex, group, total)],
       chk = chk[, .(who_code = d$Country[1], list = L, year = Year, sex = Sex, total, chapters_equal)],
       rmin = rmin[, .(who_code = d$Country[1], year = Year, sex = Sex, other_raw_min, oth_residual_raw_min)],
       fmt = fmt[, .(who_code = d$Country[1], year = Year, frmat)])
}
## chunk key: md5 of the expanded cause groups (dictionary and expand()) and of the chunk code (chunk(), age18() and the
## objects they use), so that edits after this point do not force a rebuild
key <- digest_txt(list(as.list(gexp), deparse(chunk), deparse(age18), core, n4, gl, oth, ac))
unlink(grep(key, list.files(dir_c, "^chunk_", full.names = TRUE), value = TRUE, invert = TRUE))   # stale chunks
cl  <- unique(dr[, .(Country, List)])[order(Country, List)]
res <- lapply(seq_len(nrow(cl)), function(i) {
  w <- cl$Country[i]; L <- cl$List[i]
  f <- file.path(dir_c, sprintf("chunk_%d_%s_%s.rds", w, L, key))
  if (file.exists(f)) return(readRDS(f))
  message(sprintf("%3d/%d %d %s List %s %s", i, nrow(cl), w, ctry[who_code == w, country], L, paste(range(dr[Country == w & List == L, Year]), collapse = "-")))
  r <- chunk(dr[Country == w & List == L], L)
  saveRDS(r, f); r
})
pick <- function(k) rbindlist(lapply(res, `[[`, k), fill = TRUE)

## ----- all-cause code per List: empirical (largest and >= the chapter codes) vs documented -----
tc <- merge(pick("chk"), grp[group == "all", .(list, doc = codes)], by = "list")
tc[, ok := mapply(function(t, d) d %chin% strsplit(t, ";")[[1]], total, doc)]
cat("All-cause code: documented vs empirical, (country, year, sex) cells\n")
print(tc[, .(doc = doc[1], empirical = names(which.max(table(unlist(strsplit(total, ";"))))), cells = .N, cells_ok = sum(ok)), by = list])
bad <- tc[ok == FALSE]
if (nrow(bad)) {
  cat("Cells where the documented all-cause code is not the largest code (set to NA):\n")
  print(merge(bad, ctry, by = "who_code")[, .(sexes = paste(sex, collapse = ","), empirical = total[1]), by = .(who_code, country, list, year)])
}

## ----- interior missing years, population, composites, save -----
dt  <- pick("age")[, interpolated := FALSE]
tot <- pick("tot")
if (nrow(bad)) { dt <- dt[!bad, on = c("who_code", "year", "sex")]; tot <- tot[!bad, on = c("who_code", "year", "sex")] }
stopifnot(!anyDuplicated(dt, by = c("who_code", "year", "sex", "age", "group")))
cyl <- unique(dt[, .(who_code, year, list)])
ip  <- interp(dt, "who_code", c("sex", "age", "group"), "deaths")
ip  <- merge(merge(ip, cyl[, .(who_code, lo = year, l0 = list)], by = c("who_code", "lo")), cyl[, .(who_code, hi = year, l1 = list)], by = c("who_code", "hi"))
dt  <- rbind(dt, ip[, .(who_code, list = fifelse(l0 == l1, l0, paste(l0, l1, sep = "/")), year, sex, group, age, deaths, interpolated = TRUE)], use.names = TRUE)
iy  <- unique(ip[, .(who_code, year, lo, hi)])                          # bounding observed years of the interpolated years

pw <- fread(file.path(who_raw, "mort_pop", "pop"), colClasses = list(character = c("Admin1", "SubDiv", "Frmat")), na.strings = "")
pw <- age18(pw[fcoalesce(Admin1, "") == "" & fcoalesce(SubDiv, "") == "" & Sex %in% 1:2], "Pop")   # Pop26 (age unknown) dropped
pw <- melt(pw[, c("Country", "Year", "Sex", ac), with = FALSE], measure.vars = ac, variable.name = "age", value.name = "py")[, age := as.integer(sub("a", "", age))]
setnames(pw, c("Country", "Year", "Sex"), c("who_code", "year", "sex"))
pop <- rbind(pw[, py_source := "WHO"], interp(pw, "who_code", c("sex", "age"), "py")[, .(who_code, year, sex, age, py, py_source = "WHO (interpolated)")])
## WPP where a country-year with deaths has no WHO person-years (year missing, or ages merged): WHO/WPP ratio by sex and
## age, mean over the 3 nearest WHO years on each side (rb: this and 2 earlier, ra: this and 2 later) rolled to the
## gap; ratio 1 for countries without any WHO population
un <- merge(areas[!is.na(m49_code), .(who_code, m49_code)], readRDS(wpp_rds)[, .(m49_code = as.integer(country_code), year = as.integer(year), sex = as.integer(sex),
                                                                                  age = as.integer(age), un = py)], by = "m49_code")[, m49_code := NULL]
rt <- merge(pw[!is.na(py)], un, by = c("who_code", "year", "sex", "age"))[order(who_code, sex, age, year)][, r := py / un]
rt[, `:=`(rb = frollmean(r, pmin(seq_len(.N), 3L), adaptive = TRUE), ra = rev(frollmean(rev(r), pmin(seq_len(.N), 3L), adaptive = TRUE))), by = .(who_code, sex, age)]
pop <- pop[!is.na(py)]
fl <- un[unique(dt[, .(who_code, year)]), on = c("who_code", "year"), nomatch = NULL][!pop, on = c("who_code", "year", "sex", "age")]
fl[, c("lo", "r0") := rt[fl, .(x.year, rb), on = c("who_code", "sex", "age", "year"), roll = Inf]]
fl[, c("hi", "r1") := rt[fl, .(x.year, ra), on = c("who_code", "sex", "age", "year"), roll = -Inf]]
fl[, `:=`(ratio = fcase(!is.na(lo) & !is.na(hi), r0 + (year - lo) / (hi - lo) * (r1 - r0), !is.na(lo), r0, !is.na(hi), r1, !who_code %in% rt$who_code, 1),
          py_source = fifelse(who_code %in% rt$who_code, "WPP calibrated", "WPP"))][, py := un * ratio]
pop <- rbind(pop, fl[!is.na(py), names(pop), with = FALSE])
stopifnot(!anyDuplicated(pop, by = c("who_code", "year", "sex", "age")))
dt  <- merge(dt, pop, by = c("who_code", "year", "sex", "age"), all.x = TRUE)[, composite := FALSE]

cm <- areas[!is.na(composite)]
for (i in seq_len(nrow(cm))) {
  p <- as.integer(strsplit(cm$composite[i], "+", fixed = TRUE)[[1]]); y <- as.integer(strsplit(cm$composite_years[i], "-")[[1]]); y <- seq(y[1], y[length(y)])
  x <- dt[who_code %in% p & year %in% y][, np := uniqueN(who_code), by = year][np == length(p)]
  x <- x[, .(deaths = sum(deaths), py = sum(py), list = paste(sort(unique(list)), collapse = "+"), interpolated = any(interpolated),
             py_source = paste(sort(unique(py_source)), collapse = "+")), by = .(year, sex, age, group)]
  stopifnot(!nrow(dt[who_code == cm$who_code[i] & year %in% x$year & interpolated == FALSE]))   # replaces interpolated years only
  dt <- rbind(dt[!(who_code == cm$who_code[i] & year %in% x$year)], x[, `:=`(who_code = cm$who_code[i], composite = TRUE)], use.names = TRUE)
  tx <- tot[who_code %in% p & year %in% x$year][, np := uniqueN(who_code), by = year][np == length(p)]   # all-ages totals: observed parts only
  tot <- rbind(tot, tx[, .(total = sum(total), list = paste(sort(unique(list)), collapse = "+")), by = .(year, sex, group)][, who_code := cm$who_code[i]], use.names = TRUE)
}
dt <- merge(dt, ctry, by = "who_code")
dt <- dt[order(who_code, year, sex, match(group, gl), age), .(who_code, country, year, sex, age, group, deaths, py, list, interpolated, composite, py_source)]
saveRDS(dt, file.path(dir_d, "who_deaths.rds"))

## ----- population sources (country-years with deaths: sources at ages 30-69 and at all ages) and WPP seams (the WHO
## year next to WPP years, by sex and age) -----
ids <- areas[, .(who_code, iso3, m49_code, country = name)]
ps  <- dt[group == "all", .(py_source_3069 = src_join(py_source[!is.na(py) & age %in% a3069]), py_source_all = src_join(py_source[!is.na(py)])), by = .(who_code, year)]
ps  <- merge(ids, ps[, .(n_years = .N, years = runs_txt(year)), by = .(who_code, py_source_3069, py_source_all)], by = "who_code")[order(who_code, py_source_3069, py_source_all)]
sm  <- rbind(fl[!is.na(lo), .(side = "after", year_wpp = min(year), ratio = r0[1]), by = .(who_code, sex, age, year_who = lo)],
             fl[!is.na(hi), .(side = "before", year_wpp = max(year), ratio = r1[1]), by = .(who_code, sex, age, year_who = hi)])
sm[rt, on = .(who_code, sex, age, year_who = year), `:=`(py_who = i.py, un_who = i.un)][fl, on = .(who_code, sex, age, year_wpp = year), `:=`(py_wpp = i.py, un_wpp = i.un)]
sm  <- merge(ids, sm, by = "who_code")[order(who_code, year_who, side, sex, age)]
sm[, `:=`(ratio_who = py_who / un_who, ratio_wpp = py_wpp / un_wpp)][, `:=`(step = ratio_wpp / ratio_who - 1, flag = abs(ratio - 1) > 0.1)]
fwrite(ps, file.path(dir_out, "population_sources.csv"))
smo <- sm[, .(who_code, iso3, m49_code, country, side, year_who, year_wpp, sex, age, ratio, ratio_who, ratio_wpp, step, flag)]
fwrite(smo, file.path(dir_out, "population_seams.csv"))

## ----- checks by (country, year, sex): ill-defined shares, groups sum to all-cause, raw disjointness -----
w   <- c(all = 1, setNames(rep(-1, length(core) + 1), c(core, "other")))
ck  <- rbind(dt[group %chin% names(w), .(d = sum(deaths * w[group]), o = deaths[group == "other"]), by = .(who_code, year, sex, age)],
             tot[group %chin% names(w), .(age = 0L, d = sum(total * w[group]), o = total[group == "other"]), by = .(who_code, year, sex)], use.names = TRUE)
ck  <- ck[, .(sum_diff = if (all(is.na(d))) NA_real_ else max(abs(d), na.rm = TRUE), other_min = if (all(is.na(o))) NA_real_ else min(o, na.rm = TRUE)), by = .(who_code, year, sex)]
rmn <- pick("rmin")[, .(who_code, year, sex, other_raw_min = fifelse(is.finite(other_raw_min), other_raw_min, NA_real_))]
cx  <- unique(pick("chk")[, .(who_code, year, sex, chapters_equal)])
iy  <- iy[unique(dt[interpolated == TRUE & composite == FALSE, .(who_code, year)]), on = c("who_code", "year"), nomatch = 0]
bound <- function(x, v, f) {                                            # interpolated years: worst case of the bounding observed years
  b <- merge(merge(iy, setnames(copy(x), c("year", v), c("lo", "v0")), by = c("who_code", "lo"), allow.cartesian = TRUE),
             setnames(copy(x), c("year", v), c("hi", "v1")), by = c("who_code", "hi", "sex"))
  rbind(x, setnames(b[, .(who_code, year, sex, f(v0, v1))], "V4", v), use.names = TRUE)
}
rmn <- bound(rmn, "other_raw_min", pmin)
cx  <- bound(cx, "chapters_equal", `&`)
for (i in seq_len(nrow(cm))) {                                          # composites: worst case over the parts
  p <- as.integer(strsplit(cm$composite[i], "+", fixed = TRUE)[[1]]); yc <- dt[who_code == cm$who_code[i] & composite == TRUE, unique(year)]
  rmn <- rbind(rmn, rmn[who_code %in% p & year %in% yc, .(who_code = cm$who_code[i], other_raw_min = min(other_raw_min)), by = .(year, sex)], use.names = TRUE)
  cx <- rbind(cx, cx[who_code %in% p & year %in% yc, .(who_code = cm$who_code[i], chapters_equal = all(chapters_equal)), by = .(year, sex)], use.names = TRUE)
}
ill <- dt[age %in% a3069 & group %chin% c("all", "ill_defined"), .(d = sum(deaths)), by = .(who_code, year, sex, group)]
ill <- dcast(rbind(ill, ill[, .(sex = 0L, d = sum(d)), by = .(who_code, year, group)]), who_code + year + sex ~ group, value.var = "d")[, ill_share_3069 := ill_defined / all]
cys <- merge(merge(merge(ck, rmn, by = c("who_code", "year", "sex"), all.x = TRUE), cx, by = c("who_code", "year", "sex"), all.x = TRUE),
             ill[sex > 0, .(who_code, year, sex, ill_share_3069)], by = c("who_code", "year", "sex"), all = TRUE)
cys <- rbind(cys, ill[sex == 0, .(who_code, year, sex, ill_share_3069)], fill = TRUE)
cys[, check := sum_diff < 1e-6 & other_min >= -1e-9 & other_raw_min >= 0]
cys <- merge(cys, merge(unique(dt[, .(who_code, country, year, list, interpolated, composite)]), pick("fmt"), by = c("who_code", "year"), all.x = TRUE), by = c("who_code", "year"))
saveRDS(cys[order(who_code, year, sex)], file.path(dir_d, "who_checks.rds"))

## ----- List transitions: last year of the old List vs first year of the new one, both sexes, ages 30-69 -----
yl <- unique(dt[interpolated == FALSE & composite == FALSE, .(who_code, year, list)])[order(who_code, year)]
tr <- yl[, .(list_old = shift(list), year_old = shift(year), list_new = list, year_new = year), by = who_code][!is.na(list_old) & list_old != list_new]
m4 <- dt[age %in% a3069 & composite == FALSE, .(deaths = sum(deaths), py = sum(py)), by = .(who_code, year, group)]
m4 <- merge(m4, m4[group == "all", .(who_code, year, all = deaths)], by = c("who_code", "year"))[, `:=`(share = deaths / all, rate = 1e5 * deaths / py)]
ta <- tot[, .(total = sum(total)), by = .(who_code, year, group)]
m4 <- merge(m4, merge(ta, ta[group == "all", .(who_code, year, all = total)], by = c("who_code", "year"))[, .(who_code, year, group, share_all_ages = total / all)],
            by = c("who_code", "year", "group"), all.x = TRUE)[, c("py", "all") := NULL]
tj <- merge(merge(tr, m4, by.x = c("who_code", "year_old"), by.y = c("who_code", "year")), m4,
            by.x = c("who_code", "year_new", "group"), by.y = c("who_code", "year", "group"), suffixes = c("_old", "_new"))
tj[, `:=`(jump_share = share_new / share_old - 1, jump_count = deaths_new / deaths_old - 1, jump_rate = rate_new / rate_old - 1)]
tj[, `:=`(flag = abs(jump_share) > 0.15, count_jump = abs(jump_count) > 0.15)]
tj <- merge(ctry, tj, by = "who_code")[order(who_code, year_old, match(group, gl)),
  .(who_code, country, list_old, list_new, year_old, year_new, group, deaths_old, deaths_new, rate_old, rate_new, share_old, share_new,
    share_all_ages_old, share_all_ages_new, jump_share, jump_count, jump_rate, flag, count_jump)]
fwrite(tj, file.path(dir_out, "list_transitions.csv"))

## ----- non-exclusive groups: codes shared by two groups of a List (same code, a 4-character code in a 3-character one, a
## sub-item in its parent item: PDF p.15, 22, 30, 38, 43-46); 'other' = codes in none of cancer, cvd, external, ill_defined, covid -----
par <- data.table(list = rep(c("07A", "08A", "09A", "09B", "09N", "101", "UE1"), c(2, 2, 2, 2, 4, 3, 5)),
                  code = c("1550", "156", "155", "1978", "1551", "1552", "1551", "1552", "B27", "B29", "B54", "B101", "1067", "1069", "1101", "UE13", "UE34", "UE36", "UE63", "UE65"),
                  a = c("A057", "A057", "A058", "A058", "B09", "B09", "B09", "B09", "CH07", "CH07", "CH17", "S08", "1064", "1064", "1095", "UE07", "CH09", "CH09", "CH20", "CH20"))
rng <- function(x) {                                                    # consecutive codes as ranges
  d <- data.table(x = unique(x))[, `:=`(p = sub("[0-9]+$", "", x), n = sub("^[A-Z]*", "", x))][, `:=`(w = nchar(n), i = suppressWarnings(as.integer(n)))]
  d <- d[order(p, w, i)][, b := cumsum(c(TRUE, !(diff(i) %in% 1L) | p[-1] != p[-.N] | w[-1] != w[-.N]))]
  paste(sort(d[, if (.N > 1) paste(x[1], x[.N], sep = "-") else x, by = b]$V1), collapse = "; ")
}
ov <- rbindlist(lapply(unique(grp$list), function(L) {
  g  <- gexp[list == L & !group %chin% c("all", "chapters"), .(s = sum(s)), by = .(group, code)]
  an <- unique(rbind(g[s > 0, .(code, a = code)], g[s > 0 & nchar(code) == 4, .(code, a = substr(code, 1, 3))], par[list == L & code %chin% g$code, .(code, a)]))
  mb <- merge(an, g[s > 0, .(a = code, group)], by = "a")[!merge(an, g[s < 0, .(a = code, group)], by = "a"), on = c("code", "group")]
  mb <- unique(mb[, .(code, group)])
  mb <- rbind(mb, data.table(code = setdiff(mb$code, mb[group %chin% core, code]), group = "other"))
  p  <- merge(mb, mb, by = "code", allow.cartesian = TRUE)[match(group.x, gl) < match(group.y, gl)]
  p  <- p[!merge(an[a != code], p, by.x = "a", by.y = "code")[, .(code, group.x, group.y)], on = c("code", "group.x", "group.y")]   # parent listed: drop sub-items
  p[, .(n_codes = .N, codes = rng(code)), by = .(group_1 = group.x, group_2 = group.y)][, list := L]
}))
ov <- ov[order(match(list, unique(grp$list)), match(group_1, gl), match(group_2, gl)), .(list, group_1, group_2, n_codes, codes)]
stopifnot(!nrow(ov[group_1 %chin% c(core, oth) & group_2 %chin% c(core, oth)]),   # partition and oth_* share no code
          !nrow(ov[group_1 %chin% n4 & group_2 %chin% n4]))                         # ncd4 parts are mutually exclusive in every List
fwrite(ov, file.path(dir_out, "group_overlap.csv"))

## ----- respiratory diagnostics (NOTES items 10-13): observed country-years, ages 30-69, both sexes; ICD-10 detailed Lists
## (3-character codes) and ICD-9 09A/09B country-years reporting B324 or B325 -----
rd <- age18(dr[Sex %in% 1:2 & ((List %chin% c("103", "104", "10M") & Cause %like% "^J") | (List %chin% c("09A", "09B") & Cause %chin% c("B323", "B324", "B325")))], "Deaths")
rd <- rd[, .(d = sum(rowSums(.SD))), .SDcols = ac[a3069], by = .(who_code = Country, year = Year, list = List, code = fifelse(List %like% "^09", Cause, substr(Cause, 1, 3)))]
rd <- rd[, if (!anyNA(d) && (list[1] %like% "^10" || any(code %chin% c("B324", "B325")))) .SD, by = .(who_code, year, list)]
jc <- function(a, b) sprintf("J%02d", a:b)
rsh <- function(nm, num, den) rd[, .(measure = nm, numerator = sum(d[code %chin% num]), denominator = sum(d[code %chin% den])), by = .(who_code, year, list)]
rc <- rbind(rsh("J30-J39 / J30-J98", jc(30, 39), jc(30, 98)), rsh("J00-J06 / J30-J98", jc(0, 6), jc(30, 98)), rsh("J45-J47 / J40-J47", jc(45, 47), jc(40, 47)),
            rsh("B324-B325 / B323-B325 (ICD-9 494-496 / 490-496)", c("B324", "B325"), c("B323", "B324", "B325")))[denominator > 0][, share := numerator / denominator]
rc <- merge(areas[, .(who_code, iso3, country = name)], rc, by = "who_code")[order(measure, who_code, year), .(who_code, iso3, country, list, year, measure, numerator, denominator, share)]
fwrite(rc, file.path(dir_out, "respiratory_checks.csv"))

## ----- dictionaries -----
dict_update(dir_d, "03_who_deaths.R", source = pm_src, data.table(
  file = c(rep("who_deaths.rds", 12), rep("who_checks.rds", ncol(cys))),
  variable = c(names(dt), names(cys)),
  description = c(
    "WHO country code (data/dict/areas.csv)", "WHO name", "Calendar year", "1 = male, 2 = female", "5-year age group (1 = 0-4, ..., 17 = 80-84, 18 = 85+)",
    paste("Cause group (data/dict/who_cause_groups.csv). Mutually exclusive, adding up to all: cancer, cvd, external, ill_defined, covid (0 before 2020), other.",
          "ncd4 = cancer + cvd + diabetes + crd (NA if any is NA). Not mutually exclusive (output/data_build/group_overlap.csv): lung within cancer; ihd, stroke within cvd;",
          "crd, copd, copd44 (J40-J44, ICD-10 detailed Lists only) within other; and the Eu40_APC groups (cancer_excl_c44, liver, suicide, undetermined, alcohol_specific,",
          "drug_related, hiv, liver_disease, diabetes, copd, dementia, infectious, oth_*)"),
    paste("Deaths, unknown age redistributed proportionally within each group; NA where the group is not isolable in the List or a required sub-item is not reported,",
          "where ages are merged by Frmat (03/04 75+, 05 70+, 06 65+, 07/08/09, Norway workbook 0-14), Germany 2021-22 except all"),
    paste("Person-years: WHO mid-year population; cells without it (year missing or ages merged): UN WPP calibrated to WHO (py_source; NOTES item 84);",
          "NA where neither exists (codes not in WPP: Hong Kong, Macao, Taiwan, Netherlands Antilles, former entities, parts)"),
    "WHO List (old/new: interpolated across a List change; a+b: composite)",
    "Year missing from the WHO series (gap of 1-3 years), interpolated linearly (composites: any part interpolated)",
    sprintf("Composite year (%s)", paste(sprintf("%s %s = %s", cm$name, cm$composite_years, gsub("+", " + ", cm$composite, fixed = TRUE)), collapse = "; ")),
    paste("Source of py: WHO; WHO (interpolated, gap of 1-3 years); WPP calibrated (no WHO value for the year, sex and age: UN WPP x WHO/WPP ratio by sex and age, mean of",
          "the 3 nearest WHO years on each side, held constant beyond the WHO years or interpolated linearly between two seams); WPP (uncalibrated: no WHO population",
          "for the country); '+'-joined for composites"),
    "WHO country code", "Year", "Sex (0 = both sexes, ill_share_3069 only)",
    "Largest |all - (cancer + cvd + external + ill_defined + covid + other)| over ages and the all-ages total (NA: groups not available)",
    "Smallest 'other' over ages and the all-ages total",
    "Smallest all - (cancer + cvd + external + ill_defined + covid) in the raw counts, any age column or total (< 0: overlapping codes)",
    "The all-cause code equals the sum of the List's mutually exclusive top-level codes", "Ill-defined (R00-R99 and equivalents) share of all deaths at 30-69",
    "sum_diff < 1e-6, other_min >= 0 and other_raw_min >= 0", "WHO name", "WHO List", "Interpolated year", "Composite year", "Frmat of the all-cause rows (observed years)"),
  unit = c("code", "text", "calendar year", "code", "code", "text", "deaths", "person-years", "code", "logical", "logical", "text",
           "code", "calendar year", "code", "deaths", "deaths", "deaths", "logical", "proportion", "logical", "text", "code", "logical", "logical", "code")))
dict_update(dir_out, "03_who_deaths.R", source = pm_src, data.table(
  file = c(rep("list_transitions.csv", ncol(tj)), rep("group_overlap.csv", ncol(ov)), rep("respiratory_checks.csv", ncol(rc)), rep("population_sources.csv", ncol(ps)),
           rep("population_seams.csv", ncol(smo))),
  variable = c(names(tj), names(ov), names(rc), names(ps), names(smo)),
  description = c("WHO country code", "WHO name", "List before the change", "List after the change", "Last year of the old List",
    "First year of the new List", "Cause group", "Deaths 30-69, both sexes, old year", "Deaths 30-69, new year",
    "Rate 30-69 per 100,000, old year", "Rate 30-69 per 100,000, new year", "Share of all deaths 30-69, old year", "Share of all deaths 30-69, new year",
    "Share of all deaths at all ages, old year", "Share of all deaths at all ages, new year", "Relative change in the 30-69 share",
    "Relative change in the 30-69 count", "Relative change in the 30-69 rate", "|jump_share| > 15%", "|jump_count| > 15% (the count also jumps)",
    "WHO List", "First group (order of who_deaths.rds)", "Second group (other: codes in none of cancer, cvd, external, ill_defined, covid)",
    "Number of shared codes", "Shared codes (as in who_cause_groups.csv; 4-character codes and sub-items where only they are shared; consecutive codes as ranges)",
    "WHO country code", "ISO3", "WHO name", "WHO List (ICD-10 detailed 103, 104, 10M; ICD-9 09A, 09B where B324 or B325 is reported)", "Year (observed)",
    "Numerator / denominator (ICD-10 3-character codes; ICD-9 Basic Tabulation List items)", "Deaths in the numerator, ages 30-69, both sexes",
    "Deaths in the denominator, ages 30-69, both sexes", "numerator / denominator",
    "WHO country code", "ISO3", "UN M49 numeric country code (UN WPP code)", "WHO name",
    "Sources of person-years at 30-69 (who_deaths.rds py_source, '+'-joined; none: no person-years)", "Sources of person-years at all ages ('+'-joined)",
    "Country-years with deaths with these sources", "Those years as runs",
    "WHO country code", "ISO3", "UN M49 numeric country code (UN WPP code)", "WHO name",
    "after: WPP years follow the WHO years; before: WPP years precede them", "WHO year at the seam (last before or first after the WPP years)", "Adjacent WPP year",
    "1 = male, 2 = female", "5-year age group (1 = 0-4, ..., 18 = 85+)", "Calibration ratio WHO/WPP: mean over the 3 nearest WHO years on this side (up to 3)",
    "WHO/WPP person-years in year_who", "Calibrated/WPP person-years in year_wpp", "ratio_wpp / ratio_who - 1: step at the seam beyond WPP's own change", "ratio outside 0.9-1.1"),
  unit = c("code", "text", "code", "code", "calendar year", "calendar year", "text", "deaths", "deaths", "per 100,000 person-years", "per 100,000 person-years",
           "proportion", "proportion", "proportion", "proportion", "relative change", "relative change", "relative change", "logical", "logical",
           "code", "text", "text", "count", "WHO cause codes",
           "code", "code", "text", "code", "calendar year", "text", "deaths", "deaths", "proportion",
           "code", "code", "code", "text", "text", "text", "count of years", "calendar years",
           "code", "code", "code", "text", "text", "calendar year", "calendar year", "code", "code", "ratio", "ratio", "ratio", "relative change", "logical")))

## ----- acceptance checks -----
k <- cys[sex > 0 & !is.na(sum_diff)]
cat(sprintf("\nGroups sum to all-cause (cancer + cvd + external + ill_defined + covid + other): %d of %d (country, year, sex) with groups available (%.2f%%)\n",
            k[, sum(sum_diff < 1e-6)], nrow(k), 100 * k[, mean(sum_diff < 1e-6)]))
cat(sprintf("Raw counts: cancer + cvd + external + ill_defined + covid <= all in %d of %d (country, year, sex); chapter codes = all-cause code in %d of %d\n",
            k[, sum(other_raw_min >= 0, na.rm = TRUE)], k[!is.na(other_raw_min), .N], cys[sex > 0, sum(chapters_equal, na.rm = TRUE)], cys[sex > 0 & !is.na(chapters_equal), .N]))
cat("Top-level codes do not add up to the all-cause code (observed years; the unreported items fall in 'other'; unusable in Step 4):\n")
print(unique(cys[sex > 0 & chapters_equal == FALSE & interpolated == FALSE & composite == FALSE, .(who_code, country, list, year)])[, .(years = runs_txt(year)), by = .(who_code, country, list)])
if (k[other_raw_min < 0, .N]) print(merge(k[other_raw_min < 0], ctry, by = c("who_code", "country"))[, .(n = .N, years = runs_txt(year), min = min(other_raw_min)), by = .(who_code, country, list)])
cat(sprintf("Negative deaths in any group: %d cells (smallest %.2g)\n", dt[deaths < -1e-9, .N], if (dt[deaths < -1e-9, .N]) dt[deaths < -1e-9, min(deaths)] else 0))
cat(sprintf("Interpolated country-years: %d; composite country-years: %d; country-years with deaths but without person-years at 30-69: %d\n",
            uniqueN(dt[interpolated == TRUE, .(who_code, year)]), uniqueN(dt[composite == TRUE, .(who_code, year)]),
            uniqueN(dt[age %in% a3069 & group == "all" & !is.na(deaths) & is.na(py), .(who_code, year)])))
pz <- merge(ps, areas[, .(who_code, include)], by = "who_code")
cat("\nPerson-years by source (population_sources.csv; country-years with deaths; sources at 30-69 and at all ages), included and other codes:\n")
print(pz[, .(countries = uniqueN(who_code), country_years = sum(n_years)), by = .(include, py_source_3069, py_source_all)][order(-include, py_source_3069, py_source_all)], nrows = 40)
cat("Uncalibrated WPP (no WHO population):", pz[py_source_3069 == "WPP", paste(sprintf("%s %s", iso3, years), collapse = "; ")], "\n")
cat("No person-years at 30-69:", pz[py_source_3069 == "none", paste(sprintf("%s %s", fcoalesce(iso3, as.character(who_code)), years), collapse = "; ")], "\n")
sp <- sm[age %in% a3069, .(n = .N, r_min = min(ratio), r_max = max(ratio), n_out = sum(flag), r_who = sum(py_who) / sum(un_who), r_wpp = sum(py_wpp) / sum(un_wpp)),
         by = .(who_code, iso3, country, side, year_who, year_wpp)][, step := r_wpp / r_who - 1]
sp <- merge(sp, areas[, .(who_code, include)], by = "who_code")
a1 <- sp[abs(year_wpp - year_who) == 1]
cat(sprintf("\nWPP seams (population_seams.csv): %d at 30-69 (%d of included codes); calibration ratio at 30-69 (both sexes, adjacent WPP year) %.3f-%.3f, median %.3f;",
            nrow(sp), sp[include == TRUE, .N], min(sp$r_wpp), max(sp$r_wpp), median(sp$r_wpp)))
cat(sprintf(" cells outside 0.9-1.1: %d of %d at 30-69, %d of %d at all ages. Continuity (%d seams with the WPP year next to the WHO year): |step| at 30-69 median %.4f, maximum %.4f, > 0.02 in %d\n",
            sm[age %in% a3069, sum(flag)], sm[age %in% a3069, .N], sm[, sum(flag)], nrow(sm), nrow(a1), median(abs(a1$step)), max(abs(a1$step)), a1[abs(step) > 0.02, .N]))
cat("Seams at 30-69 of included codes (r_wpp: calibrated/WPP at 30-69 in year_wpp; range of the cell ratios and cells outside 0.9-1.1; step = r_wpp / r_who - 1):\n")
print(sp[include == TRUE, .(iso3, side, year_who, year_wpp, r_wpp = round(r_wpp, 3), cells = sprintf("%.3f-%.3f", r_min, r_max), n_out = sprintf("%d/%d", n_out, n), step = round(step, 4))], nrows = 300)
icd <- function(l) fcase(l %like% "^07", "7", l %like% "^08", "8", l %like% "^09", "9", default = "10")
cat("\nList transitions, ages 30-69: flagged (share of all deaths changes by > 15%) / assessable, by ICD revision step:\n")
print(dcast(tj[group %chin% show & !is.na(jump_share), .(v = sprintf("%d/%d", sum(flag), .N)), by = .(group = factor(group, show), step = paste(icd(list_old), icd(list_new), sep = ">"))],
            group ~ step, value.var = "v", fill = ""))
cat("Median |jump_share| by group and List pair (assessable transitions with >= 100 deaths in the group at 30-69 on both sides):\n")
print(dcast(tj[group %chin% c("lung", "crd", "copd", "diabetes", "ncd4") & deaths_old >= 100 & deaths_new >= 100,
               .(v = sprintf("%.0f%% (%d/%d)", 100 * median(abs(jump_share)), sum(flag), .N)), by = .(group, pair = paste(list_old, list_new, sep = ">"))],
            pair ~ group, value.var = "v", fill = ""), nrows = 60)
cat("Flagged transitions of crd, copd, lung and ncd4 with >= 100 deaths at 30-69 on both sides (* = count also > 15%):\n")
writeLines(tj[flag == TRUE & group %chin% c("lung", "crd", "copd", "ncd4") & deaths_old >= 100 & deaths_new >= 100,
              sprintf("  %s %s>%s %d-%d: %s", country[1], list_old[1], list_new[1], year_old[1], year_new[1],
                      paste(sprintf("%s %+.0f%%%s", group, 100 * jump_share, fifelse(count_jump, "*", "")), collapse = ", ")), by = .(who_code, year_old)]$V1)
cat(sprintf("\nGroup overlaps (group_overlap.csv): %d pairs in %d Lists; exclusive groups and the ncd4 parts share no code\n", nrow(ov), uniqueN(ov$list)))
print(ov[group_1 %chin% c(n4, "lung", "copd", "copd44") | group_2 %chin% c(n4, "lung", "copd", "copd44")][!group_2 %like% "^oth_" & !group_1 %like% "^oth_", .(list, group_1, group_2, codes = substr(codes, 1, 60))], nrows = 80)
cat("\nRespiratory diagnostics (respiratory_checks.csv; observed country-years, ages 30-69, both sexes):\n")
print(rc[, .(country_years = .N, countries = uniqueN(who_code), median = median(share), p90 = quantile(share, 0.9), max = max(share),
             max_den100 = max(share[denominator >= 100])), by = measure])
cat("Pooled over years by country (>= 100 deaths in the denominator), highest shares:\n")
print(rc[, .(share = sum(numerator) / sum(denominator), denominator = sum(denominator), years = runs_txt(year)), by = .(measure, country)][denominator >= 100][
  order(measure, -share), head(.SD, 3), by = measure])
cat("B324-B325 / B323-B325 pooled over years: countries with a 09N List and the USA\n")
print(rc[measure %like% "^B324" & who_code %in% c(dr[List == "09N", unique(Country)], 2450L), .(share = sum(numerator) / sum(denominator), years = runs_txt(year)), by = country])
a9 <- dr[List == "09A"]
yn <- function(cz) paste(range(a9[Cause %chin% cz, uniqueN(Year), by = Country]$V1), collapse = "-")
cat(sprintf("09A: %d countries; B311 reported by %d (%s years each), B324 or B325 by %d (%s years each)\n", uniqueN(a9$Country),
            a9[Cause == "B311", uniqueN(Country)], yn("B311"), a9[Cause %chin% c("B324", "B325"), uniqueN(Country)], yn(c("B324", "B325"))))

## ----- consistency with Eu40_APC causes_full_history.rds (deaths; WHO person-years) -----
eu <- readRDS(file.path(eu40_root, "data", "who_mortality", "causes_full_history.rds"))
cmp <- merge(eu[, .(who_code, year, sex, age, group, deaths_eu = deaths, py_eu = py, src_eu = py_source)],
             dt[composite == FALSE, .(who_code, year, sex, age, group, deaths, py)], by = c("who_code", "year", "sex", "age", "group"))
cat(sprintf("\nEu40_APC consistency: %s cells matched (%d countries, %d groups); deaths NA in one only: %d (Eu40 NA here not: %d)\n",
            format(nrow(cmp), big.mark = ","), uniqueN(cmp$who_code), uniqueN(cmp$group), cmp[is.na(deaths_eu) != is.na(deaths), .N], cmp[is.na(deaths_eu) & !is.na(deaths), .N]))
print(cmp[!is.na(deaths_eu) & !is.na(deaths), .(cells = .N, max_abs_diff = max(abs(deaths - deaths_eu))), by = group][order(-max_abs_diff)])
print(cmp[is.na(deaths_eu) != is.na(deaths), .(cells = .N, years = runs_txt(year)), by = .(group, eu40_na = is.na(deaths_eu))])
cat(sprintf("WHO person-years (Eu40 py_source WHO): max |difference| %.3g over %s cells\n", cmp[src_eu == "WHO" & !is.na(py), max(abs(py - py_eu))], format(cmp[src_eu == "WHO" & !is.na(py), .N], big.mark = ",")))
cat(sprintf("\n%s: %s rows, %d countries, %d-%d; run time %.1f min\n", "data/derived/who_deaths.rds", format(nrow(dt), big.mark = ","),
            uniqueN(dt$who_code), min(dt$year), max(dt$year), (proc.time() - t0)[[3]] / 60))
