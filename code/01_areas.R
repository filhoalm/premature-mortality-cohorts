## Areas (Step 1): every WHO country code of the WHO Mortality Database (mort_country_codes) with name, ISO3 (countrycode
## from the name; manual fixes below), UN M49 country code, region, sub-region (the area) and intermediate region, names
## and numeric codes (countrycode un, un.region.*, un.regionsub.*, un.regionintermediate.*), national-level and
## sub-national years in the raw mortality files and national years in mort_pop, the WHO data-quality assessment (estican
## WHO_mortality_data_quality.csv, joined on ISO3), and include/exclude with the reason for sub-national, overlapping and
## former codes and countries rated Low by WHO. Composites: United Kingdom = sum of its parts in the year 4308 lacks
## (mort_notes) and Germany = FRG + GDR before 4085 starts. Territory changes (mort_notes) with the population step
## (mort_pop) in `note` and `change_years`.
## Usage (project root): Rscript code/01_areas.R
## Writes data/dict/areas.csv, data/dict/dictionary.csv

source(here::here("code", "functions_pm.R"))
library(countrycode); library(readxl)

cc <- fread(file.path(who_raw, "mort_country_codes", "country_codes"), encoding = "UTF-8")[, .(who_code = country, name)]

## ISO3 from the name; former entities get their ISO 3166-3 codes, sub-national parts NA (countrycode maps USSR to RUS and
## the GDR to DEU)
fix <- c(`1365` = NA, `1470` = "SDN", `2330` = "ANT", `2455` = "VIR", `3070` = "TWN", `3330` = NA, `4040` = "CSK", `4090` = "DDR",
         `4120` = NA, `4304` = "SUN", `4310` = NA, `4320` = NA, `4330` = NA, `4340` = "YUG", `4350` = "SCG")
cc[, iso3 := suppressWarnings(countrycode(name, "country.name", "iso3c"))]
cc[as.character(who_code) %in% names(fix), iso3 := fix[as.character(who_code)]]
m49 <- function(o) suppressWarnings(countrycode(cc$iso3, "iso3c", o))
cc[, `:=`(region = m49("un.region.name"), subregion = m49("un.regionsub.name"), intermediate = m49("un.regionintermediate.name"))]
## not in M49 (historical codes, Taiwan, parts): region of the successor or of the parent country
man <- rbindlist(list(
  list(1365L, "Africa", "Sub-Saharan Africa", "Eastern Africa"), list(2330L, "Americas", "Latin America and the Caribbean", "Caribbean"),
  list(2455L, "Americas", "Latin America and the Caribbean", "Caribbean"), list(3070L, "Asia", "Eastern Asia", NA_character_),
  list(3330L, "Asia", "Eastern Asia", NA_character_), list(4040L, "Europe", "Eastern Europe", NA_character_),
  list(4090L, "Europe", "Western Europe", NA_character_), list(4100L, "Europe", "Western Europe", NA_character_),
  list(4120L, "Europe", "Western Europe", NA_character_), list(4304L, "Europe", "Eastern Europe", NA_character_),
  list(4310L, "Europe", "Northern Europe", NA_character_), list(4320L, "Europe", "Northern Europe", NA_character_),
  list(4330L, "Europe", "Northern Europe", NA_character_), list(4340L, "Europe", "Southern Europe", NA_character_),
  list(4350L, "Europe", "Southern Europe", NA_character_)))
setnames(man, c("who_code", "r", "s", "i"))
cc <- merge(cc, man, by = "who_code", all.x = TRUE)[!is.na(s), `:=`(region = r, subregion = s, intermediate = i)][, c("r", "s", "i") := NULL]
cc[, region_source := fifelse(who_code %in% man$who_code, "manual", "countrycode")]
## M49 numeric codes: regions from their names; countries from ISO3, former entities their historical M49 codes, Taiwan
## its ISO 3166-1 numeric code (as UN WPP; not in M49)
cl <- as.data.table(codelist)
cc[, `:=`(region_code = cl$un.region.code[match(region, cl$un.region.name)], subregion_code = cl$un.regionsub.code[match(subregion, cl$un.regionsub.name)],
          intermediate_code = cl$un.regionintermediate.code[match(intermediate, cl$un.regionintermediate.name)], m49_code = m49("un"))]
m49f <- c(`1470` = 736L, `2330` = 530L, `3070` = 158L, `4040` = 200L, `4090` = 278L, `4100` = 280L, `4304` = 810L, `4340` = 890L, `4350` = 891L)
cc[as.character(who_code) %in% names(m49f), m49_code := m49f[as.character(who_code)]]

## WHO data-quality assessment (completeness, ill-defined or non-specific causes, usability = completeness x (1 - share
## ill-defined); quality High, Medium, Low)
q  <- fread(who_q, na.strings = "")
cc <- merge(cc, q[, .(iso3 = country_iso3, who_completeness = Completeness, who_ill_defined = `Percentage of ill-defined or non-specific causes to total deaths`,
                      who_usability = Usability, who_quality = quality, q_m49 = country_code)], by = "iso3", all.x = TRUE)

## years in the raw mortality files (national: Admin1 and SubDiv blank), national population totals (mort_pop), notes
yc <- c("Country", "Admin1", "SubDiv", "Year")
mr <- rbindlist(c(lapply(who_files, function(f) unique(fread(file.path(who_raw, f), select = yc, colClasses = "character", na.strings = ""))),
                  lapply(names(who_xl), function(f) unique(as.data.table(read_excel(file.path(who_raw, "morticd10_add", paste0(f, ".xlsx")), sheet = who_xl[f], col_types = "text"))[, yc, with = FALSE]))))
mr <- unique(mr[, .(who_code = as.integer(Country), year = as.integer(Year), sub = fcoalesce(Admin1, "") != "" | fcoalesce(SubDiv, "") != "")])
pt <- fread(file.path(who_raw, "mort_pop", "pop"), na.strings = "")[is.na(Admin1) & is.na(SubDiv) & Sex %in% 1:2, .(pop = sum(Pop1)), by = .(who_code = Country, year = Year)]
mn <- fread(file.path(who_raw, "mort_notes", "notes"))
yy <- function(d, nm) d[, setNames(.(.N, runs_txt(year)), paste0(nm, c("_years", "_runs"))), by = who_code]
cc <- merge(merge(cc, yy(mr[sub == FALSE], "mort"), by = "who_code", all.x = TRUE), yy(pt, "pop"), by = "who_code", all.x = TRUE)
cc <- merge(cc, mr[sub == TRUE, .(subnational_mort = runs_txt(year)), by = who_code], by = "who_code", all.x = TRUE)
cc[is.na(mort_years), mort_years := 0L][is.na(pop_years), pop_years := 0L]

yr  <- function(w) mr[who_code %in% w & sub == FALSE, sort(unique(year))]
ovl <- function(a, b) runs_txt(intersect(yr(a), yr(b)))
ny  <- function(w, p = "") mn[country == w & note %like% p, year]
pp  <- function(w, y) pt[who_code == w & year == y, pop]
stp <- function(w, y) paste(vapply(y, function(z) { z0 <- pt[who_code == w & year < z, max(year)]
  sprintf("population %+.1f%% %d to %d", 100 * (pp(w, z) / pp(w, z0) - 1), z0, z) }, ""), collapse = ", ")
uk  <- setdiff(Reduce(intersect, lapply(c(4310, 4320, 4330), yr)), yr(4308)); uk <- uk[uk > min(yr(4308)) & uk < max(yr(4308))]
de  <- c(min(yr(4090)), min(yr(4085)) - 1L)

## include/exclude: sub-national parts, former entities overlapping their successors, codes without national data
ukp <- sprintf("Part of the United Kingdom (4308 is used; 4308 %s = 4310 + 4320 + 4330)", runs_txt(uk))
ex <- c(
  `4310` = ukp, `4320` = ukp, `4330` = ukp,
  `4100` = sprintf("Former entity (FRG): summed with the GDR into Germany 4085 for %d-%d; its population includes West Berlin", de[1], de[2]),
  `4090` = sprintf("Former entity (GDR): summed with the FRG into Germany 4085 for %d-%d", de[1], de[2]),
  `4120` = sprintf("Part of the FRG 4100 (FRG population %.2f million in 1970 includes West Berlin's %.2f million)", pp(4100, 1970) / 1e6, pp(4120, 1970) / 1e6),
  `4040` = sprintf("Former entity: overlaps Czechia (4045) in %s; successor states used", ovl(4040, 4045)),
  `4304` = sprintf("Former entity: overlaps its successor states (e.g. Russian Federation 4272) in %s; successor states used", ovl(4304, 4272)),
  `4340` = sprintf("Former entity: overlaps Croatia, Slovenia and Bosnia and Herzegovina in %s; successor states used", ovl(4340, c(4038, 4276, 4025))),
  `4350` = sprintf("Former entity: overlaps Serbia (from %d) and Montenegro (from %d); successor states used", min(intersect(yr(4350), yr(4273))), min(intersect(yr(4350), yr(4207)))),
  `1470` = "Former entity (Sudan before 2011): no data",
  `1365` = sprintf("Part of Mauritius (1300 includes Rodrigues from %d, mort_notes)", min(ny(1300))),
  `3330` = sprintf("Part of Japan from 1972 (Okinawa; Japan 3160 includes it from 1972); data %s only", runs_txt(yr(3330))),
  `3068` = sprintf("No national-level data: selected urban and rural areas only (Subdiv A35/A41/A51, List 09C %s; < 10%% of deaths, PDF p.39)", runs_txt(mr[who_code == 3068 & sub == TRUE, year])),
  `3283` = "No national-level data: West Bank only (Subdiv A30)")
cc[, `:=`(include = !as.character(who_code) %in% names(ex), reason = ex[as.character(who_code)])]
cc[include == TRUE & mort_years == 0, `:=`(include = FALSE, reason = "No national-level mortality data")]
cc[include == TRUE & who_quality %in% "Low", `:=`(include = FALSE, reason = sprintf("WHO data-quality assessment Low (usability %.1f%%: completeness %.1f%%, ill-defined or non-specific causes %.1f%%)",
                                                                                     who_usability, who_completeness, who_ill_defined))]
cc[, composite := fcase(who_code == 4308L, "4310+4320+4330", who_code == 4085L, "4100+4090", default = NA_character_)]
cc[, composite_years := fcase(who_code == 4308L, runs_txt(uk), who_code == 4085L, sprintf("%d-%d", de[1], de[2]), default = NA_character_)]

## territory changes inside national series (first year of the change; population step from the previous year with data)
chg <- list(`1300` = min(ny(1300)), `3160` = 1972L, `4272` = range(ny(4272, "Exclude")) + 0:1, `4260` = ny(4260), `4084` = ny(4084), `4303` = min(ny(4303)))
sar <- "Special Administrative Region of China (China has no national-level data)"
nt <- c(`4080` = sprintf("Metropolitan France (%.2f million in 2010); overseas departments are separate codes", pp(4080, 2010) / 1e6),
        `1300` = sprintf("Includes Rodrigues (1365) from %d (mort_notes)", chg$`1300`),
        `3160` = "Includes Okinawa (Ryu Kyu Islands, 3330) from 1972",
        `4272` = sprintf("Excludes Chechnya %s (mort_notes)", runs_txt(ny(4272, "Exclude"))),
        `4260` = sprintf("Excludes Transnistria and Bender from %d (mort_notes)", chg$`4260`),
        `4084` = sprintf("Excludes Abkhazia and South Ossetia from %d (mort_notes)", chg$`4084`),
        `4303` = sprintf("Excludes non-government-controlled areas in %s (mort_notes)", runs_txt(ny(4303))),
        `4273` = sprintf("Excludes Kosovo from %d, the first year of the series (mort_notes)", ny(4273)),
        `2450` = "50 states and DC; Puerto Rico and the US Virgin Islands are separate codes", `3070` = "Not in UN M49; placed in Eastern Asia",
        `3090` = sar, `3220` = sar, `3080` = "Government-controlled areas (mort_notes)")
nt[names(chg)] <- paste0(nt[names(chg)], "; ", vapply(names(chg), function(w) stp(as.integer(w), chg[[w]]), ""))
cc[, `:=`(note = nt[as.character(who_code)], change_years = vapply(as.character(who_code), function(w) if (is.null(chg[[w]])) NA_character_ else paste(chg[[w]], collapse = ";"), ""))]
qm <- cc[q_m49 != m49_code, paste(sprintf("%s %d (%d)", iso3, who_code, m49_code), collapse = ", ")]
cc[, q_m49 := NULL]
setcolorder(cc, c("who_code", "name", "iso3", "m49_code", "region", "region_code", "subregion", "subregion_code", "intermediate", "intermediate_code", "region_source",
                  "include", "reason", "who_completeness", "who_ill_defined", "who_usability", "who_quality", "composite", "composite_years", "mort_years", "mort_runs",
                  "pop_years", "pop_runs", "subnational_mort", "note", "change_years"))
stopifnot(!anyDuplicated(cc$who_code), !anyNA(cc$subregion), !anyNA(cc$subregion_code), !nrow(cc[include == TRUE & (is.na(iso3) | is.na(m49_code))]))
fwrite(cc[order(who_code)], here::here("data", "dict", "areas.csv"))

dict_update(here::here("data", "dict"), "01_areas.R", source = paste0(who_src, ": mort_country_codes, raw mortality files, mort_pop and mort_notes; countrycode ", packageVersion("countrycode"),
                                                                      " (UN M49); WHO data-quality assessment, estican data/WHO_mortality_data_quality.csv of 2 June 2026 (read-only)"),
  data.table(file = "areas.csv", variable = names(cc), description = c(
  "WHO country code (mort_country_codes)", "WHO name", "ISO 3166-1 alpha-3 (countrycode from the name; former entities ISO 3166-3; NA for sub-national parts)",
  "UN M49 numeric country code (countrycode from ISO3; former entities their historical M49 code; Taiwan 158, ISO 3166-1 numeric as in UN WPP; NA for sub-national parts)",
  "UN M49 region", "UN M49 numeric region code", "UN M49 sub-region (the area of the area series)", "UN M49 numeric sub-region code (area_code of the area outputs)",
  "UN M49 intermediate region (NA where none)", "UN M49 numeric intermediate region code (NA where none)",
  "countrycode or manual (codes not in M49: former entities, Taiwan, parts)", "TRUE: eligible as a constituent of an area series",
  "Reason for include = FALSE",
  "WHO data-quality assessment (joined on ISO3; NA: not assessed): completeness of death registration", "WHO data-quality assessment: ill-defined or non-specific causes, share of total deaths",
  "WHO data-quality assessment: usability = completeness x (1 - ill-defined or non-specific share)", "WHO data-quality assessment: High, Medium or Low (in the file: usability >= 80%, 60-80%, < 60%); Low is not eligible; NA where not rated",
  "Codes summed to fill the composite years", "Years filled by the composite",
  "Years with national-level mortality rows (Admin1 and SubDiv blank) in the raw files (ICD-7 to ICD-10, Germany and Norway workbooks)", "Those years as runs",
  "Years with national-level population (mort_pop)", "Those years as runs", "Years with sub-national mortality rows (Admin1 or SubDiv) in the raw files",
  "Note: coverage and territory changes (mort_notes) with the step in total population (mort_pop, both sexes) from the previous year with data",
  "First years of territory changes inside the national series (';'-separated)"),
  unit = c("code", "text", "code", "code", "text", "code", "text", "code", "text", "code", "text", "logical", "text", "percent", "percent", "percent", "text",
           "code", "calendar years", "count of years", "calendar years", "count of years", "calendar years", "calendar years", "text", "calendar years")))

cat(sprintf("%d WHO codes: %d included, %d excluded; %d with national-level mortality data\n", nrow(cc), cc[include == TRUE, .N], cc[include == FALSE, .N], cc[mort_years > 0, .N]))
cat(sprintf("WHO data-quality assessment: %d codes with an assessment (joined on ISO3; %s); M49 code differs from the file's for %s\n", cc[!is.na(who_completeness), .N],
            cc[!is.na(who_completeness), .N, by = .(q = fcoalesce(who_quality, "not rated"))][, paste(q, N, collapse = ", ")], qm))
print(cc[mort_years > 0 & (who_quality %in% "Low" | (!is.na(who_completeness) & is.na(who_quality))), .(who_code, name, iso3, subregion, who_completeness, who_ill_defined, who_usability, who_quality, include)])
cat(sprintf("Included codes with mortality data not assessed by WHO: %s\n", cc[include == TRUE & mort_years > 0 & is.na(who_completeness), paste(iso3, collapse = " ")]))
cat(sprintf("Composites (population): United Kingdom %s: 4308 %s, parts %s; Germany %d: 4085 %s, FRG + GDR %s\n", runs_txt(uk),
            format(sum(pp(4308, uk)), big.mark = ","), format(pt[who_code %in% c(4310, 4320, 4330) & year %in% uk, sum(pop)], big.mark = ","),
            min(yr(4085)), format(pp(4085, min(yr(4085))), big.mark = ","), format(pt[who_code %in% c(4100, 4090) & year == min(yr(4085)), sum(pop)], big.mark = ",")))
print(cc[include == FALSE & mort_years > 0 | !is.na(composite) | !is.na(subnational_mort), .(who_code, name, iso3, subregion, include, mort = mort_runs, sub = subnational_mort, reason = substr(reason, 1, 70))], nrows = 50)
cat("\nNotes:\n"); print(cc[!is.na(note), .(who_code, name, change_years, note)])
cat("\nCodes by sub-region (included, with mortality data):\n")
print(cc[include == TRUE & mort_years > 0, .(n = .N, codes = paste(iso3, collapse = " ")), by = .(region, subregion)][order(region, subregion)])
