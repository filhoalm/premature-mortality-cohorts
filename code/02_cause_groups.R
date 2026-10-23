## Cause groups (Step 2): copy of the Eu40_APC dictionary (data/dict/who_cause_groups.csv, read-only) extended to the lists
## used outside Europe (09A = 09B codes, Table 5; 08B, Table 4; S47 in 09A/09B) and to the SDG 3.4 groups: lung (trachea,
## bronchus and lung cancer, C33-C34), crd (chronic respiratory diseases, J30-J98), copd44 (J40-J44, ICD-10 detailed lists
## only) and copd in 09N. Every row cites the WHO documentation page (PDF page; printed page = PDF page - 2). ncd4 =
## cancer + cvd + diabetes + crd is computed in code/03_who_deaths.R. China's 09C list (Table 7) is not needed: China has
## sub-national data only (data/dict/areas.csv).
## Usage (project root): Rscript code/02_cause_groups.R
## Writes data/dict/who_cause_groups.csv, data/dict/dictionary.csv

source(here::here("code", "functions_pm.R"))
eu <- fread(file.path(eu40_root, "data", "dict", "who_cause_groups.csv"), colClasses = c(list = "character", codes = "character"))

r <- function(list, group, codes, match, sign, required, source, note = "") data.table(list, group, codes, match, sign, required, source, note)
add <- rbind(
  ## 09A: same codes as 09B (PDF p.9: '09A,09B' = ICD-9 Basic Tabulation List; Table 5)
  copy(eu[list == "09B"])[, `:=`(list = "09A", source = paste0("PDF p.9 (printed 7; 09A and 09B share Table 5); ", source))],
  r(c("09A", "09B"), "chapters", "S47", "exact", 1, FALSE, "PDF p.35 (printed 33)", "All accidental causes where B47-B52 are not broken down (Bahamas 1983)"),
  r(c("09A", "09B"), "external", "S47", "exact", 1, FALSE, "PDF p.35 (printed 33)", "Replaces B47-B52 (Bahamas 1983); disjoint from B47-B56 (B00 identity)"),
  ## 08B (Table 4, PDF p.27-28)
  r("08B", "all", "B000", "exact", 1, FALSE, "PDF p.27 (printed 25)", "All causes"),
  r("08B", "chapters", "B001-B050", "exact", 1, FALSE, "PDF p.27-28 (printed 25-26)", "Mutually exclusive B items (B018 and B046 residuals); used only to check the all-cause code"),
  r("08B", "cancer", "B019", "exact", 1, FALSE, "PDF p.27 (printed 25)", "ICD-8 140-209 (includes 208-209, as 08A)"),
  r("08B", "cvd", "B025-B030", "exact", 1, FALSE, "PDF p.27 (printed 25)", "ICD-8 390-438 only: 440-458 are in the residual B046"),
  r("08B", "ihd", "B028", "exact", 1, FALSE, "PDF p.27 (printed 25)", "ICD-8 410-414"),
  r("08B", "stroke", "B030", "exact", 1, FALSE, "PDF p.27 (printed 25)", "ICD-8 430-438"),
  r("08B", "external", "B047-B050", "exact", 1, FALSE, "PDF p.28 (printed 26)", "E800-E999"),
  r("08B", "suicide", "B049", "exact", 1, FALSE, "PDF p.28 (printed 26)", "E950-E959"),
  r("08B", "ill_defined", "B045", "exact", 1, FALSE, "PDF p.28 (printed 26)", "ICD-8 780-796"),
  r("08B", "diabetes", "B021", "exact", 1, FALSE, "PDF p.27 (printed 25)", "ICD-8 250"),
  r("08B", "copd", "B033", "exact", 1, FALSE, "PDF p.27 (printed 25)", "ICD-8 490-493, as 08A A093"),
  r("08B", "liver_disease", "B037", "exact", 1, FALSE, "PDF p.27 (printed 25)", "ICD-8 571 (cirrhosis of liver) only, as 08A A102"),
  ## lung: trachea, bronchus and lung (ICD-10 C33-C34)
  r("07A", "lung", "A050", "exact", 1, FALSE, "PDF p.15 (printed 13)", "ICD-7 162-163 (163: lung not specified as primary or secondary); 07B: inside B018 (not isolable)"),
  r("08A", "lung", "A051", "exact", 1, FALSE, "PDF p.22 (printed 20)", "ICD-8 162; 08B: inside B019 (not isolable)"),
  r(c("09A", "09B"), "lung", "B101", "exact", 1, TRUE, "PDF p.30 (printed 28)", "ICD-9 162; sub-item of B10; NA where B101 is not reported"),
  r("09N", "lung", "B101", "exact", 1, TRUE, "PDF p.30 and p.38 (printed 28 and 36)", "ICD-9 162; sub-item of S08 (CH02)"),
  r("101", "lung", "1034", "exact", 1, FALSE, "PDF p.42 (printed 40)", "C33-C34; sub-item of 1026"),
  r(c("103", "104"), "lung", "C33-C34", "prefix3", 1, FALSE, "PDF p.42 (printed 40; row 1034)", ""),
  r("10M", "lung", "C33-C34", "prefix3", 1, FALSE, "PDF p.10 and p.42 (printed 8 and 40)", "UE1: UE15 is C32-C34 (larynx included; PDF p.45), not isolable"),
  ## crd: chronic respiratory diseases (ICD-10 J30-J98)
  r("08A", "crd", "A093-A096", "exact", 1, FALSE, "PDF p.23 (printed 21)", "ICD-8 490-519 (490-493 bronchitis emphysema asthma; 500 tonsils; 510 513 empyema abscess; 501-508 511 512 514-519 other). ICD-7 is NA: asthma (241) is in A066 with endocrine disorders (PDF p.15) and 07B/08B have the rest in residuals (PDF p.19-20 and p.28)"),
  r(c("09A", "09B"), "crd", "B32", "exact", 1, FALSE, "PDF p.32-33 (printed 30-31)", "ICD-9 490-519 = B32 (466, 480-519) - B320 - B321 - B322; 470-478 (chronic upper respiratory, J30-J39) are not included: they are in B31 with acute infections and 09A does not report B311-B319"),
  r(c("09A", "09B"), "crd", "B320;B321;B322", "exact", -1, TRUE, "PDF p.32-33 (printed 30-31)", "Acute bronchitis (466), pneumonia (480-486), influenza (487) subtracted; NA where any is not reported"),
  r("09N", "crd", "B323;S329", "exact", 1, TRUE, "PDF p.33 and p.38 (printed 31 and 36)", "ICD-9 490-493 + S329 (rest of B31 and B32: B313-B315, B319, B324-B327, B329 = 470-478, 494-519); sub-items of CH08 (= S310 + B321 + B322 + B323 + S329, verified)"),
  r("101", "crd", "1076;1077", "exact", 1, FALSE, "PDF p.43 (printed 41)", "J40-J47 + J00-J06, J30-J39, J60-J98: includes J00-J06 (acute upper respiratory infections, not separable; share of J30-J98 in output/data_build/respiratory_checks.csv). UE1: not isolable (CH10 - UE38 - UE39 keeps J00-J06 and J20-J22; PDF p.45-46)"),
  r(c("103", "104"), "crd", "J30-J98", "prefix3", 1, FALSE, "PDF p.43 (printed 41; rows 1076 and 1077) and ICD-10 codes", "J99 is an asterisk code"),
  r("10M", "crd", "J30-J98", "prefix3", 1, FALSE, "PDF p.10 and p.43 (printed 8 and 41)", ""),
  ## copd44 (J40-J44): ICD-10 detailed lists only
  r(c("103", "104"), "copd44", "J40-J44", "prefix3", 1, FALSE, "PDF p.43 (printed 41; row 1076 J40-J47) and ICD-10 codes", "Not isolable elsewhere: asthma is inside 101 1076 (J40-J47), UE1 UE40 (J40-J47; UE41 = J45-J46 but J47 stays), ICD-9 B323 (490-493) and ICD-8 A093/B033 (490-493)"),
  r("10M", "copd44", "J40-J44", "prefix3", 1, FALSE, "PDF p.10 and p.43 (printed 8 and 41)", ""),
  ## copd in 09N
  r("09N", "copd", "B323", "exact", 1, TRUE, "PDF p.33 and p.38 (printed 31 and 36)", "ICD-9 490-493 only, as ICD-8 A093: 494-496 are inside S329 (approximation; validated at the 09N List transitions)")
)
grp <- rbind(eu, add)[, required := as.logical(required)]
grp <- grp[order(match(list, c("07A", "07B", "08A", "08B", "09A", "09B", "09N", "101", "103", "104", "10M", "UE1")))]
stopifnot(!anyDuplicated(grp[, .(list, group, codes, sign)]), !nrow(grp[source == "" | is.na(source)]))
fwrite(grp, here::here("data", "dict", "who_cause_groups.csv"))

dict_update(here::here("data", "dict"), "02_cause_groups.R", source = "Eu40_APC data/dict/who_cause_groups.csv (read-only copy) extended from the WHO Mortality Database documentation (February 2026 release, raw/mort_documentation)",
  data.table(file = "who_cause_groups.csv", variable = names(grp), description = c(
  "WHO List", "Group (chapters = mutually exclusive top-level codes, used only to check the all-cause code)",
  "Codes or ranges as in the Cause column, ';'-separated", "exact: Cause; prefix3: first 3 characters of Cause (3- and 4-character rows both added)",
  "+1 added, -1 subtracted", "TRUE: every code of the row must be reported in the country-year, otherwise the group is NA (sub-items of condensed lists)",
  "WHO documentation page (PDF page; printed page in brackets) or empirical correction", "Note"),
  unit = c("code", "text", "WHO cause codes (ICD-7 to ICD-10)", "text", "+1/-1", "logical", "text", "text")))
cat(sprintf("who_cause_groups.csv: %d rows (%d from Eu40_APC, %d added)\n", nrow(grp), nrow(eu), nrow(add)))
print(dcast(grp[!group %in% c("chapters")][, .N, by = .(group, list)], group ~ list, value.var = "N", fill = 0)[
  group %in% c("all", "cancer", "cvd", "ihd", "stroke", "external", "ill_defined", "covid", "diabetes", "copd", "copd44", "lung", "crd")])
