# premature-mortality-cohorts

Premature mortality in successive birth cohorts across world regions, from WHO death registrations, in R.

The project builds harmonised series of premature deaths (ages 30-69) from the WHO Mortality Database (ICD-7 to ICD-10, 1950-2024) for 13 UN M49 sub-regions, then compares the SDG 3.4 indicator with the risk faced by real birth cohorts and follows the shift from cardiovascular disease to cancer with the New APC model ([matisseR](https://github.com/filhoalm/matisseR)). The code is published one component at a time.

**Read the documentation: [WHO Mortality Database: data harmonisation](https://filhoalm.github.io/premature-mortality-cohorts/)**

## Component 1: data quality and harmonisation

- **Raw files**: WHO Mortality Database raw files read, never edited; national rows only
- **Countries and data quality**: WHO codes mapped to ISO3 and UN M49 regions; one code per territory; countries rated Low by WHO left out
- **Ages**: every WHO age layout put into 18 five-year groups; deaths of unknown age shared out pro rata within each cause group
- **Causes**: cause groups across 12 WHO cause lists (ICD-7 to ICD-10), including the four major NCDs of SDG 3.4; checks that groups add up and never overlap
- **Data quality checks**: changes of cause list flagged (share jumps of more than 15%); usable years only (every cause separable at 30-69, population known, ill-defined causes at most 20%); gaps of 1-3 years filled linearly; no smoothing
- **Population**: WHO population, completed from UN World Population Prospects calibrated to WHO by sex and age
- **Areas**: each UN M49 sub-region sums a fixed set of countries over a fixed window

## Files

| Path | Contents |
|---|---|
| `code/functions_pm.R` | Shared helpers: paths, WHO raw files, age formats, gap filling, dictionaries |
| `code/00_session.R` | R and package versions |
| `code/01_areas.R` | Country codes, UN M49 regions, WHO data-quality rating, eligibility |
| `code/02_cause_groups.R` | Cause groups across ICD revisions and WHO cause lists |
| `code/03_who_deaths.R` | Raw files to deaths by year, sex, age and cause; checks; population |
| `code/04_area_series.R` | Usable country-years, stable country sets and windows, area series |
| `docs/index.html` | The documentation page (GitHub Pages), with the full R code in its appendix |
| `docs/who_mortality_harmonisation.Rmd`, `code/08_data.Rmd`, `code/08_who_processing.Rmd`, `code/08_helpers.R`, `code/16_docs.R` | Source of the page |
| `docs/published.csv` | List of the published files; `code/17_publish.R` copies them here |

## Running it

Requirements: R 4.4 with data.table, here, countrycode and readxl.

```powershell
Rscript code/00_session.R
Rscript code/01_areas.R
Rscript code/02_cause_groups.R
Rscript code/03_who_deaths.R
Rscript code/04_area_series.R
```

The data are not included. The scripts expect the raw files of the [WHO Mortality Database](https://www.who.int/data/data-collection-tools/who-mortality-database) in `../Eu40_APC/data/who_mortality/raw/` and the UN WPP population and WHO data-quality file in `../estican/data/`; set `eu40_root` and `estican_root` in `code/functions_pm.R` to your own folders.

## Coming next

The analyses will follow as separate components: the SDG 3.4 indicator by birth cohort, the leading premature cause by cohort, the tobacco cohort lag, the replication of the European competing-mortality analyses for world regions, and the replication of Bray et al. 2021 by world region with COVID-19.

## Good to know

- **Deaths as certified.** Ill-defined causes are not redistributed; years where they exceed 20% of deaths at 30-69 are not used.
- **Work in progress.** Results are unpublished and may change; please contact the author before citing or reusing them.
- **Licence**: [LICENSE](LICENSE) (to be confirmed): code MIT. The WHO Mortality Database and UN WPP data are not redistributed here; see their terms of use.
