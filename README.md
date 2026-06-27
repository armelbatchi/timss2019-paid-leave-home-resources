# Paid parental leave, home resources, and Grade 4 mathematics

Reviewer-facing reproducibility code for:

**Does More Generous Paid Parental Leave Policy Change How Home Support Matters for Children’s Early Numeracy? Evidence From 39 Education Systems**

Authors: Armel Landry Batchi-Bouyou, Päivi Häkkinen, and Ronny Scherer.

## Repository contents

- `run_analysis.R`: runs the complete analysis and exports the manuscript tables and figures.
- `R/00_setup.R`: packages, output folders, and plotting theme.
- `R/01_helpers.R`: weighting, plausible-value pooling, country matching, and export functions.
- `R/02_external_data.R`: paid-leave harmonization and World Bank indicators.
- `R/03_timss_data.R`: TIMSS import, variable extraction, and analytic-sample construction.
- `R/04_descriptives.R`: Table 1 and Figure 1.
- `R/05_country_gradients.R`: Table 2, Table S1, Figure 2, and Figure S2.
- `R/06_meta_regression.R`: Table 3 and Figure 3.
- `R/07_supplementary.R`: Table S2 and Figure S1.
- `R/08_validation.R`: comparison of reproduced estimates with the manuscript values.

## Data access

No microdata or separate data files are distributed in this repository. The analysis uses the following official sources:

- [IEA TIMSS data repository](https://www.iea.nl/data-tools/repository/timss), including the TIMSS 2019 Grade 4 international database.
- [OECD Family Database](https://www.oecd.org/en/data/datasets/oecd-family-database.html), indicator PF2.1 on parental leave systems.
- [ILO, *Maternity and paternity at work: Law and practice across the world*](https://www.ilo.org/publications/maternity-and-paternity-work-law-and-practice-across-world).
- [World Bank World Development Indicators](https://databank.worldbank.org/source/world-development-indicators).

Download the official TIMSS 2019 Grade 4 SPSS files and provide their local directory through the `TIMSS_DIR` environment variable:

```text
TIMSS_DIR=/absolute/path/to/TIMSS2019_Grade4_SPSS
```

The harmonized paid-leave file is not included. After obtaining the underlying OECD and ILO information from the official links above, provide a local CSV with the columns `iso3c` and `leave_weeks`, and set its path through `LEAVE_DATA_FILE`:

```text
LEAVE_DATA_FILE=/absolute/path/to/harmonized_leave_weeks.csv
```

GDP per capita and female labour-force participation are retrieved directly through the World Bank API for 2018–2020 and averaged across those years. The indicators are `NY.GDP.PCAP.PP.CD` and `SL.TLF.CACT.FE.ZS`.

## Reproducing the analysis

From the repository root, run:

```r
source("run_analysis.R")
```

The workflow constructs the 39-system analytic sample, produces weighted descriptive profiles, pools the five mathematics plausible-value regressions using Rubin’s rules, estimates the random-effects meta-regression, produces the supplementary analyses, and exports all results to `output/`.

The main system models adjust for parental education, preschool years, and gender. The reduced models omit parental education. The meta-regression is the inferential test of cross-system moderation.

## Expected manuscript values

A successful reproduction should return approximately:

- 162,393 students across 39 education systems
- 37 statistically significant main-model slopes
- system slopes from 4.92 to 45.71 TIMSS points per 1 SD of home resources
- random-effects pooled slope of 34.83, SE 1.38
- paid-leave meta-regression coefficient of -0.0163, p = 0.7048

Exact comparisons are written to `output/validation_against_manuscript.csv`.

## License

Code is released under the MIT License. TIMSS data remain subject to IEA terms of use.
