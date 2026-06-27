source("R/00_setup.R")
source("R/01_helpers.R")
source("R/02_external_data.R")
source("R/03_timss_data.R")
source("R/04_descriptives.R")
source("R/05_country_gradients.R")
source("R/06_meta_regression.R")
source("R/07_supplementary.R")
source("R/08_validation.R")

country_external <- build_country_external()
timss_2019 <- read_timss_2019()
raw_data <- extract_timss_variables(timss_2019)
analysis_data <- build_analysis_data(raw_data, country_external)

saveRDS(analysis_data, file.path(object_dir, "analysis_data.rds"))

country_profiles <- build_country_profiles(analysis_data)
heatmap_data <- build_heatmap_data(analysis_data)

table1 <- make_table1(country_profiles)
figure1 <- make_figure1(heatmap_data)

country_slopes_main <- build_country_slopes(analysis_data, "main")
country_slopes_reduced <- build_country_slopes(analysis_data, "reduced")

readr::write_csv(
  country_slopes_main,
  file.path(table_dir, "country_slopes_main_numeric.csv")
)
readr::write_csv(
  country_slopes_reduced,
  file.path(table_dir, "country_slopes_reduced_numeric.csv")
)

table2 <- make_slope_table(country_slopes_main, "main")
table_s1 <- make_slope_table(country_slopes_reduced, "reduced")

prediction_grid <- build_prediction_grid(analysis_data)
figure2 <- make_figure2(prediction_grid)
figure_s2 <- make_figure_s2(country_slopes_main)

meta_data <- build_meta_data(country_slopes_main, country_external)
meta_model <- fit_meta_regression(meta_data)
saveRDS(meta_model, file.path(object_dir, "meta_regression_model.rds"))
readr::write_csv(meta_data, file.path(table_dir, "meta_regression_input.csv"))

table3 <- make_table3(meta_model, meta_data)
figure3 <- make_figure3(meta_data)

student_correlations <- build_student_correlation_matrix(analysis_data)
table_s2 <- make_table_s2(student_correlations)
figure_s1 <- make_figure_s1(country_profiles, country_slopes_main)

validation <- build_validation_summary(
  analysis_data,
  country_slopes_main,
  meta_model
)
readr::write_csv(validation, file.path(output_dir, "validation_against_manuscript.csv"))

capture.output(
  sessionInfo(),
  file = file.path(output_dir, "sessionInfo.txt")
)

print(validation)
