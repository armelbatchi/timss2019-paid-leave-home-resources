build_validation_summary <- function(analysis_data, main_slopes, meta_model) {
  pooled_model <- metafor::rma(
    yi = resource_slope,
    vi = se^2,
    data = main_slopes,
    method = "REML"
  )

  leave_index <- which(names(stats::coef(meta_model)) == "leave_weeks")

  tibble(
    quantity = c(
      "Student sample size",
      "Education systems in analysis",
      "Statistically significant main-model slopes",
      "Smallest main-model slope",
      "Largest main-model slope",
      "Random-effects pooled slope",
      "Random-effects pooled slope SE",
      "Leave meta-regression coefficient",
      "Leave meta-regression p value"
    ),
    observed = c(
      nrow(analysis_data),
      dplyr::n_distinct(analysis_data$iso3c),
      sum(main_slopes$p < 0.05),
      min(main_slopes$resource_slope),
      max(main_slopes$resource_slope),
      as.numeric(pooled_model$b),
      as.numeric(pooled_model$se),
      as.numeric(stats::coef(meta_model)[leave_index]),
      as.numeric(meta_model$pval[leave_index])
    ),
    manuscript_value = c(
      162393,
      39,
      37,
      4.92,
      45.71,
      34.83,
      1.38,
      -0.0163,
      0.7048
    )
  ) %>%
    mutate(difference = observed - manuscript_value)
}
