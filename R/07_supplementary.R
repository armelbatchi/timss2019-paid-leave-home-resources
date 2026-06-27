weighted_cor_matrix_pv <- function(
  data,
  non_pv_variables,
  plausible_values,
  weight_variable = "totwgt"
) {
  variables <- c("math", non_pv_variables)
  output <- matrix(
    NA_real_,
    nrow = length(variables),
    ncol = length(variables),
    dimnames = list(variables, variables)
  )

  for (variable_a in non_pv_variables) {
    for (variable_b in non_pv_variables) {
      output[variable_a, variable_b] <- weighted_cor(
        data[[variable_a]],
        data[[variable_b]],
        data[[weight_variable]]
      )
    }
  }

  for (variable in non_pv_variables) {
    correlations <- vapply(
      plausible_values,
      function(plausible_value) {
        weighted_cor(
          data[[plausible_value]],
          data[[variable]],
          data[[weight_variable]]
        )
      },
      numeric(1)
    )

    pooled <- fisher_pool_r(correlations)
    output["math", variable] <- pooled
    output[variable, "math"] <- pooled
  }

  output["math", "math"] <- 1
  output
}

build_student_correlation_matrix <- function(data) {
  weighted_cor_matrix_pv(
    data = data,
    non_pv_variables = c(
      "home_resources",
      "books",
      "parent_ed",
      "preschool_years"
    ),
    plausible_values = intersect(paste0("asmmat0", 1:5), names(data)),
    weight_variable = "totwgt"
  )
}

make_table_s2 <- function(correlation_matrix) {
  table_data <- as.data.frame(correlation_matrix) %>%
    tibble::rownames_to_column("Variable")

  table_data$Variable <- c(
    "Mathematics (PV-pooled)",
    "Home resources",
    "Books at home",
    "Parental education",
    "Preschool years"
  )

  names(table_data) <- c(
    "Variable",
    "Mathematics (PV-pooled)",
    "Home resources",
    "Books at home",
    "Parental education",
    "Preschool years"
  )

  table_gt <- table_data %>%
    gt(rowname_col = "Variable") %>%
    fmt_number(columns = where(is.numeric), decimals = 2) %>%
    tab_header(
      title = md("**Supplementary Table S2. Weighted student-level correlations among the key variables**"),
      subtitle = "Correlations involving mathematics are pooled across the five plausible values"
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    tab_source_note(
      paste(
        "TIMSS final student weights were used.",
        "Negative correlations with parental education reflect the reversed IEA coding in the input variable.",
        "ASBGHRL includes books and parental education by construction."
      )
    )

  save_table_exports(
    table_gt,
    table_data,
    "tableS2_student_level_correlation_matrix"
  )

  table_gt
}

make_figure_s1 <- function(country_profiles, slopes) {
  country_level_data <- country_profiles %>%
    select(
      iso3c,
      `Mean mathematics` = mean_math,
      `Mean home resources` = mean_home_resources,
      `Mean books at home` = mean_books,
      `Mean parental educ.` = mean_parent_ed,
      `Leave (weeks)` = leave_weeks,
      `GDP per capita` = gdp_pc,
      `Female LFP` = fem_lfp
    ) %>%
    left_join(
      slopes %>% select(iso3c, `Home-resource gradient` = resource_slope),
      by = "iso3c"
    ) %>%
    select(
      `Mean mathematics`,
      `Mean home resources`,
      `Mean books at home`,
      `Mean parental educ.`,
      `Home-resource gradient`,
      `Leave (weeks)`,
      `GDP per capita`,
      `Female LFP`
    ) %>%
    filter(if_all(everything(), ~ !is.na(.x) & is.finite(.x)))

  correlation_matrix <- stats::cor(
    country_level_data,
    use = "complete.obs",
    method = "pearson"
  )

  variable_names <- colnames(correlation_matrix)

  long_data <- as.data.frame(as.table(correlation_matrix)) %>%
    as_tibble() %>%
    transmute(
      row_variable = as.character(Var1),
      column_variable = as.character(Var2),
      correlation = as.numeric(Freq),
      row_index = match(row_variable, variable_names),
      column_index = match(column_variable, variable_names)
    ) %>%
    filter(row_index > column_index) %>%
    mutate(
      column_variable = factor(
        column_variable,
        levels = variable_names[-length(variable_names)]
      ),
      row_variable = factor(
        row_variable,
        levels = rev(variable_names[-1])
      ),
      correlation_label = sprintf("%.2f", correlation)
    )

  plot <- ggplot(
    long_data,
    aes(x = column_variable, y = row_variable, fill = correlation)
  ) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(
      aes(label = correlation_label),
      color = "black",
      fontface = "bold",
      size = 3.5
    ) +
    scale_fill_gradient2(
      low = pal$short_leave,
      mid = "#F5F5F5",
      high = pal$long_leave,
      midpoint = 0,
      limits = c(-1, 1),
      name = "Correlation"
    ) +
    coord_fixed() +
    labs(
      title = "Supplementary Figure S1. System-level correlations among the key country-level variables",
      subtitle = sprintf(
        "Strictly lower-triangular Pearson correlation matrix across %d education systems",
        nrow(country_level_data)
      ),
      x = NULL,
      y = NULL,
      caption = paste(
        "Source: TIMSS 2019 linked to OECD, ILO, and World Bank data.",
        "Each observation is one education system. Each correlation is displayed once."
      )
    ) +
    theme_paper() +
    theme(
      axis.text.x = element_text(angle = 35, hjust = 1),
      panel.grid = element_blank(),
      legend.position = "right"
    )

  save_plot_exports(
    plot,
    "figureS1_country_level_correlation_matrix",
    width = 9,
    height = 7.5
  )

  plot
}
