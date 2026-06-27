build_meta_data <- function(slopes, country_external) {
  slopes %>%
    left_join(
      country_external %>% select(iso3c, gdp_pc, fem_lfp),
      by = "iso3c"
    ) %>%
    filter(!is.na(gdp_pc), !is.na(fem_lfp)) %>%
    mutate(
      vi = se^2,
      gdp_pc_z = as.numeric(scale(gdp_pc)),
      fem_lfp_z = as.numeric(scale(fem_lfp))
    )
}

fit_meta_regression <- function(meta_data) {
  metafor::rma(
    yi = resource_slope,
    vi = vi,
    mods = ~ leave_weeks + gdp_pc_z + fem_lfp_z,
    data = meta_data,
    method = "REML"
  )
}

make_table3 <- function(model, meta_data) {
  estimates <- as.numeric(stats::coef(model))
  standard_errors <- as.numeric(model$se)
  z_values <- as.numeric(model$zval)
  p_values <- as.numeric(model$pval)

  labels <- c(
    "Intercept",
    "Leave duration (weeks)",
    "GDP per capita (z)",
    "Female LFP (z)"
  )[seq_along(estimates)]

  table_data <- tibble(
    Term = labels,
    Estimate = round(estimates, 4),
    SE = round(standard_errors, 4),
    z = round(z_values, 3),
    p = round(p_values, 4),
    CI_lo = round(estimates - 1.96 * standard_errors, 4),
    CI_hi = round(estimates + 1.96 * standard_errors, 4)
  )

  table_gt <- table_data %>%
    gt() %>%
    tab_header(
      title = md("**Table 3. Cross-country meta-regression of the home-resource gradient on paid parental leave duration**"),
      subtitle = "Inverse-variance-weighted random-effects meta-regression (REML), adjusted for GDP per capita and female labour force participation"
    ) %>%
    tab_spanner(label = "95% CI", columns = c(CI_lo, CI_hi)) %>%
    tab_style(
      style = list(
        cell_fill(color = pal$highlight),
        cell_text(weight = "bold")
      ),
      locations = cells_body(rows = Term == "Leave duration (weeks)")
    ) %>%
    tab_source_note(
      paste0(
        "k = ", nrow(meta_data),
        " education systems with complete moderator data. I^2 = ",
        round(model$I2, 1),
        "%. tau^2 = ", round(model$tau2, 4), "."
      )
    )

  save_table_exports(
    table_gt,
    table_data,
    "table3_cross_country_meta_regression_leave_gradient"
  )

  table_gt
}

make_figure3 <- function(meta_data) {
  plot <- ggplot(meta_data, aes(x = leave_weeks, y = resource_slope)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      color = pal$long_leave,
      fill = pal$light,
      linewidth = 1.2,
      alpha = 0.3
    ) +
    geom_point(
      aes(size = 1 / se, fill = fem_lfp),
      shape = 21,
      color = "white",
      stroke = 0.8,
      alpha = 0.85
    ) +
    geom_text_repel(
      aes(label = iso3c),
      size = 2.8,
      color = "grey40",
      max.overlaps = 20,
      seed = 42
    ) +
    scale_fill_viridis_c(
      option = "B",
      name = "Female LFP (%)",
      direction = -1
    ) +
    scale_size_continuous(range = c(2, 7), guide = "none") +
    labs(
      title = "The home-resource gradient is positive across systems, with little systematic relation to leave duration",
      subtitle = "Each point is one education system. Y = per 1 SD home-resource gradient for TIMSS mathematics. Size reflects precision.",
      x = "Paid parental leave (weeks, full-rate equivalent)",
      y = "Home-resource gradient\n(per 1 SD increase in resources)",
      caption = "Source: TIMSS 2019 linked to OECD/ILO and World Bank data."
    ) +
    theme_paper() +
    theme(
      legend.position = c(0.15, 0.2),
      legend.background = element_rect(
        fill = scales::alpha("white", 0.9),
        color = NA
      )
    )

  save_plot_exports(
    plot,
    "figure3_scatter_leave_home_resource_gradient",
    width = 10,
    height = 7
  )

  plot
}
