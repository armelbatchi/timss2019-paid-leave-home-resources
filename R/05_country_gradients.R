estimate_resource_gradient <- function(data, adjustment = c("main", "reduced")) {
  adjustment <- match.arg(adjustment)
  plausible_values <- intersect(paste0("asmmat0", 1:5), names(data))

  if (length(plausible_values) == 0 || sum(!is.na(data$home_resources_z)) < 100) {
    return(tibble())
  }

  right_hand_side <- if (adjustment == "main") {
    "home_resources_z + parent_ed + preschool_years + gender"
  } else {
    "home_resources_z + preschool_years + gender"
  }

  estimates <- purrr::map_dfr(plausible_values, function(plausible_value) {
    model <- tryCatch(
      lm(
        as.formula(paste(plausible_value, "~", right_hand_side)),
        data = data,
        weights = totwgt
      ),
      error = function(e) NULL
    )

    if (is.null(model)) {
      return(tibble(estimate = NA_real_, std.error = NA_real_))
    }

    broom::tidy(model) %>%
      filter(term == "home_resources_z") %>%
      select(estimate, std.error)
  })

  pooled <- rubin_pool(estimates$estimate, estimates$std.error)

  tibble(
    resource_slope = pooled$estimate,
    se = pooled$se,
    n = nrow(data)
  )
}

build_country_slopes <- function(data, adjustment = c("main", "reduced")) {
  adjustment <- match.arg(adjustment)

  data %>%
    group_by(iso3c, country_name, leave_weeks, leave_group) %>%
    group_modify(~ estimate_resource_gradient(.x, adjustment = adjustment)) %>%
    ungroup() %>%
    filter(is.finite(resource_slope), is.finite(se), se > 0) %>%
    mutate(
      z = resource_slope / se,
      p = 2 * pnorm(-abs(z)),
      ci_lo = resource_slope - 1.96 * se,
      ci_hi = resource_slope + 1.96 * se,
      sig = if_else(p < 0.05, "*", "")
    ) %>%
    arrange(resource_slope)
}

make_slope_table <- function(slopes, adjustment = c("main", "reduced")) {
  adjustment <- match.arg(adjustment)
  main_model <- adjustment == "main"

  table_data <- slopes %>%
    transmute(
      Country = country_name,
      `Leave (wks)` = leave_weeks,
      Group = leave_group,
      `Home-resource gradient` = sprintf("%.2f%s", resource_slope, sig),
      SE = sprintf("%.2f", se),
      `95% CI` = sprintf("[%.2f, %.2f]", ci_lo, ci_hi),
      N = n
    )

  title <- if (main_model) {
    "**Table 2. Within-system home-resource gradient (main specification)**"
  } else {
    "**Supplementary Table S1. Within-system home-resource gradient (reduced specification)**"
  }

  subtitle <- if (main_model) {
    "Per 1 SD increase in home resources, adjusting for parental education, preschool years, and gender"
  } else {
    "Per 1 SD increase in home resources, adjusting only for preschool years and gender"
  }

  note <- if (main_model) {
    paste(
      "Country-specific slopes were estimated separately for each of the five mathematics plausible values",
      "and pooled with Rubin's rules using TIMSS final student weights. * p < 0.05."
    )
  } else {
    paste(
      "Reduced-adjustment analysis omitting parental education.",
      "Slopes were pooled across the five mathematics plausible values with Rubin's rules using TIMSS final student weights. * p < 0.05."
    )
  }

  table_gt <- table_data %>%
    gt() %>%
    tab_header(title = md(title), subtitle = subtitle) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    tab_source_note(note)

  output_name <- if (main_model) {
    "table2_country_specific_home_resource_slopes_main"
  } else {
    "tableS1_country_specific_home_resource_slopes_reduced"
  }

  save_table_exports(table_gt, table_data, output_name)
  table_gt
}

build_prediction_grid <- function(data) {
  mean_parent_ed <- mean(data$parent_ed, na.rm = TRUE)
  mean_preschool <- mean(data$preschool, na.rm = TRUE)
  reference_gender <- factor(levels(data$gender)[1], levels = levels(data$gender))
  resource_range <- seq(-2, 2, by = 0.1)

  data %>%
    filter(!is.na(leave_group), !is.na(home_resources_z), is.finite(math_avg)) %>%
    group_by(leave_group) %>%
    group_modify(~ {
      model <- tryCatch(
        lm(
          math_avg ~ home_resources_z + parent_ed + preschool + gender,
          data = .x,
          weights = totwgt
        ),
        error = function(e) NULL
      )

      if (is.null(model)) return(tibble())

      new_data <- tibble(
        home_resources_z = resource_range,
        parent_ed = mean_parent_ed,
        preschool = mean_preschool,
        gender = reference_gender
      )

      predictions <- as.data.frame(
        predict(model, newdata = new_data, interval = "confidence")
      )

      new_data %>%
        mutate(
          predicted = predictions$fit,
          ci_lo = predictions$lwr,
          ci_hi = predictions$upr
        )
    }) %>%
    ungroup() %>%
    mutate(
      leave_label = factor(
        leave_group,
        levels = c("Short (≤15 wks)", "Medium (16–40)", "Long (>40)")
      )
    )
}

make_figure2 <- function(prediction_grid) {
  plot <- ggplot(
    prediction_grid,
    aes(
      x = home_resources_z,
      y = predicted,
      color = leave_label,
      fill = leave_label,
      group = leave_label
    )
  ) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1.4, na.rm = TRUE) +
    scale_color_manual(
      values = c(pal$short_leave, pal$medium_leave, pal$long_leave)
    ) +
    scale_fill_manual(
      values = c(pal$short_leave, pal$medium_leave, pal$long_leave)
    ) +
    labs(
      title = "The home-resource slope is strongly positive and nearly parallel across paid-leave settings",
      subtitle = "Predicted TIMSS Grade 4 mathematics by home learning resources, from leave-group-specific regressions",
      x = "Home resources for learning (z-score)",
      y = "Predicted TIMSS mathematics score",
      color = NULL,
      fill = NULL,
      caption = paste(
        "Source: TIMSS 2019 linked to OECD/ILO leave data.",
        "Lines are predicted values from leave-group-specific regressions of average plausible-value mathematics on ASBGHRL,",
        "holding parental education, preschool, and gender at sample reference values. Shaded bands are 95% CIs."
      )
    ) +
    theme_paper() +
    theme(
      legend.position = c(0.82, 0.84),
      legend.background = element_rect(
        fill = scales::alpha("white", 0.9),
        color = NA
      )
    )

  save_plot_exports(
    plot,
    "figure2_predicted_math_by_home_resources_leave",
    width = 10,
    height = 6.5
  )

  plot
}

make_figure_s2 <- function(slopes) {
  pooled_model <- metafor::rma(
    yi = resource_slope,
    vi = se^2,
    data = slopes,
    method = "REML"
  )

  forest_data <- slopes %>%
    transmute(
      country_name,
      slope = resource_slope,
      lo = ci_lo,
      hi = ci_hi
    ) %>%
    arrange(slope) %>%
    mutate(country_name = factor(country_name, levels = country_name))

  pooled_data <- tibble(
    country_name = "Random-effects summary",
    slope = as.numeric(pooled_model$b),
    lo = as.numeric(pooled_model$ci.lb),
    hi = as.numeric(pooled_model$ci.ub)
  )

  plot <- ggplot(forest_data, aes(x = slope, y = country_name)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, color = "grey50") +
    geom_point(color = pal$long_leave, size = 2) +
    geom_errorbarh(
      data = pooled_data,
      aes(xmin = lo, xmax = hi, y = country_name),
      inherit.aes = FALSE,
      height = 0,
      linewidth = 1
    ) +
    geom_point(
      data = pooled_data,
      aes(x = slope, y = country_name),
      inherit.aes = FALSE,
      shape = 18,
      size = 4,
      color = "black"
    ) +
    labs(
      title = "Supplementary Figure S2. System-level home-resource gradients with random-effects summary",
      subtitle = "Main-model slopes per 1 SD increase in home resources, with the random-effects pooled estimate",
      x = "Home-resource gradient (TIMSS points per 1 SD)",
      y = NULL,
      caption = "Horizontal bars are 95% CIs. The black diamond is the random-effects pooled estimate of the Table 2 slopes."
    ) +
    theme_paper() +
    theme(
      axis.text.y = element_text(size = rel(0.7)),
      axis.line = element_blank(),
      panel.border = element_rect(fill = NA, color = "grey25", linewidth = 0.7)
    )

  save_plot_exports(
    plot,
    "figureS2_forest_system_home_resource_slopes",
    width = 8.5,
    height = 10
  )

  plot
}
