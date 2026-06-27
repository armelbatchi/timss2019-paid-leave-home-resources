build_country_profiles <- function(data) {
  data %>%
    group_by(iso3c, country_name, leave_weeks, leave_group) %>%
    summarise(
      N = n(),
      mean_math = safe_weighted_mean(math_avg, totwgt),
      mean_home_resources = safe_weighted_mean(home_resources, totwgt),
      mean_books = safe_weighted_mean(books, totwgt),
      mean_parent_ed = safe_weighted_mean(parent_ed, totwgt),
      gdp_pc = first(gdp_pc),
      fem_lfp = first(fem_lfp),
      .groups = "drop"
    ) %>%
    arrange(leave_weeks, country_name)
}

make_table1 <- function(country_profiles) {
  table_data <- country_profiles %>%
    select(
      Country = country_name,
      `Leave (wks)` = leave_weeks,
      Group = leave_group,
      N,
      `Mean Math` = mean_math,
      `Home Resources` = mean_home_resources,
      `Books at Home` = mean_books
    )

  table_gt <- table_data %>%
    gt() %>%
    tab_header(
      title = md("**Table 1. Country profiles of mathematics achievement, home resources, and paid parental leave**"),
      subtitle = "TIMSS 2019 Grade 4 linked to OECD/ILO leave data"
    ) %>%
    fmt_number(
      columns = c(`Mean Math`, `Home Resources`, `Books at Home`),
      decimals = 1
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    tab_source_note(
      "Mean mathematics is the system weighted mean of the five plausible values. TIMSS final student weights were used."
    )

  save_table_exports(
    table_gt,
    table_data,
    "table1_country_profiles_home_resources_leave"
  )

  table_gt
}

build_heatmap_data <- function(data) {
  data %>%
    filter(!is.na(leave_group), !is.na(home_resources_z)) %>%
    mutate(
      resource_group = ntile(home_resources_z, 3),
      resource_group = factor(
        resource_group,
        levels = 1:3,
        labels = c("Low", "Middle", "High")
      )
    ) %>%
    group_by(leave_group, resource_group) %>%
    summarise(
      mean_math = safe_weighted_mean(math_avg, totwgt),
      .groups = "drop"
    )
}

make_figure1 <- function(heatmap_data) {
  plot <- ggplot(
    heatmap_data,
    aes(x = resource_group, y = leave_group, fill = mean_math)
  ) +
    geom_tile(color = "white", linewidth = 1.2) +
    geom_text(
      aes(label = round(mean_math, 0)),
      color = "white",
      fontface = "bold",
      size = 4.8
    ) +
    scale_fill_gradient2(
      low = pal$short_leave,
      mid = "#F5F5F5",
      high = pal$long_leave,
      midpoint = median(heatmap_data$mean_math, na.rm = TRUE),
      name = "Mean mathematics"
    ) +
    labs(
      title = "Average mathematics rises with home resources in every paid-leave setting",
      subtitle = "Mean TIMSS Grade 4 mathematics by home-resource tercile and paid-leave setting",
      x = "Home resources for learning",
      y = "Paid-leave setting",
      caption = "Source: TIMSS 2019 linked to OECD/ILO leave data."
    ) +
    theme_paper() +
    theme(panel.grid = element_blank(), legend.position = "right")

  save_plot_exports(
    plot,
    "figure1_heatmap_home_resources_leave",
    width = 9,
    height = 5
  )

  plot
}
