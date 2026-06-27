safe_weighted_mean <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & !is.na(x) & !is.na(w)
  if (!any(keep)) return(NA_real_)
  stats::weighted.mean(x[keep], w[keep])
}

weighted_cor <- function(x, y, w) {
  keep <- is.finite(x) & is.finite(y) & is.finite(w) &
    !is.na(x) & !is.na(y) & !is.na(w)
  if (sum(keep) < 3) return(NA_real_)

  x <- x[keep]
  y <- y[keep]
  w <- w[keep]
  w <- w / sum(w)

  mean_x <- sum(w * x)
  mean_y <- sum(w * y)
  covariance <- sum(w * (x - mean_x) * (y - mean_y))
  variance_x <- sum(w * (x - mean_x)^2)
  variance_y <- sum(w * (y - mean_y)^2)

  if (variance_x <= 0 || variance_y <= 0) return(NA_real_)
  covariance / sqrt(variance_x * variance_y)
}

rubin_pool <- function(estimates, standard_errors) {
  keep <- is.finite(estimates) & is.finite(standard_errors)
  estimates <- estimates[keep]
  standard_errors <- standard_errors[keep]
  m <- length(estimates)

  if (m == 0) {
    return(list(estimate = NA_real_, variance = NA_real_, se = NA_real_))
  }

  pooled_estimate <- mean(estimates)
  within_variance <- mean(standard_errors^2)
  between_variance <- if (m > 1) stats::var(estimates) else 0
  total_variance <- within_variance + (1 + 1 / m) * between_variance

  list(
    estimate = pooled_estimate,
    variance = total_variance,
    se = sqrt(total_variance)
  )
}

fisher_pool_r <- function(correlations) {
  correlations <- correlations[is.finite(correlations)]
  if (length(correlations) == 0) return(NA_real_)

  fisher_z <- atanh(pmax(pmin(correlations, 0.999999), -0.999999))
  tanh(mean(fisher_z))
}

custom_country_match <- c(
  "Hong Kong SAR" = "HKG",
  "Hong Kong, China" = "HKG",
  "Chinese Taipei" = "TWN",
  "United States" = "USA",
  "Russian Federation" = "RUS",
  "Slovak Republic" = "SVK",
  "Korea" = "KOR",
  "Republic of Korea" = "KOR",
  "Iran, Islamic Rep." = "IRN",
  "Türkiye" = "TUR",
  "Turkey" = "TUR"
)

build_country_lookup <- function(idcntry_vector) {
  labels_map <- attr(idcntry_vector, "labels")

  if (!is.null(labels_map) && length(labels_map) > 0) {
    tibble(
      idcntry_raw = as.numeric(unname(labels_map)),
      country_name = names(labels_map)
    ) %>%
      mutate(
        iso3c = countrycode(
          country_name,
          origin = "country.name",
          destination = "iso3c",
          custom_match = custom_country_match,
          warn = FALSE
        )
      )
  } else {
    tibble(idcntry_raw = sort(unique(suppressWarnings(as.numeric(idcntry_vector))))) %>%
      filter(!is.na(idcntry_raw)) %>%
      mutate(
        iso3c = countrycode(idcntry_raw, "iso3n", "iso3c", warn = FALSE),
        country_name = countrycode(iso3c, "iso3c", "country.name", warn = FALSE)
      )
  }
}

leave_group_factor <- function(weeks) {
  factor(
    case_when(
      weeks <= 15 ~ "Short (≤15 wks)",
      weeks <= 40 ~ "Medium (16–40)",
      weeks > 40 ~ "Long (>40)",
      TRUE ~ NA_character_
    ),
    levels = c("Short (≤15 wks)", "Medium (16–40)", "Long (>40)")
  )
}

save_plot_exports <- function(plot_object, stem, width, height, dpi = 300) {
  ggsave(
    filename = file.path(figure_dir, paste0(stem, ".png")),
    plot = plot_object,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
  ggsave(
    filename = file.path(figure_dir, paste0(stem, ".pdf")),
    plot = plot_object,
    width = width,
    height = height,
    bg = "white"
  )
  invisible(plot_object)
}

save_table_exports <- function(gt_object, data, stem) {
  readr::write_csv(as.data.frame(data), file.path(table_dir, paste0(stem, ".csv")))

  tryCatch(
    gt::gtsave(gt_object, filename = paste0(stem, ".docx"), path = table_dir),
    error = function(e) warning("DOCX export skipped: ", conditionMessage(e), call. = FALSE)
  )

  tryCatch(
    gt::gtsave(
      gt_object,
      filename = paste0(stem, ".png"),
      path = table_dir,
      zoom = 2,
      expand = 10
    ),
    error = function(e) warning("PNG table export skipped: ", conditionMessage(e), call. = FALSE)
  )

  invisible(gt_object)
}
