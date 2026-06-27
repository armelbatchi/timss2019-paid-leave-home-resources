required_packages <- c(
  "EdSurvey", "broom", "countrycode", "dplyr", "ggplot2", "ggrepel",
  "gt", "metafor", "purrr", "readr", "scales", "tibble", "tidyr", "WDI"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing R packages: ", paste(missing_packages, collapse = ", "),
      ". Install them before running the analysis."
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(EdSurvey)
  library(broom)
  library(countrycode)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(gt)
  library(metafor)
  library(purrr)
  library(readr)
  library(scales)
  library(tibble)
  library(tidyr)
  library(WDI)
})

options(stringsAsFactors = FALSE, scipen = 999)

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
output_dir <- file.path(project_dir, "output")
table_dir <- file.path(output_dir, "tables")
figure_dir <- file.path(output_dir, "figures")
object_dir <- file.path(output_dir, "objects")

for (path in c(output_dir, table_dir, figure_dir, object_dir)) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

pal <- list(
  short_leave = "#C44E52",
  medium_leave = "#D4A03B",
  long_leave = "#2C5F8A",
  light = "#E8EEF4",
  highlight = "#FFF3E0"
)

theme_paper <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.25), margin = margin(b = 8)),
      plot.subtitle = element_text(color = "grey40", size = rel(0.9), margin = margin(b = 10)),
      plot.caption = element_text(color = "grey45", size = rel(0.72), hjust = 0, margin = margin(t = 10)),
      axis.title = element_text(size = rel(0.9), color = "grey25"),
      axis.text = element_text(size = rel(0.82), color = "grey35"),
      axis.line = element_line(color = "grey25", linewidth = 0.5),
      axis.ticks = element_line(color = "grey25", linewidth = 0.5),
      axis.ticks.length = grid::unit(2.5, "mm"),
      panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}
