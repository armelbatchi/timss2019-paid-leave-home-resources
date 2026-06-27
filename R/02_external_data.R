# Official source pages are documented in README.md. No source or derived data
# are distributed with this repository.

read_leave_data <- function(path = Sys.getenv("LEAVE_DATA_FILE", unset = "")) {
  if (!nzchar(path)) {
    stop(
      paste0(
        "Set LEAVE_DATA_FILE to a local CSV containing the harmonized paid-leave ",
        "measure used in the manuscript. Required columns: iso3c and leave_weeks. ",
        "See README.md for the official OECD and ILO source links."
      ),
      call. = FALSE
    )
  }

  if (!file.exists(path)) {
    stop("LEAVE_DATA_FILE does not exist: ", path, call. = FALSE)
  }

  leave_data <- readr::read_csv(path, show_col_types = FALSE)
  required_columns <- c("iso3c", "leave_weeks")
  missing_columns <- setdiff(required_columns, names(leave_data))

  if (length(missing_columns) > 0) {
    stop(
      "The paid-leave file is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  leave_data %>%
    dplyr::transmute(
      iso3c = as.character(iso3c),
      leave_weeks = as.numeric(leave_weeks)
    ) %>%
    dplyr::filter(!is.na(iso3c), !is.na(leave_weeks)) %>%
    dplyr::distinct(iso3c, .keep_all = TRUE)
}

get_macro_indicators <- function(start_year = 2018, end_year = 2020) {
  gdp <- WDI::WDI(
    indicator = "NY.GDP.PCAP.PP.CD",
    start = start_year,
    end = end_year,
    extra = TRUE
  ) %>%
    dplyr::filter(!is.na(NY.GDP.PCAP.PP.CD), !is.na(iso3c)) %>%
    dplyr::group_by(iso3c) %>%
    dplyr::summarise(
      gdp_pc = mean(NY.GDP.PCAP.PP.CD, na.rm = TRUE),
      .groups = "drop"
    )

  female_lfp <- WDI::WDI(
    indicator = "SL.TLF.CACT.FE.ZS",
    start = start_year,
    end = end_year,
    extra = TRUE
  ) %>%
    dplyr::filter(!is.na(SL.TLF.CACT.FE.ZS), !is.na(iso3c)) %>%
    dplyr::group_by(iso3c) %>%
    dplyr::summarise(
      fem_lfp = mean(SL.TLF.CACT.FE.ZS, na.rm = TRUE),
      .groups = "drop"
    )

  dplyr::full_join(gdp, female_lfp, by = "iso3c")
}

build_country_external <- function() {
  read_leave_data() %>%
    dplyr::left_join(get_macro_indicators(2018, 2020), by = "iso3c")
}
