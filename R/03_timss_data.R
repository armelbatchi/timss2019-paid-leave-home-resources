locate_timss_dir <- function() {
  timss_dir <- Sys.getenv("TIMSS_DIR", unset = "")

  if (!nzchar(timss_dir)) {
    stop(
      "TIMSS_DIR is not set. Define the path to the official TIMSS 2019 Grade 4 SPSS files.",
      call. = FALSE
    )
  }

  timss_dir <- normalizePath(path.expand(timss_dir), winslash = "/", mustWork = FALSE)
  if (!dir.exists(timss_dir)) {
    stop("TIMSS_DIR does not exist: ", timss_dir, call. = FALSE)
  }

  sav_files <- list.files(
    timss_dir,
    pattern = "^(ACG|ASA|ASG|ASH|ASR|AST|ATG).*[MBZ]7[.](sav|SAV)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(sav_files) == 0) {
    stop(
      "No TIMSS 2019 Grade 4 SPSS files were found under TIMSS_DIR.",
      call. = FALSE
    )
  }

  names(sort(table(dirname(sav_files)), decreasing = TRUE))[1]
}

read_timss_2019 <- function() {
  EdSurvey::readTIMSS(
    path = locate_timss_dir(),
    countries = "*",
    gradeLvl = "4"
  )
}

extract_timss_variables <- function(timss_object) {
  requested_variables <- c(
    "asdhaps", "asdhedup", "asbg04", "asbghrl",
    "itsex", "idcntry", "totwgt", paste0("asmmat0", 1:5)
  )

  sdf_list <- if (!is.null(timss_object$datalist)) {
    timss_object$datalist
  } else {
    list(timss_object)
  }

  purrr::map_dfr(sdf_list, function(sdf) {
    available <- tolower(colnames(sdf))
    matched <- intersect(requested_variables, available)
    missing <- setdiff(requested_variables, matched)

    extracted <- EdSurvey::getData(
      data = sdf,
      varnames = matched,
      addAttributes = FALSE
    )

    for (variable in missing) {
      extracted[[variable]] <- NA
    }

    extracted
  })
}

build_analysis_data <- function(raw_data, country_external) {
  country_lookup <- build_country_lookup(suppressWarnings(as.numeric(raw_data$idcntry))) %>%
    distinct(idcntry_raw, .keep_all = TRUE)

  raw_data %>%
    mutate(
      preschool_years = suppressWarnings(as.numeric(asdhaps)),
      preschool = case_when(
        is.na(preschool_years) ~ NA_real_,
        preschool_years > 0 ~ 1,
        preschool_years == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      books = suppressWarnings(as.numeric(asbg04)),
      home_resources = suppressWarnings(as.numeric(asbghrl)),
      parent_ed = suppressWarnings(as.numeric(asdhedup)),
      idcntry_raw = suppressWarnings(as.numeric(idcntry)),
      gender = case_when(
        suppressWarnings(as.numeric(itsex)) == 1 ~ "Female",
        suppressWarnings(as.numeric(itsex)) == 2 ~ "Male",
        TRUE ~ NA_character_
      )
    ) %>%
    left_join(country_lookup, by = "idcntry_raw") %>%
    left_join(country_external, by = "iso3c") %>%
    mutate(
      leave_group = leave_group_factor(leave_weeks),
      gender = factor(gender, levels = c("Female", "Male")),
      math_avg = rowMeans(
        dplyr::across(dplyr::all_of(paste0("asmmat0", 1:5))),
        na.rm = TRUE
      )
    ) %>%
    filter(
      !is.na(iso3c),
      !is.na(leave_weeks),
      !is.na(home_resources),
      !is.na(parent_ed),
      !is.na(totwgt),
      is.finite(math_avg)
    ) %>%
    mutate(
      home_resources_z = as.numeric(scale(home_resources))
    )
}
