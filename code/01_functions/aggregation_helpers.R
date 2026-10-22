# DWR Nutrient Synthesis
# Purpose: Functions used in the targets workflow for integrating and aggregating data used in the
# SEM models
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

#' Join Standardized Time-Series Datasets
#'
#' Takes multiple cleaned data frames (each containing `region`, `date`, and unique metric columns)
#' and full-joins them by region and date.
#'
#' @param ... Cleaned time-series data frames (e.g., clean_water_quality, clean_meteorology).
#'
#' @return A single integrated data frame aligned by `region` and `date`.
join_time_series <- function(...) {
  datasets <- list(...)

  # Reduce list of datasets by full-joining on region and date
  purrr::reduce(
    datasets,
    \(x, y) dplyr::full_join(x, y, by = c("region", "date"))
  ) |>
    dplyr::arrange(region, date)
}

#' Summarize Daily Time-Series to Monthly Regional Scale
#'
#' @param data Data frame with standardized `region` and `date` columns.
#' @return Data frame grouped by region and year_month.
summarize_to_monthly <- function(data) {
  data |>
    dplyr::mutate(year_month = lubridate::floor_date(as.Date(date), "month")) |>
    dplyr::summarize(
      dplyr::across(tidyselect::where(is.numeric), \(x) mean(x, na.rm = TRUE)),
      .by = c(region, year_month)
    )
}

#' Summarize Monthly Data to Seasonal Regional Scale
#'
#' @param data Data frame with `region` and `year_month` (or `date`) columns.
#' @param min_months_per_season Minimum required months to form a complete season (default: 3).
#' @return Data frame grouped by region, season_year, and season.
summarize_to_seasonal <- function(data, min_months_per_season = 3) {
  # Ensure year_month exists if raw daily data is passed directly
  if (!"year_month" %in% names(data) && "date" %in% names(data)) {
    data <- data |>
      dplyr::mutate(year_month = lubridate::floor_date(as.Date(date), "month"))
  }

  data |>
    dplyr::mutate(
      season = assign_season(year_month),
      # Shift December to next year so Winter 2024 = Dec 2024 - Feb 2025
      season_year = dplyr::if_else(
        month(year_month) == 12,
        year(year_month) + 1,
        year(year_month)
      )
    ) %>%
    group_by(region, season_year, season) %>%
    summarize(
      across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
      months_present = n(),
      .groups = "drop"
    ) %>%
    filter(months_present >= min_months_per_season)
}
