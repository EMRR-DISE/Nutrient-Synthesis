# DWR Drought Synthesis
# Purpose: Retrieve Delta Inflow from DAYFLOW model for water years 1997-2024 and process it
# for integration with other data for our analyses
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

#' Safely Execute an httr2 Request with Informative Error Handling
#'
#' @param req An httr2 request object.
#' @param error_msg Custom error message prefix for cli::cli_abort.
#'
#' @return An httr2 response object.
exec_cnra_req <- function(
  req,
  error_msg = "CNRA Datastore API request execution failed"
) {
  # Execute request safely, capturing low-level network/HTTP errors
  resp <- rlang::try_fetch(
    expr = {
      httr2::req_perform(req)
    },
    error = function(cnd) {
      cli::cli_abort(error_msg, parent = cnd)
    }
  )

  # Ensure HTTP status code is valid (200 OK)
  httr2::resp_check_status(resp)

  return(resp)
}

#' Download CSV Resource from CNRA Portal
#'
#' @param resource_id_or_url CKAN Resource UUID string OR direct URL.
#' @param ... Additional arguments passed to `readr::read_csv()`.
#'
#' @return A data frame containing the downloaded dataset.
get_cnra_csv_data <- function(resource_id_or_url, ...) {
  # Resolve URL if passed a CKAN resource UUID
  if (!stringr::str_detect(resource_id_or_url, "^https?://")) {
    api_url <- paste0(
      "https://data.cnra.ca.gov/api/3/action/resource_show?id=",
      resource_id_or_url
    )

    req_meta <- httr2::request(api_url) |>
      httr2::req_user_agent("targets-data-integration-pipeline") |>
      httr2::req_timeout(seconds = 30) |>
      httr2::req_retry(max_tries = 3)

    resp_meta <- exec_cnra_req(
      req_meta,
      "Failed to resolve CNRA metadata endpoint"
    )
    download_url <- httr2::resp_body_json(resp_meta)$result$url
  } else {
    download_url <- resource_id_or_url
  }

  cli::cli_alert_info("Downloading CNRA resource: {.url {download_url}}")

  # Fetch the actual CSV payload
  req_data <- httr2::request(download_url) |>
    httr2::req_user_agent("targets-data-integration-pipeline") |>
    httr2::req_timeout(seconds = 60) |>
    httr2::req_retry(max_tries = 3)

  resp_data <- exec_cnra_req(
    req_data,
    paste0("Failed to download payload from ", download_url)
  )

  readr::read_csv(httr2::resp_body_raw(resp_data), show_col_types = FALSE, ...)
}

get_dayflow <- function() {
  # Download data from CNRA data portal: <https://data.cnra.ca.gov/dataset/dayflow>
  dayflow_1997_2023 <- readr::read_csv(
    "https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/21c377fe-53b8-4bd6-9e1f-2025221be095/download/dayflow-results-1997-2023.csv"
  )
  dayflow_2024 <- readr::read_csv(
    "https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/6a7cb172-fb16-480d-9f4f-0322548fee83/download/dayflowcalculations2024.csv"
  )

  # Clean up and combine data
  df_dayflow <- dayflow_1997_2023 |>
    dplyr::mutate(Date = lubridate::mdy(Date)) |>
    dplyr::bind_rows(dayflow_2024) |>
    dplyr::select(Date, SAC, SJR, TOT) |>
    dplyr::mutate(
      Year = lubridate::year(Date),
      Month = lubridate::month(Date)
    ) |>
    # Remove data prior to 2009 and after 2024
    dplyr::filter(Year %in% 2009:2024)

  return(df_dayflow)
}
