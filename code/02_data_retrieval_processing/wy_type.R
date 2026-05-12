# DWR Drought Synthesis
# Purpose: Scrape data from Water Year Hydrologic Classification Indices webpage for
# years 2010-2024 and process it for integration with other data for our analyses
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

get_wy_type <- function() {
  # Start session
  wyt_session <- rvest::session(
    "https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST"
  )

  # Find element for WY type table
  wyt_tbl_element <- rvest::html_element(
    wyt_session,
    xpath = '//*[@id="main-content"]/div/div[2]/main/section/pre/text()'
  )

  # Extract and clean text for WY type table
  wyt_tbl_text <- rvest::html_text(wyt_tbl_element) |>
    stringr::str_split("\r\n") |>
    unlist() |>
    stringr::str_trim()

  # Find start and end of WY type table and extract
  wyt_tbl_start <- stringr::str_which(wyt_tbl_text, "WY\\s+Oct-Mar") + 2
  wyt_tbl_end <- stringr::str_which(wyt_tbl_text, "Eight River Runoff") - 1

  wyt_tbl_trim <- wyt_tbl_text[wyt_tbl_start:wyt_tbl_end]
  wyt_tbl_keep <- wyt_tbl_trim[
    dplyr::if_else(
      cumsum(dplyr::if_else(wyt_tbl_trim == "", 1, 0)) < 1,
      TRUE,
      FALSE
    )
  ]

  # Define column names for WY type table
  wyt_names <- c(
    "Year",
    "Sac_OctMar",
    "Sac_AprJul",
    "Sac_WYsum",
    "Sac_Index",
    "Sac_WYtype",
    "SJ_OctMar",
    "SJ_AprJul",
    "SJ_WYsum",
    "SJ_Index",
    "SJ_WYtype"
  )

  # Convert WY type table to tibble
  df_wyt_1901_1905 <- readr::read_table(
    wyt_tbl_keep[1:5],
    col_names = wyt_names[c(1, 7:11)]
  )
  df_wyt_1906_cur <- readr::read_table(
    wyt_tbl_keep[6:length(wyt_tbl_keep)],
    col_names = wyt_names
  )
  df_wyt_all <- dplyr::bind_rows(df_wyt_1906_cur, df_wyt_1901_1905) |>
    dplyr::arrange(Year)

  # Select WY's and columns of interest
  df_wyt_2010_2024 <- df_wyt_all |>
    dplyr::filter(Year %in% 2010:2024) |>
    dplyr::select(Year, Sac_Index, Sac_WYtype, SJ_Index, SJ_WYtype)

  return(df_wyt_2010_2024)
}
