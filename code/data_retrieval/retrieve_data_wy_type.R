# DWR Drought Synthesis
# Purpose: Scrape data from Water Year Hydrologic Classification Indices webpage for years 2010-2024
# and save copy in data/external for continued processing
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

library(dplyr)
library(stringr)
library(readr)
library(rvest)
library(here)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Start session
wyt_session <- session(
  "https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST"
)

# Find element for WY type table
wyt_tbl_element <- html_element(
  wyt_session,
  xpath = '//*[@id="main-content"]/div/div[2]/main/section/pre/text()'
)

# Extract and clean text for WY type table
wyt_tbl_text <- html_text(wyt_tbl_element) %>%
  str_split("\r\n") %>%
  unlist() %>%
  str_trim()

# Find start and end of WY type table and extract
wyt_tbl_start <- str_which(wyt_tbl_text, "WY\\s+Oct-Mar") + 2
wyt_tbl_end <- str_which(wyt_tbl_text, "Eight River Runoff") - 1

wyt_tbl_trim <- wyt_tbl_text[wyt_tbl_start:wyt_tbl_end]
wyt_tbl_keep <- wyt_tbl_trim[if_else(
  cumsum(if_else(wyt_tbl_trim == "", 1, 0)) < 1,
  TRUE,
  FALSE
)]

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
df_wyt_1901_1905 <- read_table(
  wyt_tbl_keep[1:5],
  col_names = wyt_names[c(1, 7:11)]
)
df_wyt_1906_cur <- read_table(
  wyt_tbl_keep[6:length(wyt_tbl_keep)],
  col_names = wyt_names
)
df_wyt_all <- bind_rows(df_wyt_1906_cur, df_wyt_1901_1905) %>% arrange(Year)

# Select WY's and columns of interest
df_wyt_2010_2024 <- df_wyt_all %>%
  filter(Year %in% 2010:2024) %>%
  select(Year, Sac_Index, Sac_WYtype, SJ_Index, SJ_WYtype)

# Save local copy of data
df_wyt_2010_2024 %>% saveRDS(here("data/external/wy_type_2010_2024.rds"))
