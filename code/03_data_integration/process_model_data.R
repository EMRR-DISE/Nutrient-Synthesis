# Prepare input data for models used in the DWR nutrient synthesis project
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Global Code --------------------------------------------------------------------------------

# Load packages
library(tidyverse)
library(rlang)
library(here)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Load global functions
source(here("code/01_functions/global_funcs.R"))

# Load data retrieval and processing functions
data_funcs <- list.files(
  here("code/02_data_retrieval_processing"),
  full.names = TRUE
)
walk(data_funcs, source)

# Create functions used in this script
# Replace values below the reporting limit with simulated values between `min_val`
# and the RL
replace_blw_rl <- function(df, min_val = 0, seed = 1) {
  # Pull out values that are below the RL
  df_blw_rl <- filter(df, Sign == "<")

  # Replace below RL values with simulated ones
  withr::with_seed(
    # Set seed for reproducibility
    seed = seed,
    df_blw_rl_sim <- df_blw_rl |>
      mutate(
        Result = round(runif(nrow(df_blw_rl), min = min_val, max = Result), 6)
      )
  )

  # Add simulated values back to main data frame
  df |>
    filter(Sign != "<") |>
    bind_rows(df_blw_rl_sim)
}

# DWR Integrated Discrete Nutrient Data ------------------------------------------------------

df_nutr <- get_nutrients()

# Prepare data before averaging
df_nutr_c <- df_nutr %>%
  # Replace values below the reporting limit with simulated values
  replace_blw_rl() |>
  # Rename parameters to be friendly as column names
  mutate(
    Parameter = recode_values(
      Parameter,
      "Dissolved Ammonia" ~ "DissAmmonia",
      "Dissolved Nitrate + Nitrite" ~ "DissNitrateNitrite",
      "Dissolved Organic Nitrogen" ~ "DON",
      "Dissolved ortho-Phosphate" ~ "DissOrthophos",
      "Total Kjeldahl Nitrogen" ~ "TKN",
      "Total Phosphorus" ~ "TotPhos"
    )
  ) |>
  # Pivot to wide for integration and aggregation
  pivot_wider(
    id_cols = c(Project, Station_Name, Date, Year, Month, Region),
    names_from = Parameter,
    values_from = Result
  )

# Integrated Discrete WQ Data from discretewq ------------------------------------------------

df_dwq <- get_discretewq()

# Prepare data before averaging
df_dwq_c <- df_dwq %>%
  # Replace Chlorophyll and DissSilica values below the reporting limit with simulated values
  replace_blw_rl() |>
  # Convert pH values to H+ concentration before averaging
  mutate(Result = replace_when(Result, Parameter == "pH" ~ 10^-Result)) |>
  # Pivot to wide for integration and aggregation
  pivot_wider(
    id_cols = c(Source, Station, Date, Year, Month, Region),
    names_from = Parameter,
    values_from = Result
  )

# DAYFLOW ------------------------------------------------------------------------------------

df_dayflow <- get_dayflow()

# Calculate monthly totals
df_dayflow_tot_mo <- df_dayflow %>%
  summarize(
    Sac_Inflow = sum(SAC),
    SJR_Inflow = sum(SJR),
    Total_Inflow = sum(TOT),
    .by = c(Year, Month)
  )

# Further calculate seasonal totals using Adjusted calendar year: December-November,
# with December of the previous calendar year included with the following year
df_dayflow_tot_seas <- df_dayflow_tot_mo %>%
  mutate(
    YearAdj = assign_year_adj(Month, Year),
    Season = assign_season(Month)
  ) |>
  summarize(across(ends_with("Inflow"), sum), .by = c(YearAdj, Season))

# Water Year Hydrologic Classification Indices -----------------------------------------------

df_wyt <- get_wy_type()

# Prepare monthly values
df_wyt_mo <- df_wyt %>%
  # Expand with months
  expand_grid(Month = 1:12) %>%
  # Adjust WY to calendar year
  mutate(Year = if_else(Month %in% 10:12, Year - 1, Year))

# Prepare seasonal values
df_wyt_seas <- df_wyt_mo %>%
  mutate(Season = assign_season(Month)) |>
  # Use adjusted calendar year for the seasonal assignments Adjusted calendar year:
  # December-November, with December of the previous calendar year included with the
  # following year
  filter(!Month %in% 10:12) %>%
  select(-Month) %>%
  rename(YearAdj = Year) %>%
  distinct()

# Clam Data ----------------------------------------------------------------------------------

df_clams <- get_clam_data()

# Vegetation Index Data ----------------------------------------------------------------------

df_veg_index <- get_veg_index()

# Wind Data ----------------------------------------------------------------------------------

df_wind <- get_wind_data()

# PESP ---------------------------------------------------------------------------------------

df_pesp <- get_pesp()

# Define AlgalGroups to be categorized as "Other"
other_groups <- c(
  'Raphidophytes',
  'Ciliates',
  'Coccolithophores',
  'Ochrophytes',
  'Xanthophytes',
  'Eustigmatophytes',
  'Dinoflagellates',
  'Euglenoids',
  'Chrysophytes'
)

# Define variables to be aggregated
pesp_aggr_vars <- c("Biovolume_per_mL", "Units_per_mL", "C_biomass")

# Count unique stations per region-month-year
df_pesp_count <- rep(list(df_pesp), 3) |>
  set_names(c("n_stations_biovol", "n_stations_units", "n_stations_cbio")) |>
  map2(pesp_aggr_vars, \(x, y) drop_na(x, all_of(y))) |>
  map(\(x) distinct(x, Region, Month, Year, Station)) |>
  imap(\(x, idx) count(x, Region, Month, Year, name = idx)) |>
  reduce(\(x, y) full_join(x, y, by = join_by(Region, Month, Year)))

# Calculate Biovolume/Units/C Biomass per Algal Group per Region and Month-Year
df_pesp_mo <- df_pesp |>
  # Sum total Biovolume/Units/C Biomass per Station, Date, and Algal Group (Sum1)
  summarize(
    across(all_of(pesp_aggr_vars), \(x) sum(x, na.rm = TRUE)),
    .by = c(Region, Station, Month, Year, Date, AlgalGroup)
  ) |>
  # Per month, average data from multi-sampled stations (for overall weighted average)
  # average across dates within each station-month-year
  summarize(
    across(all_of(pesp_aggr_vars), \(x) mean(x, na.rm = TRUE)),
    .by = c(Region, Station, Month, Year, AlgalGroup)
  ) |>
  # Sum over all Sum1's in the given Month-Year per Region
  summarize(
    across(all_of(pesp_aggr_vars), \(x) sum(x, na.rm = TRUE)),
    .by = c(Region, Month, Year, AlgalGroup)
  ) |>
  # Divide by the number of Stations in that Region + Month-Year that collected data
  # (regardless of if they have data for a particular group)
  left_join(df_pesp_count, by = join_by(Region, Month, Year)) |>
  mutate(
    bv = Biovolume_per_mL / n_stations_biovol,
    units = Units_per_mL / n_stations_units,
    cbio = C_biomass / n_stations_cbio,
    .keep = "unused"
  ) |>
  # Categorize and combine "Other" AlgalGroups (sum)
  mutate(
    AlgalGroup = replace_when(
      AlgalGroup,
      AlgalGroup %in% other_groups ~ "Other"
    )
  ) |>
  summarize(
    across(c(bv, units, cbio), \(x) round(sum(x, na.rm = TRUE), 2)),
    .by = c(Region, Month, Year, AlgalGroup)
  ) |>
  pivot_wider(
    names_from = AlgalGroup,
    values_from = c(bv, units, cbio),
    values_fill = 0
  )

# Aggregate Data ----------------------------------------------------------------------------

# Define parameter names for aggregation
aggr_vars <- c(
  "DissAmmonia",
  "DissNitrateNitrite",
  "DON",
  "DissOrthophos",
  "TKN",
  "TotPhos",
  "Temperature",
  "Salinity",
  "Turbidity",
  "DissolvedOxygen",
  "Chlorophyll",
  "pH",
  "DissSilica",
  "MVI",
  "Clam_Filtration",
  "Clam_Turnover",
  "CorbiculaBiomass",
  "PotamocorbulaBiomass",
  "VegIndex_Subm",
  "VegIndex_Surf"
)

# Calculate Monthly-Regional averages for those that require it -
# nutrients, discretewq parameters, clams, and vegetation index
df_avg_mo <-
  bind_rows(df_nutr_c, df_dwq_c, df_clams, df_veg_index) %>%
  summarize(
    across(all_of(aggr_vars), \(x) na_if(mean(x, na.rm = TRUE), NaN)),
    .by = c(Year, Month, Region)
  ) |>
  # Add monthly averages for PESP
  full_join(df_pesp_mo, by = join_by(Year, Month, Region))

# Further calculate Seasonal-Regional averages using Adjusted calendar year:
# December-November, with December of the previous calendar year included with the
# following year
df_avg_seas <- df_avg_mo %>%
  mutate(
    YearAdj = assign_year_adj(Month, Year),
    Season = assign_season(Month)
  ) |>
  summarize(
    across(
      all_of(aggr_vars) | starts_with(c("bv_", "units_", "cbio_")),
      \(x) na_if(mean(x, na.rm = TRUE), NaN)
    ),
    .by = c(YearAdj, Season, Region)
  )

# Combine All Data ---------------------------------------------------------------------------

# Add additional monthly and seasonal values with specific calculation methods

# Create data frame that contains all possible combinations of year, month, and region
df_yr_mo_reg <-
  expand_grid(
    Year = 2010:2024,
    Month = 1:12,
    Region = sort(unique(df_avg_mo$Region))
  ) %>%
  # Remove rows after June 2024
  filter_out(Year == 2024, Month > 6) %>%
  # Create Month-Year column
  mutate(
    MonthYear = paste(month(Month, label = TRUE), Year, sep = "-"),
    .before = Year
  )

# Combine all Monthly-Regional averages, monthly totals, or annual values into one data frame
monthly_values <-
  list(
    df_yr_mo_reg,
    df_avg_mo,
    df_dayflow_tot_mo,
    df_wyt_mo,
    df_wind
  ) %>%
  reduce(left_join)

# Create data frame that contains all possible combinations of year, season, and region
df_yr_seas_reg <- df_yr_mo_reg %>%
  mutate(Season = assign_season(Month)) %>%
  distinct(YearAdj = Year, Season, Region) %>%
  # Remove Summer 2024 because of low sampling
  filter_out(YearAdj == 2024, Season == "Summer")

# Combine all Seasonal-Regional averages, seasonal totals, or annual values into one data frame
# Only using monthly values for the wind variables for now
seasonal_values <-
  list(
    df_yr_seas_reg,
    df_avg_seas,
    df_dayflow_tot_seas,
    df_wyt_seas
  ) %>%
  reduce(left_join)

# Conversions and Calculations --------------------------------------------------------------

# Combine data frames of monthly and seasonal values into a list for joint operations
ls_model_data <- lst(monthly_values, seasonal_values) |>
  map(
    \(x) {
      mutate(
        x,
        # Convert average H+ concentration to average pH
        pH = -log10(pH),
        # Nutrient molarity conversions and ratio calculations
        across(
          c(DissAmmonia, DissNitrateNitrite, DON, TKN),
          \(var) var / 14.01,
          .names = "{.col}_mol"
        ),
        across(
          c(DissOrthophos, TotPhos),
          \(var) var / 30.97,
          .names = "{.col}_mol"
        ),
        DIN_DIP = (DissAmmonia_mol + DissNitrateNitrite_mol) /
          DissOrthophos_mol,
        NOx_NH3 = DissNitrateNitrite_mol / DissAmmonia_mol,
        NH3_DIN = DissAmmonia_mol / (DissAmmonia_mol + DissNitrateNitrite_mol),
        NH3_DIP = DissAmmonia_mol / DissOrthophos_mol,
        NOx_DIP = DissNitrateNitrite_mol / DissOrthophos_mol,
        DON_DIN = DON_mol / (DissAmmonia_mol + DissNitrateNitrite_mol),
        TN_TP = (TKN_mol + DissNitrateNitrite_mol) / TotPhos_mol
      ) |>
        # Round MVI to nearest whole number
        mutate(MVI_round = round(MVI), .after = MVI)
    }
  )

# Export Processed Data ----------------------------------------------------------------------

# Export the monthly and seasonal data as .csv and .rds files for the models
fp_processed <- here("data/processed")

# Export data as csv files
ls_model_data %>%
  iwalk(\(x, idx) write_csv(x, file = paste0(fp_processed, "/", idx, ".csv")))

# Export data as rds files
ls_model_data %>%
  iwalk(\(x, idx) saveRDS(x, file = paste0(fp_processed, "/", idx, ".rds")))
