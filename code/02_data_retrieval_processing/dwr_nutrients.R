# DWR Nutrient Synthesis
# Purpose: Retrieve integrated DWR nutrient data set and process it for integration with
# other data for our analyses
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# NOTE: Requires functions from "code/01_functions/global_funcs.R"

get_nutrients <- function() {
  # Import Data -------------------------------------------------------------------------------

  # Import nutrient data set
  df_nutr <- readRDS(here::here("data/processed/nutrient_data_discrete.rds"))

  # Import phytoplankton data from FRP's EDI publication to temporarily assign lat-long coordinates
  # to FRP's nutrient data in the DWR integrated discrete nutrient data set
  edi_id_frp <- "edi.269.5"
  edi_data_ent_frp <- get_edi_data_entities(edi_id_frp)
  edi_data_ent_frp_phyto <- stringr::str_subset(edi_data_ent_frp, "^phyto")
  get_edi_data(edi_id_frp, edi_data_ent_frp_phyto)
  temp_files <- list.files(tempdir(), full.names = TRUE)

  df_frp_phyto_coord <-
    readr::read_csv(stringr::str_subset(temp_files, edi_data_ent_frp_phyto)) |>
    tidyr::drop_na(LatitudeStart, LongitudeStart) |>
    dplyr::summarize(
      Latitude = mean(LatitudeStart),
      Longitude = mean(LongitudeStart),
      .by = Location
    )

  # Prepare Data ------------------------------------------------------------------------------

  # Prepare data for aggregation
  df_nutr_c1 <- df_nutr |>
    # Temporarily add lat-long coordinates for FRP stations from the phytoplankton data set
    dplyr::filter(Project == "Fish Restoration Program") |>
    dplyr::select(-c(Latitude, Longitude)) |>
    dplyr::left_join(
      df_frp_phyto_coord,
      by = dplyr::join_by(Station_Name == Location)
    ) |>
    dplyr::bind_rows(
      df_nutr |> dplyr::filter(Project != "Fish Restoration Program")
    ) |>
    # Remove records without lat-long coordinates
    tidyr::drop_na(Latitude, Longitude) |>
    # Assign regions to the stations
    assign_regions() |>
    # Temporarily remove dissolved nitrate records
    dplyr::filter_out(Analyte == "Dissolved Nitrate")

  # Remove various duplicated records so that there is only one sample per station-day:

  # Clean up duplicated records that share same Date_Time - it might be more
  # appropriate to address this in the nutrient data integration code. TODO: talk to
  # Morgan about this
  df_nutr_dt_dups <- df_nutr_c1 |>
    dplyr::add_count(Project, Station_Name, Date_Time, Analyte) |>
    dplyr::filter(n > 1) |>
    dplyr::select(-n) |>
    tidyr::drop_na(Date_Time)

  # Keep the records with the "earliest" Sample_Code
  df_nutr_dt_dups_fixed <- df_nutr_dt_dups |>
    dplyr::arrange(Project, Station_Name, Date_Time, Analyte, Sample_Code) |>
    dplyr::distinct(Project, Station_Name, Date_Time, Analyte, .keep_all = TRUE)

  df_nutr_c2 <- df_nutr_c1 |>
    dplyr::anti_join(df_nutr_dt_dups) |>
    dplyr::bind_rows(df_nutr_dt_dups_fixed)

  # Clean up duplicated records that share same Date and don't have a collection time
  df_nutr_dups_nodt <- df_nutr_c2 |>
    dplyr::add_count(Project, Station_Name, Date, Analyte) |>
    dplyr::filter(n > 1, is.na(Date_Time)) |>
    dplyr::select(-n)

  # Keep the records with the "earliest" Sample_Code
  df_nutr_dups_nodt_fixed <- df_nutr_dups_nodt |>
    dplyr::arrange(Project, Station_Name, Date, Analyte, Sample_Code) |>
    dplyr::distinct(Project, Station_Name, Date, Analyte, .keep_all = TRUE)

  df_nutr_c3 <- df_nutr_c2 |>
    dplyr::anti_join(df_nutr_dups_nodt) |>
    dplyr::bind_rows(df_nutr_dups_nodt_fixed)

  # Filter data so that there is only one sample per station-day by choosing the data
  # point closest to noon
  df_nutr_daily_dups <- df_nutr_c3 |>
    dplyr::add_count(Project, Station_Name, Date, Analyte) |>
    dplyr::filter(n > 1) |>
    dplyr::select(-n)

  df_nutr_daily_dups_fixed <- df_nutr_daily_dups |>
    # Create variable for time and calculate difference from noon for each data point
    dplyr::mutate(
      Time = hms::as_hms(Date_Time),
      Noon_diff = abs(hms::hms(hours = 12) - Time)
    ) |>
    dplyr::group_by(Project, Station_Name, Date, Analyte) |>
    # Select only 1 data point per Station, Date, and Analyte, choose data closest to
    # noon
    dplyr::filter(Noon_diff == min(Noon_diff)) |>
    # When points are equidistant from noon, select earlier point
    dplyr::filter(Time == min(Time)) |>
    dplyr::ungroup() |>
    dplyr::select(-c(Time, Noon_diff))

  df_nutr_c4 <- df_nutr_c3 |>
    dplyr::anti_join(df_nutr_daily_dups) |>
    dplyr::bind_rows(df_nutr_daily_dups_fixed)

  # Add Sign variable to indicate values below the reporting limit and make Result equal
  # to the RL for the <RL values
  df_nutr_c <- df_nutr_c4 |>
    dplyr::mutate(
      Sign = dplyr::if_else(Detection_Condition == "Not Detected", "<", "="),
      Result = dplyr::if_else(
        Detection_Condition == "Not Detected",
        Reporting_Limit,
        Result
      ),
      .keep = "unused"
    ) |>
    # Omit <RL values with reporting limits that are greater than the 95th percentile
    # of the values for the parameter
    dplyr::mutate(q95 = quantile(Result, probs = 0.95), .by = Analyte) |>
    dplyr::filter_out(Sign == "<", Result > q95) |>
    dplyr::select(-q95) |>
    # Rename and create variables to allow for future integration
    dplyr::rename(Parameter = Analyte) |>
    dplyr::mutate(
      Year = lubridate::year(Date),
      Month = lubridate::month(Date)
    )

  return(df_nutr_c)
}
