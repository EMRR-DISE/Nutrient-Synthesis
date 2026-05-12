# DWR Nutrient Synthesis
# Purpose: Retrieve full integrated water quality data set from discretewq for years 2009-2024
# and process it for integration with other data for our analyses
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# NOTE: Requires functions from "code/01_functions/global_funcs.R"

get_discretewq <- function() {
  # Import Data -------------------------------------------------------------------------------

  # Make sure we are using `discretewq` version 2.4.0.9000,
  # commit b68425c2eca88015b5e01ae6029f84b2a3af2ea8
  discretewq_ref_target <- "b68425c"
  pak_pkg <- paste0(
    "InteragencyEcologicalProgram/discretewq@",
    discretewq_ref_target
  )

  if (requireNamespace("discretewq", quietly = TRUE)) {
    discretewq_src <- devtools::package_info(
      "discretewq",
      dependencies = FALSE
    )$source

    discretewq_ref <- stringr::str_sub(
      discretewq_src,
      start = stringr::str_locate(discretewq_src, "@")[1] + 1,
      end = stringr::str_locate(discretewq_src, "@")[1] + 7
    )

    if (is.na(discretewq_ref) || discretewq_ref != discretewq_ref_target) {
      pak::pak(pak_pkg)
    }
  } else {
    pak::pak(pak_pkg)
  }

  # Import integrated data set from discretewq
  df_dwq <- discretewq::wq(
    Sources = c(
      "20mm",
      "Baystudy",
      "DJFMP",
      "DOP",
      "EDSM",
      "EMP",
      "FMWT",
      "NCRO",
      "SDO",
      "SKT",
      "SLS",
      "STN",
      "Suisun",
      "USBR",
      "USGS_CAWSC",
      "USGS_SFBS",
      "YBFMP"
    ),
    Start_year = 2009,
    End_year = 2024
  )

  # Prepare Data ------------------------------------------------------------------------------

  # Define water quality parameters to keep from discretewq
  dwq_param <- c(
    "Microcystis",
    "Temperature",
    "Salinity",
    "DissolvedOxygen",
    "pH",
    "TurbidityNTU",
    "TurbidityFNU",
    "Chlorophyll"
  )

  # Prepare data for aggregation
  df_dwq_c1 <- df_dwq |>
    dplyr::select(
      Source,
      Station,
      Latitude,
      Longitude,
      Date,
      Datetime,
      Year,
      Month,
      tidyselect::all_of(dwq_param),
      Chlorophyll_Sign
    ) |>
    # Convert Datetime to PST
    dplyr::mutate(
      Datetime = lubridate::with_tz(Datetime, tzone = "Etc/GMT+8")
    ) |>
    # Remove rows where all parameters are NA
    dplyr::filter_out(dplyr::if_all(tidyselect::all_of(dwq_param), is.na)) |>
    # Remove records without lat-long coordinates
    tidyr::drop_na(Latitude, Longitude) |>
    # Assign regions to the stations
    assign_regions() |>
    # Only keep the stations located in Suisun, Montezuma, and Nurse Sloughs from the
    # Suisun Marsh survey
    dplyr::filter_out(
      Source == "Suisun",
      !stringr::str_detect(Station, "^SU|^MZ|^NS")
    )

  # Pivot the parameter columns to long data structure
  df_dwq_c2 <- df_dwq_c1 |>
    # Add a value suffix to the parameter columns
    dplyr::rename_with(
      \(x) paste0(x, "_Result"),
      tidyselect::all_of(dwq_param)
    ) |>
    # Pivot parameters longer, creating a sign and result column for each parameter
    tidyr::pivot_longer(
      cols = ends_with(c("Sign", "Result")),
      names_to = c("Parameter", ".value"),
      names_pattern = "(.*)_(.*)"
    ) |>
    tidyr::drop_na(Result) |>
    tidyr::replace_na(list(Sign = "=")) |>
    # Rename Microcystis to MVI
    dplyr::mutate(
      Parameter = dplyr::replace_values(Parameter, "Microcystis" ~ "MVI")
    )

  # Consolidate Turbidity parameters and prefer TurbidityNTU when both NTU and FNU were collected
  df_dwq_c3 <- df_dwq_c2 |>
    dplyr::mutate(
      Parameter2 = dplyr::if_else(
        stringr::str_detect(Parameter, "^Turbidity"),
        "Turbidity",
        Parameter
      )
    )

  df_dwq_turb_dups <- df_dwq_c3 |>
    dplyr::filter(Parameter2 == "Turbidity") |>
    dplyr::add_count(Source, Station, Datetime) |>
    dplyr::filter(n > 1) |>
    dplyr::select(-n)

  df_dwq_turb_dups_fixed <- df_dwq_turb_dups |>
    dplyr::filter(Parameter == "TurbidityNTU")

  df_dwq_c4 <- df_dwq_c3 |>
    dplyr::anti_join(df_dwq_turb_dups) |>
    dplyr::bind_rows(df_dwq_turb_dups_fixed) |>
    dplyr::select(-Parameter) |>
    dplyr::rename(Parameter = Parameter2)

  # Remove various duplicated records so that there is only one sample per station-day
  # Clean up the duplicated records that share same Datetime from EDSM
  df_dwq_dt_dups <- df_dwq_c4 |>
    dplyr::add_count(Source, Station, Datetime, Parameter) |>
    dplyr::filter(n > 1) |>
    dplyr::select(-n)

  df_dwq_dt_dups_fixed <- df_dwq_dt_dups |>
    dplyr::distinct(Source, Station, Datetime, Parameter, .keep_all = TRUE)

  df_dwq_c5 <- df_dwq_c4 |>
    dplyr::anti_join(df_dwq_dt_dups) |>
    dplyr::bind_rows(df_dwq_dt_dups_fixed)

  # Filter data so that there is only one sample per station-day by choosing the data
  # point closest to noon
  df_dwq_daily_dups <- df_dwq_c5 |>
    dplyr::add_count(Source, Station, Date, Parameter) |>
    dplyr::filter(n > 1) |>
    dplyr::select(-n)

  df_dwq_daily_dups_fixed <- df_dwq_daily_dups |>
    # Create variable for time and calculate difference from noon for each data point
    dplyr::mutate(
      Time = hms::as_hms(Datetime),
      Noon_diff = abs(hms::hms(hours = 12) - Time)
    ) |>
    dplyr::group_by(Source, Station, Date, Parameter) |>
    # Select only 1 data point per Station, Date, and Parameter, choose data closest to
    # noon
    dplyr::filter(Noon_diff == min(Noon_diff)) |>
    # When points are equidistant from noon, select earlier point
    dplyr::filter(Time == min(Time)) |>
    dplyr::ungroup() |>
    dplyr::select(-c(Time, Noon_diff))

  df_dwq_c6 <- df_dwq_c5 |>
    dplyr::anti_join(df_dwq_daily_dups) |>
    dplyr::bind_rows(df_dwq_daily_dups_fixed)

  # Remove values that are out of range of reasonable limits for the parameter
  df_dwq_c <- df_dwq_c6 |>
    # Temperature value of 2.9 collected on 8/12/2014 in Suisun Marsh
    dplyr::filter_out(Parameter == "Temperature", Result < 3) |>
    # Temperature value of 116 collected on 3/23/2017 at USGS-11447650
    dplyr::filter_out(Parameter == "Temperature", Result > 35) |>
    # DO values greater than 20 (these are obviously % Saturation)
    dplyr::filter_out(Parameter == "DissolvedOxygen", Result > 20) |>
    # pH value of 2.56 collected on 7/2/2014 in the Yolo Bypass
    dplyr::filter_out(Parameter == "pH", Result < 3) |>
    # Two Turbidity values greater than 1000 collected in early Sept 2017 in the South
    # Delta
    dplyr::filter_out(Parameter == "Turbidity", Result > 1000) |>
    # A few Chlorophyll values less than or equal to zero
    dplyr::filter_out(Parameter == "Chlorophyll", Result <= 0)

  return(df_dwq_c)
}
