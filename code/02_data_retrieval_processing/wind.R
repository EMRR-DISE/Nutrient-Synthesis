# DWR Nutrient Synthesis
# Purpose: Process wind data compiled by Norman Johns for integration with other data
# for our analyses
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

get_wind_data <- function() {
  # Import data:
  # Monthly values for each region is in a separate csv file
  # Define file paths
  fp_wind_mo <- dir(
    here::here("data/raw"),
    pattern = "Wind_[[:alpha:]]{4}_monthly.+\\.csv$",
    full.names = TRUE
  )

  # Import each csv file into a nested dataframe
  ndf_wind_mo <- tibble::tibble(
    fp = fp_wind_mo,
    Region = stringr::str_extract(fp, "(?<=Wind_)[:alpha:]{4}(?=_monthly)"),
    df_data = purrr::map(fp, readr::read_csv)
  )

  # Prepare data:
  df_wind_mo <- ndf_wind_mo %>%
    dplyr::mutate(
      Region = dplyr::recode_values(
        Region,
        "Conf" ~ "Confluence",
        "Nort" ~ "North",
        "SBay" ~ "Suisun Bay",
        "SCen" ~ "SouthCentral",
        "SMar" ~ "Suisun Marsh"
      ),
      df_data = purrr::map(
        df_data,
        \(x) {
          dplyr::rename_with(
            x,
            \(y) stringr::str_extract(y, "(?<=[:alpha:]{4}-)[:alpha:]+$"),
            tidyselect::ends_with(c("mean", "max"))
          )
        }
      ),
      .keep = "none"
    ) %>%
    tidyr::unnest(df_data) %>%
    dplyr::select(
      Region,
      date,
      max = Wmax,
      tidyselect::ends_with(c("AvLt3.0", "AvLt4.5"))
    ) %>%
    dplyr::rename_with(\(x) paste0("Wind_", x), tidyselect::where(is.numeric)) %>%
    dplyr::mutate(
      Year = lubridate::year(date),
      Month = lubridate::month(date),
      .keep = "unused"
    )

  return(df_wind_mo)
}
