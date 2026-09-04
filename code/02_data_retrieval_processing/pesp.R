# DWR Nutrient Synthesis
# Purpose: Retrieve integrated PESP data set and process it using PESP recommendations for
# integration with other data for our analyses
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# NOTE: Requires functions from "code/01_functions/global_funcs.R"

get_pesp <- function() {
  # Import Data -------------------------------------------------------------------------------

  # PESP enumeration data from the EDI package
  edi_id_pesp <- "edi.2209.2"
  edi_data_ent_pesp <- get_edi_data_entities(edi_id_pesp)
  edi_data_ent_pesp_enum <- stringr::str_subset(
    edi_data_ent_pesp,
    "enumeration"
  )
  get_edi_data(edi_id_pesp, edi_data_ent_pesp_enum)

  temp_files <- list.files(tempdir(), full.names = TRUE)
  df_pesp <- readr::read_csv(
    stringr::str_subset(
      temp_files,
      edi_data_ent_pesp_enum
    )
  )

  # Prepare data -------------------------------------------------------------------------------

  surveys <- c(
    "DWR-EMP",
    "CDFW-FRP",
    "DWR-AWCA",
    "CDFW-FMWT",
    "DWR-YBFMP",
    "DWR-NDFS"
  )

  # Filter to proper specifications for each survey
  ls_pesp <- df_pesp |>
    dplyr::filter(
      Survey %in% surveys,
      Lab == "BSA",
      CountMethodTaxa == "field"
    ) |>
    tidyr::nest(.by = Survey) |>
    tibble::deframe()

  df_pesp_c <- ls_pesp |>
    # EMP
    purrr::map_at(
      "DWR-EMP",
      \(x) {
        dplyr::filter(
          x,
          SampleMethod == "centrifugal pump",
          Magnification == "800x"
        )
      }
    ) |>
    # FRP, AWCA, and FMWT
    purrr::map_at(
      c("CDFW-FRP", "DWR-AWCA", "CDFW-FMWT"),
      \(x) {
        dplyr::filter(
          x,
          SampleMethod == "unspecified surface grab",
          Magnification == "800x"
        )
      }
    ) |>
    # YBFMP and NDFS
    purrr::map_at(
      c("DWR-YBFMP", "DWR-NDFS"),
      \(x) {
        dplyr::filter(
          x,
          SampleMethod == "water sampler dipper",
          Magnification == "630x"
        )
      }
    ) |>
    purrr::list_rbind(names_to = "Survey")

# Prepare data for aggregation
df_pesp_c <- df_pesp_c |>
  # Assign regions to stations
  tidyr::drop_na(Latitude, Longitude) |>
  assign_regions() |>
  # Add carbon biomass conversions and month, year variables
  dplyr::mutate(
    C_biomass = dplyr::case_when(
      AlgalGroup == 'Diatoms' ~
        (0.288 * (Biovolume_per_mL / Cells_per_mL)^0.811 * Cells_per_mL) / 1000,
      AlgalGroup == 'Dinoflagellates' ~
        (0.760 * (Biovolume_per_mL / Cells_per_mL)^0.819 * Cells_per_mL) / 1000,
      TRUE ~
        (0.216 * (Biovolume_per_mL / Cells_per_mL)^0.939 * Cells_per_mL) / 1000
    ),
    Month = lubridate::month(Date),
    Year = lubridate::year(Date)
  )

  return(df_pesp_c)
}
