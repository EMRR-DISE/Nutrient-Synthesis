# DWR Drought Synthesis
# Purpose: Retrieve full integrated water quality data set from discretewq for years 2009-2024
# and save copy in data/external for continued processing
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

library(here)
library(stringr)

# Install devtools if its not installed already
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Make sure we are using `discretewq` version 2.4.0.9000, commit b68425c2eca88015b5e01ae6029f84b2a3af2ea8
discretewq_ref_target <- "b68425c"
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

  if (discretewq_ref != discretewq_ref_target) {
    devtools::install_github(
      "InteragencyEcologicalProgram/discretewq",
      ref = discretewq_ref_target
    )
  }
} else {
  devtools::install_github(
    "InteragencyEcologicalProgram/discretewq",
    ref = discretewq_ref_target
  )
}

library(discretewq)

# Import integrated data set from discretewq
df_dwq <- wq(
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

# Save local copy of data
df_dwq %>% saveRDS(here("data/external/discretewq_2009_2024.rds"))
