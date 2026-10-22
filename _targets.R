# DWR Nutrient Synthesis
# Purpose: Targets workflow for creating integrated data used in the SEM models
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

library(targets)
library(tarchetypes)

tar_source("code/01_functions")
tar_source("code/02_data_retrieval_processing")

tar_option_set(
  # Default storage format for data frames (highly efficient)
  format = "qs",
  # Let the pipeline keep moving if a single station produces an error
  error = "continue",
  # Clear out memory after a target runs to keep things efficient
  garbage_collection = TRUE
)
