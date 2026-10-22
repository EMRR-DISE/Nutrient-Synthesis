# DWR Nutrient Synthesis
# Purpose: Custom targets factory for downloading and processing data used in the SEM models
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

#' Custom Target Factory for Dynamic Data Sources
#'
#' Generates a paired raw download target and a cleaning target for a single data source.
#'
#' @param name Base name for the targets (e.g., "water_quality" creates raw_water_quality and
#'   clean_water_quality).
#' @param fetch_fn Function to download raw data (unquoted name).
#' @param process_fn Function to clean and standardize raw data (unquoted name).
#' @param config_target Optional string name of a central config target
#'
#' @return A list of two `tar_target_raw` objects to append to a targets pipeline list.
tar_data_source <- function(name, fetch_fn, process_fn, config_target = NULL) {
  # Build target symbols from strings
  raw_sym <- rlang::sym(paste0("raw_", name))
  clean_sym <- rlang::sym(paste0("clean_", name))

  # Capture unquoted function names as expressions
  fetch_expr <- rlang::enexpr(fetch_fn)
  process_expr <- rlang::enexpr(process_fn)

  # Capture source-specific arguments
  fetch_args <- list()
  process_args <- list(raw_data = raw_sym)

  # Attach global config target symbol if provided
  if (!is.null(config_target)) {
    config_sym <- rlang::sym(config_target)
    fetch_args$config <- config_sym
    process_args$config <- config_sym
  }

  # Construct function call expressions: fetch_fn(..., config = global_config)
  fetch_call <- rlang::call2(fetch_expr, !!!fetch_args)
  process_call <- rlang::call2(process_expr, !!!process_args)

  # Return targets list
  list(
    targets::tar_target_raw(rlang::as_string(raw_sym), fetch_call),
    targets::tar_target_raw(rlang::as_string(clean_sym), process_call)
  )
}
