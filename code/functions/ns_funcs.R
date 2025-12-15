#' @title Download and read specific files from an EDI data package
#' @description
#' Downloads specified files by name from the latest revision of an EDI data package,
#' and reads them into a named list of dataframes.
#' 
#' @param pkg_id The EDI package ID (e.g., "1015")
#' @param fnames A character vector of filenames to match against package entities
#'
#' @return
#' A named list of dataframes where each name corresponds to a matched filename
#' 
#' @importFrom glue glue
#' @importFrom stringr str_detect str_c
#' @importFrom purrr map_chr map slowly rate_delay keep
#' @importFrom readr read_csv
#' @importFrom EDIutils list_data_entities read_data_entity_name list_data_package_revisions
#' @export
get_edi_file <- function(pkg_id, fname) {
  # get latest revision
  revisions <- list_data_package_revisions(scope = 'edi', identifier = pkg_id)
  latest_revision <- max(as.numeric(revisions))
  package_id_str <- glue('edi.{pkg_id}.{latest_revision}')
  
  # get entity IDs
  entities <- list_data_entities(packageId = package_id_str)
  
  # slow wrapper (avoid rate limit)
  slow_read <- slowly(read_data_entity_name, rate_delay(pause = 1))
  
  # find the matching entity
  matched <- keep(entities, function(entity_id) {
    entity_name <- slow_read(packageId = package_id_str, entityId = entity_id)
    print(entity_name)
    identical(entity_name, fname)
  })
  
  if (length(matched) == 0) {
    stop(glue("File '{fname}' not found in package edi.{pkg_id}.{latest_revision}"))
  }
  
  # construct download URL and read csv
  entity_id <- matched[[1]]
  file_url <- glue('https://pasta.lternet.edu/package/data/eml/edi/{pkg_id}/{latest_revision}/{entity_id}')
  df <- read_csv(file_url, guess_max = 1000000, show_col_types = FALSE)
  
  return(df)
}

#' @title Absolute path to Nutrient Synthesis modeling team folder
#' @description
#' Constructs an absolute path to the shared Nutrient Synthesis modeling directory under the user's home directory.
#' Optionally appends a relative path inside the folder.
#' 
#' @param fp_rel Optional relative path to append within the base PESP directory
#' @return
#' A character string giving the absolute path
#' @export
abs_ns_path <- function(fp_rel = NULL) {
  # construct base path
  base_path <- file.path(Sys.getenv('USERPROFILE'), 'California Department of Water Resources', 'Nutrient synthesis - Documents', 'Modeling Team')
  
  # if relative path is given, append it
  if (is.null(fp_rel)) {
    return(base_path)
  } else {
    return(file.path(base_path, fp_rel))
  }
}