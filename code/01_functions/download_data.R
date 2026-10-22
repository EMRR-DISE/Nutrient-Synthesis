# DWR Nutrient Synthesis
# Purpose: Functions used to download data via various API's
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Execute an API Call with Exponential Backoff and Jitter Retry Logic. Wraps a downloading or
# data-fetching function inside a retry loop. The function aborts execution immediately if an
# unrecoverable HTTP 404 error is identified, but handles transient network errors by calculating
# an exponential backoff delay paired with a random uniform jitter to prevent server overload.
api_with_retry <- function(api_fn, max_attempts, initial_delay, ...) {
  current_delay <- initial_delay

  for (attempt in 1:max_attempts) {
    # Establish dynamic cli status text
    cli::cli_progress_message(
      "Attempt {attempt}/{max_attempts}: Downloading data..."
    )

    # Evaluate the API expression
    result <- rlang::try_fetch(
      expr = {
        data <- api_fn(...)
        list(status = "success", data = data)
      },
      # Catch and evaluate any error condition object (cnd)
      error = function(cnd) {
        error_msg <- conditionMessage(cnd)

        # Look for explicit HTTP 404 Indicators
        is_404 <- stringr::str_detect(
          error_msg,
          stringr::regex("404|Not Found", ignore_case = TRUE)
        )

        if (isTRUE(is_404)) {
          # Terminate execution immediately for unrecoverable 404 errors
          cli::cli_abort(
            "Server returned {.val 404 Not Found}. Execution halted.",
            parent = cnd
          )
        }

        # Handle all other transient network errors (like partial transfers)
        cli::cli_alert_warning(
          "Attempt {.val {attempt}} failed: {.emph {error_msg}}"
        )
        return(list(status = "transient_error", data = NULL))
      }
    )

    # Evaluate outcomes and handle control flow
    # Return data if successful
    if (result$status == "success") {
      return(result$data)
    }

    # If it is a transient error, execute exponential backoff with jitter
    if (attempt < max_attempts) {
      jitter <- runif(1, 0, 1)
      sleep_time <- current_delay + jitter

      cli::cli_alert_info(
        "Waiting {.val {round(sleep_time, 2)}} seconds before retrying..."
      )
      Sys.sleep(sleep_time)

      current_delay <- current_delay * 2
    } else {
      cli::cli_abort(
        "Download failed permanently after {.val {max_attempts}} attempts."
      )
    }
  }
}

# Download specified data entity from an EDI package
get_edi_data <- function(api_data_id, api_station_id) {
  # Obtain all data entities for EDI data package
  df_data_ent <- api_with_retry(
    api_fn = EDIutils::read_data_entity_names,
    max_attempts = 5,
    initial_delay = 2,
    packageId = api_data_id
  )

  # Define EDI entityId to download
  entity_id <- df_data_ent$entityId[
    which(df_data_ent$entityName == api_station_id)
  ]

  # Download specified entity as a binary file
  data_raw <- api_with_retry(
    api_fn = EDIutils::read_data_entity,
    max_attempts = 5,
    initial_delay = 2,
    packageId = api_data_id,
    entityId = entity_id
  )

  # Import binary file using read_csv
  readr::read_csv(data_raw, show_col_types = FALSE)
}

# Get data entity names for specified EDI ID
get_edi_data_entities <- function(edi_id) {
  df_data_ent <- EDIutils::read_data_entity_names(edi_id)
  rlang::inform(c(
    "i" = paste0(
      "Data entities for ",
      edi_id,
      " include:\n",
      paste(df_data_ent$entityName, collapse = "\n"),
      "\n"
    )
  ))
  return(df_data_ent$entityName)
}

# Download specified data entities from an EDI package and save raw bytes files to a
# temporary directory
get_edi_data <- function(edi_id, entity_names) {
  df_data_ent <- EDIutils::read_data_entity_names(edi_id)
  df_data_ent_filt <- dplyr::filter(df_data_ent, entityName %in% entity_names)

  # Provide message on which data entities will be downloaded
  rlang::inform(c(
    "i" = paste0(
      "Downloading data entities:\n",
      paste(df_data_ent_filt$entityName, collapse = "\n"),
      "\n"
    )
  ))

  ls_data_raw <-
    purrr::map(df_data_ent_filt$entityId, \(x) {
      EDIutils::read_data_entity(edi_id, entityId = x)
    }) |>
    rlang::set_names(df_data_ent_filt$entityName)

  for (i in 1:length(ls_data_raw)) {
    file_raw <- file.path(tempdir(), glue::glue("{names(ls_data_raw)[i]}.bin"))
    con <- file(file_raw, "wb")
    writeBin(ls_data_raw[[i]], con)
    close(con)
  }

  rlang::inform(c(
    "v" = "All files successfully downloaded to temporary directory"
  ))
}

# Download specified data entities from a Science Base item and save files to a
# temporary directory
get_scibase_data <- function(item_id, entity_regex) {
  # Compile all data entities for specified Science Base item ID
  df_sb_files <- sbtools::item_list_files(item_id)
  rlang::inform(c(
    "i" = paste0(
      "Data entities for ",
      item_id,
      " include:\n",
      paste(df_sb_files$fname, collapse = "\n"),
      "\n"
    )
  ))

  # Subset to desired data entities
  sb_ent_sub <- stringr::str_subset(df_sb_files$fname, entity_regex)
  rlang::inform(c(
    "i" = paste0(
      "Downloading data entities:\n",
      paste(sb_ent_sub, collapse = "\n"),
      "\n"
    )
  ))

  # Proceed with downloading desired data entities from Science Base item
  purrr::walk(
    sb_ent_sub,
    \(x) {
      sbtools::item_file_download(
        sb_id = item_id,
        names = x,
        destinations = file.path(tempdir(), x),
        overwrite_file = TRUE
      )
    }
  )

  rlang::inform(c(
    "v" = "All files successfully downloaded to temporary directory"
  ))
}
