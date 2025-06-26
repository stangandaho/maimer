#' Correct camera trap datetime records
#'
#' This function corrects datetime stamps in camera trap data using a reference
#' correction table. It applies time adjustments based on known timing errors
#' for each camera deployment.
#'
#' @param data A data.frame or tibble containing camera trap records with
#'   datetime information that needs correction.
#' @param datetime Column name (unquoted) in `data` containing the datetime
#'   values to be corrected. Can be character or POSIXct format.
#' @param deployment Column name (unquoted) in both `data` and
#'   \code{corrector} that identifies unique camera deployments (e.g., camera ID,
#'   site name, or deployment identifier).
#' @param corrector A data.frame containing correction information with columns:
#'   \itemize{
#'     \item deployment column matching the deployment parameter
#'     \item `sign` - character indicating correction direction ("+" or "-")
#'     \item `datetimes` - reference datetime showing the correct time
#'   }
#' @param format Optional datetime format specification. Can be:
#'   \itemize{
#'     \item `NULL` (default) - attempts multiple common formats
#'     \item Single format string - used for both `data` and `corrector` datetimes
#'     \item Vector of 2 format strings - first for data, second for corrector
#'   }
#'
#'
#' @return A data.frame with the original data plus additional columns:
#'   \itemize{
#'     \item \code{corrected_datetime} - corrected datetime as POSIXct
#'     \item \code{correction_applied} - sign of correction applied
#'     \item \code{time_offset_seconds} - magnitude of correction in seconds
#'     \item \code{corrector_reference} - reference datetime used for correction
#'   }
#'
#' @examples
#' # Load camera trap data
#' library(dplyr)
#'
#' camtrap_data <- read.csv(maimer:::table_files()[1]) %>%
#'   dplyr::filter(project == "Last")
#'
#' # Create correction table
#' # CAMERA 1 was running slow (+), CAMERA 2 was running fast (-)
#' crtor <- data.frame(
#'   camera = c("CAMERA 1", "CAMERA 2"),
#'   sign = c("+", "-"),
#'   datetimes = c("2025-03-14 8:17:00", "2024-11-14 10:02:03")
#' )
#'
#' # Apply datetime corrections
#' mm_correct_datetime(
#'   data = camtrap_data,
#'   datetime = datetimes,
#'   deployment = camera,
#'   corrector = crtor
#' )
#'
#' @export
mm_correct_datetime <- function(data,
                                datetime,
                                deployment,
                                corrector,
                                format = NULL) {

  # Validate inputs
  if (!inherits(data, "data.frame")) {
    cli::cli_abort("{.strong {deparse(substitute(data))}} must be a {.cls data.frame} or {.cls tibble} object.")
  }

  if (!inherits(corrector, "data.frame")) {
    cli::cli_abort("{.strong {deparse(substitute(corrector))}} must be a {.cls data.frame} or {.cls tibble} object.")
  }

  # Check required columns in corrector
  required_cols <- c(deparse(substitute(deployment)), "sign", "datetimes")
  missing_cols <- setdiff(required_cols, names(corrector))
  if (length(missing_cols) > 0) {
    cli::cli_abort("{.strong {deparse(substitute(corrector))}} has {length(missing_cols)} missing required column{?s}: {.field {missing_cols}}")
  }

  # Get deployment identifiers
  deployment_col <- deparse(substitute(deployment))
  datetime_col <- deparse(substitute(datetime))

  # Check for unique deployments in corrector
  corrector_deployments <- corrector[[deployment_col]]
  if (length(unique(corrector_deployments)) != nrow(corrector)) {
    cli::cli_abort("{.strong {deparse(substitute(corrector))}} must have unique identifiers in {.field {deployment_col}} column.")
  }

  # Process each deployment
  corrected_data_list <- lapply(corrector_deployments, function(deploy_id) {

    # Filter data for current deployment
    current_data <- data %>%
      dplyr::filter(!!sym(deployment_col) == deploy_id)

    if (nrow(current_data) == 0) {
      cli::cli_warn("No data found for deployment: {.strong {deploy_id}}")
      return(NULL)
    }

    # Get correction info for this deployment
    correction_info <- corrector %>%
      dplyr::filter(!!sym(deployment_col) == deploy_id)

    # Parse datetime columns
    if (!is.null(format)) {
      if (length(format) == 1) {
        # Same format for both data and corrector
        data_datetime <- as.POSIXct(current_data[[datetime_col]], format = format)
        corrector_datetime <- as.POSIXct(correction_info$datetimes, format = format)
      } else if (length(format) == 2) {
        # Different formats for data and corrector
        data_datetime <- as.POSIXct(current_data[[datetime_col]], format = format[1])
        corrector_datetime <- as.POSIXct(correction_info$datetimes, format = format[2])
      } else {
        cli::cli_abort("Format must be NULL, a single format string, or a vector of 2 format strings.")
      }
    } else {
      # Try multiple formats
      data_datetime <- as.POSIXct(current_data[[datetime_col]], tryFormats = try_formats)
      corrector_datetime <- as.POSIXct(correction_info$datetimes, tryFormats = try_formats)
    }

    # Check for parsing failures
    if (any(is.na(data_datetime))) {
      cli::cli_warn("Failed to parse some datetimes in data for deployment: {.strong {deploy_id}}")
    }
    if (any(is.na(corrector_datetime))) {
      cli::cli_warn("Failed to parse corrector datetime for deployment: {.strong {deploy_id}}")
    }

    # Calculate time difference (corrector shows what the correct time should be)
    correction_sign <- trimws(correction_info$sign)

    # Sort data_datetime to use earliest timestamp as reference point
    # This ensures consistent offset calculation regardless of data order
    sorted_datetime <- sort(data_datetime, na.last = TRUE)
    reference_datetime <- sorted_datetime[1]  # Use earliest timestamp

    # The corrector datetime represents the correct time at a reference point
    # Calculate the offset between actual camera time and correct time
    time_diff <- as.numeric(difftime(corrector_datetime, reference_datetime, units = "secs"))

    if (correction_sign == "+") {
      # Camera is running slow - add the calculated difference
      corrected_datetime <- data_datetime + abs(time_diff)
      time_offset <- abs(time_diff)
    } else if (correction_sign == "-") {
      # Camera is running fast - subtract the calculated difference
      corrected_datetime <- data_datetime - abs(time_diff)
      time_offset <- abs(time_diff)
    } else {
      cli::cli_abort("Invalid sign '{.strong {correction_sign}}' for deployment {.strong {deploy_id}}.")
      corrected_datetime <- data_datetime
      time_offset <- 0
    }

    # Add corrected datetime and metadata to data
    result <- current_data %>%
      dplyr::mutate(
        corrected_datetime = corrected_datetime,
        correction_applied = correction_sign,
        time_offset_seconds = time_offset,
        corrector_reference = corrector_datetime
      )

    return(result)
  })

  # Remove NULL results and combine
  corrected_data_list <- corrected_data_list[!sapply(corrected_data_list, is.null)]

  if (length(corrected_data_list) == 0) {
    cli::cli_warn("No corrections could be applied.")
    return(data)
  }

  # Combine all corrected data
  final_data <- bind_rows(corrected_data_list)

  return(final_data)
}
