#' Calculate camera trap deployment effort
#'
#' Computes the monitoring effort (e.g., in days) for each camera deployment
#' based on start and end timestamps.
#'
#' @param deployment_data A data frame containing camera trap deployment records.
#' @param start_column The column name (unquoted or as a string) indicating deployment start datetime.
#' @param end_column The column name (unquoted or as a string) indicating deployment end datetime.
#' @param deployment_column The column name (unquoted or as a string) that uniquely identifies the deployment (e.g., camera ID).
#' @param format A character string specifying the format of the datetime columns. Default is `"%Y-%m-%d %H:%M:%OS"`.
#' @param time_zone The time zone used to parse the datetime columns. Default is `""` (i.e., system time zone).
#' @param time_unit The unit in which to compute the effort duration.
#' Can be `"secs"`, `"mins"`, `"hours"`, `"days"`, or `"weeks"`. Default is `"days"`.
#'
#' @return A data frame with columns:
#'   - `deployment_column`: Deployment identifier
#'   - `effort`: Numeric value of monitoring effort
#'   - `effort_unit`: The time unit used
#'
#' @examples
#' data("camtrapdp")
#' deployments <- camtrapdp$data$deployments
#' mm_get_effort(deployment_data = deployments,
#'               deployment_column = deploymentID,
#'               start_column = start,
#'               end_column = end)
#'
#' @seealso [mm_traprate_data()]
#'
#' @export
mm_get_effort <- function(deployment_data, # camera trap deployement data
                          start_column, # column of deployment start datetime
                          end_column, #column of deployment end datetime
                          deployment_column, # column of deployment ID (i.e camera ID column)
                          format = "%Y-%m-%d %H:%M:%OS",
                          time_zone = "",
                          time_unit = "days") {
  # Normalize column names
  has_arg <- c(hasArg(start_column), hasArg(end_column), hasArg(deployment_column))
  rq <- c("start_column", "end_column", "deployment_column")
  if (any(!has_arg)) {
    cli::cli_abort("{.code {rq[!has_arg]}} must be specified.")
  }
  start_col <- as_colname(rlang::enquo(start_column))
  end_col <- as_colname(rlang::enquo(end_column))
  deployment_col <- as_colname(rlang::enquo(deployment_column))

  effort <- deployment_data %>%
    dplyr::mutate(
      .start = as.POSIXlt(.data[[start_col]], format = format, tz = time_zone),
      .end = as.POSIXlt(.data[[end_col]], format = format, tz = time_zone),
      effort = as.numeric(difftime(.end, .start, units = time_unit)),
      effort_unit = time_unit
    ) %>%
    dplyr::select(dplyr::all_of(c(deployment_col, "effort", "effort_unit")))
  return(effort)
}


#' Prepare data for trap rate estimation
#'
#' Calculates observation counts and associated monitoring effort per deployment
#' to support trap rate estimation.
#'
#' @param observation_data A data frame of detection records (e.g., camera trap images or events).
#' @param use_deployment Logical. If `TRUE` (default), effort is derived from deployment data.
#' If `FALSE`, effort is estimated from observation timestamps.
#' @param deployment_data Optional. A data frame of deployment metadata; required if `use_deployment = TRUE`.
#' @param deployment_column The column name (unquoted or as a string) that uniquely identifies the deployment (e.g., camera ID).
#' @param start_column Optional. Start datetime column in the deployment data. Required if `use_deployment = TRUE`.
#' @param end_column Optional. End datetime column in the deployment data. Required if `use_deployment = TRUE`.
#' @param datetime_column Optional. The datetime column in `observation_data`; used if `use_deployment = FALSE`.
#' @param format A character string specifying the format of the datetime columns. If `NULL`, defaults to ISO 8601 format.
#' @param time_zone The time zone used to parse datetime values. Default is `""` (i.e., system time zone).
#' @param time_unit Unit of time to compute effort and trap rate.
#' One of `"secs"`, `"mins"`, `"hours"`, `"days"`, or `"weeks"`. Default is `"days"`.
#'
#' @return A data frame with columns:
#'   - `deployment_column`: Deployment identifier
#'   - `n`: Number of observations per deployment
#'   - `effort`: Monitoring duration
#'   - `effort_unit`: Time unit used for effort
#'
#' @examples
#' data("camtrapdp")
#' deployments <- camtrapdp$data$deployments
#' observations <- camtrapdp$data$observations %>%
#'                   dplyr::filter(scientificName == "Vulpes vulpes")
#'
#' mm_traprate_data(observation_data = observations,
#'                  deployment_data = deployments,
#'                  use_deployment = TRUE,
#'                  deployment_column = deploymentID,
#'                  datetime_column = timestamp,
#'                  start = start, end = 'end'
#'                  )
#'
#' @seealso [mm_get_effort()]
#' @export
mm_traprate_data <- function(observation_data,
                             use_deployment = TRUE,
                             deployment_data = NULL,
                             deployment_column,
                             start_column = NULL,
                             end_column = NULL,
                             datetime_column = NULL,
                             format = NULL,
                             time_zone = "",
                             time_unit = "days") {
  if (use_deployment && is.null(deployment_data)) {
    cli::cli_abort("Deployment data must be provided if {.code use_deployment = TRUE}")
  }

  # Handle both string and unquoted symbol inputs
  deployment_col <- as_colname(rlang::enquo(deployment_column))

  if (use_deployment) {
    start_col <- as_colname(rlang::enquo(start_column))
    end_col <- as_colname(rlang::enquo(end_column))

    effort <- mm_get_effort(
      deployment_data = deployment_data,
      start_column = {{start_col}},
      end_column = {{end_col}},
      deployment_column = {{deployment_col}},
      format = format,
      time_zone = time_zone,
      time_unit = time_unit
    )
    dpl_name <- rlang::as_name(deployment_col)
    record_count <- observation_data %>%
      dplyr::summarise(n = dplyr::n(), .by = dpl_name)

    traprate_data <- record_count %>%
      dplyr::left_join(y = effort, by = setNames(dpl_name, dpl_name)) %>%
      dplyr::select(dplyr::all_of(c(deployment_col, "n", "effort", "effort_unit")))
  } else {
    # If else use only observation to derive start and end datetime of
    # deployment. This method can underestimate the trap rate.
    datetime_col <- as_colname(rlang::enquo(datetime_column))

    n_individual <- suppressWarnings({
      mm_summarise_camtrap_activity(
        data = observation_data,
        deployment = {{deployment_col}},
        datetime = {{datetime_col}},
        threshold = 0,
        time_unit = time_unit,
        format = format
      )
    }) %>%
      dplyr::select(dplyr::all_of(c(deployment_col, 'total_duration', "n_records"))) %>%
      dplyr::mutate(effort_unit = time_unit)

    traprate_data <- n_individual %>%
      dplyr::select(dplyr::all_of(c(deployment_col, "n_records", "total_duration", "effort_unit"))) %>%
      dplyr::rename(n = n_records, effort = total_duration)
  }
  return(traprate_data)
}



#' Estimate trap rate
#'
#' Computes the estimated trap rate and uncertainty using bootstrapping, with optional
#' support for stratified estimation based on area-weighted averaging.
#'
#' @param data A data frame as returned by [mm_traprate_data()] with columns `n` and `effort`.
#' @param strata Optional. A data frame defining strata, with columns `stratumID` and `area`.
#' @param n_bootstrap Number of bootstrap replicates to estimate uncertainty. Default is 1000.
#'
#' @return A data frame with the following columns:
#'   - `estimate`: Trap rate estimate (e.g., detections per day)
#'   - `se`: Standard error of the estimate
#'   - `cv`: Coefficient of variation
#'   - `lower_ci`: Lower bound of the 95\% confidence interval
#'   - `upper_ci`: Upper bound of the 95\% confidence interval
#'   - `n`: Number of deployments or observation used
#'   - `unit`: Effort unit
#'
#' @examples
#'
#' data("camtrapdp")
#' deployments <- camtrapdp$data$deployments
#' observations <- camtrapdp$data$observations %>%
#'                   dplyr::filter(scientificName == "Vulpes vulpes")
#'
#' trap_rate <- mm_traprate_data(observation_data = observations,
#'                               deployment_data = deployments,
#'                               use_deployment = FALSE,
#'                               deployment_column = deploymentID,
#'                               datetime_column = timestamp,
#'                               start = start, end = 'end'
#' )
#'
#' mm_traprate_estimate(data = trap_rate, n_bootstrap = 1000)
#'
#' @seealso [mm_get_effort()], [mm_traprate_data()]
#'
#' @export
mm_traprate_estimate <- function(data, # as outputted by mm_traprate_data
                                 strata = NULL,
                                 n_bootstrap = 1000
                                 )
  {
  traprate_data <- data
  traprate <- function(dat) {
    if (is.null(strata)) {
      sum(dat$n)/sum(dat$effort)
    }
    else {
      local_density <- sapply(strata$stratumID, function(stratum) {
        i <- dat$stratumID == stratum
        sum(dat$n[i])/sum(dat$effort[i])
      })
      sum(local_density * strata$area)/sum(strata$area)
    }
  }

  sampled_traprate <- function() {
    i <- if (is.null(strata))
      sample(1:nrow(traprate_data), replace = TRUE)
    else unlist(sapply(strata$stratumID, function(stratum) {
      sample(which(traprate_data$stratumID == stratum),
             replace = TRUE)
    }))
    traprate(traprate_data[i, ])
  }
  if (!all(c("effort", "n") %in% names(traprate_data)))
    cli::cli_abort("data must contain (at least) columns `effort` and observations `n`")
  if (!is.null(strata)) {
    if (!"stratumID" %in% names(traprate_data))
      cli::cli_abort("data must contain column `stratumID` for stratified analysis")
    if (!all(c("stratumID", "area") %in% names(strata)))
      cli::cli_abort("strata must contain columns stratumID and area")
    if (!all(traprate_data$stratumID %in% strata$stratumID))
      cli::cli_abort("Not all strata in data are present in strata")
  }
  tr_sample <- replicate(n_bootstrap, sampled_traprate())
  est <- traprate(traprate_data)
  se <- sd(tr_sample)
  cv <- se/est
  ci <- unname(quantile(tr_sample, c(0.025, 0.975)))
  data.frame(estimate = est,
             se = se,
             cv = cv,
             lower_ci = ci[1],
             upper_ci = ci[2],
             n = nrow(traprate_data),
             unit = paste("n",traprate_data$effort_unit[1], sep = "/"),
             row.names = "trap_rate")
}
