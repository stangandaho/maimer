#' Fit Random Encounter Model (REM)
#'
#' Fits a random encounter model using observed data and trap rate information.
#' Automatically estimates detection radius, detection angle, animal speed, and
#' activity pattern models if not provided.
#'
#' @param data A data frame of observations, including distance, angle, speed, and time-of-day (in radians).
#' @param traprate_data A data frame created by [mm_traprate_data()].
#' @param radius_model Optional. A detection function model for radius (distance) fitted using [mm_fit_detmodel()].
#' @param angle_model Optional. A detection function model for angle fitted using [mm_fit_detmodel()].
#' @param speed_model Optional. A model for movement speed fitted using [mm_fit_speedmodel()].
#' @param activity_model Optional. An activity model fitted with [activity::fitact()].
#' @param strata Optional. A data frame of stratification information with columns `stratumID` and `area`.
#' @param time_of_day The column name (unquoted or as a string) representing time-of-day in radians.
#' @param n_bootstrap Number of bootstrap replicates for uncertainty estimation. Default is 1000.
#'
#' @return A data frame with columns:
#'   - `parameters`: Model parameter name
#'   - `estimate`: Estimated value
#'   - `se`: Standard error
#'   - `cv`: Coefficient of variation
#'   - `lower_ci`: Lower bound of 95% confidence interval
#'   - `upper_ci`: Upper bound of 95% confidence interval
#'
#' @examples
#' data("camtrapdp")
#' deployments <- camtrapdp$data$deployments
#' observations <- camtrapdp$data$observations %>%
#'   dplyr::filter(scientificName == "Vulpes vulpes") %>%
#'   # Add time of day
#'   dplyr::mutate(time_of_day = mm_to_radian(times = timestamp))
#'
#' # Prepare trap rate data
#' trap_rate <- mm_traprate_data(observation_data = observations,
#'                               deployment_data = deployments,
#'                               deployment_column = deploymentID,
#'                               datetime_column = timestamp,
#'                               start = start, end = 'end'
#' )
#'
#' @seealso [mm_fit_speedmodel()], [mm_fit_detmodel()], [mm_fit_activity()]
#'
#' @export
mm_fit_rem <- function(data,
                       traprate_data,
                       radius_model = NULL,
                       angle_model = NULL,
                       speed_model = NULL,
                       activity_model= NULL,
                       strata = NULL,
                       time_of_day, #A numeric vector of radian time-of-day data.
                       n_bootstrap = 1000
                         ){


  cli::cli_process_start("Fitting radius model")
  if(is.null(radius_model))
    radius_model <- mm_fit_detmodel(radius~1, data, order = 0, truncation = "5%")
  cli::cli_process_done()

  cli::cli_process_start("Fitting angle model")
  if(is.null(angle_model))
    angle_model <- mm_fit_detmodel(angle~1, data, order = 0, unit = "radian")
  cli::cli_process_done()

  cli::cli_process_start("Fitting speed model")
  if(is.null(speed_model))
    speed_model <- mm_fit_speedmodel(formula = speed~1, data = data)
  cli::cli_process_done()

  cli::cli_process_start("Fitting activity model")
  if(is.null(activity_model))
    activity_model <- activity::fitact(dat = data %>% dplyr::pull({{time_of_day}}),
                                       reps = n_bootstrap, sample = "model", show = FALSE)

  cli::cli_process_done()

  cli::cli_process_start("Calculating density")
  parameters <- get_parameter_table(traprate_data,
                                    radius_model,
                                    angle_model,
                                    speed_model,
                                    activity_model,
                                    strata,
                                    n_bootstrap)

  estimates <- rem(parameters) %>%
    convert_units(radius_unit = "m",
                  angle_unit = "degree",
                  active_speed_unit = "km/hour",
                  overall_speed_unit = "km/day")%>%
    dplyr::mutate(dplyr::across(.cols = 1:5, .fns = round, digit = 3))

  # Rowname to column
  rnames <- rownames(estimates)
  rownames(estimates) <- NULL
  estimates <- estimates %>%
    dplyr::mutate(parameters = rnames) %>%
    dplyr::relocate(parameters, .before = 1)

  cli::cli_process_done()
  cli::cli_text("\n\n\n")

  return(dplyr::as_tibble(estimates))
}

#' Fit animal speed model
#'
#' Fits a statistical model to estimate average movement speed of animals.
#' Used in the REM density estimation.
#'
#' @param formula A formula indicating how speed should be modeled (e.g., `speed ~ 1`).
#' @param data A data frame containing speed observations.
#' @param newdata Optional new data to use for prediction.
#' @param distance_unit Unit of distance. One of `"m"`, `"km"`, `"cm"`.
#' @param time_unit Unit of time. One of `"second"`, `"minute"`, `"hour"`, `"day"`.
#' @param ... Additional arguments passed to [sbd::sbm()].
#'
#' @return An object of class `sbm`, with an additional `unit` attribute indicating the speed unit.
#'
#' @examples
#' data("camtrapdp")
#' observations <- camtrapdp$data$observations %>%
#'   dplyr::filter(scientificName == "Vulpes vulpes")
#'
#' mm_fit_speedmodel(speed ~ 1, data = observations)
#'
#' @seealso [mm_fit_rem()], [mm_fit_detmodel()], [mm_fit_activity()]
#'
#' @export
mm_fit_speedmodel <- function(formula = speed ~ 1,
                              data,
                              newdata = NULL,
                              distance_unit = c("m", "km", "cm"),
                              time_unit = c("second", "minute", "hour", "day"),
                           ...){
  distance_unit <- match.arg(distance_unit)
  time_unit <- match.arg(time_unit)
  varnms <- all.vars(formula)

  obs <- data %>%
    dplyr::filter(speed > 0.01 & speed < 10) %>%
    tidyr::drop_na(speed)

  if(nrow(obs) == 0) {cli::cli_abort("There are no usable speed data")}

  res <- sbd::sbm(formula, obs, ...)
  res$unit <- paste(distance_unit, time_unit, sep="/")
  res
}

#' Fit animal detection
#'
#' Fits a detection function (either point or line transect) to model detection radius or angle.
#'
#' @param formula A formula specifying the response (e.g., `radius ~ 1` or `angle ~ covariate`).
#' @param data A data frame containing detection observations.
#' @param newdata Optional new data frame with covariate values for prediction.
#' @param unit Unit of the detection variable. One of `"m"`, `"km"`, `"cm"` for distance,
#' or `"degree"`, `"radian"` for angle.
#' @param ... Additional arguments passed to [Distance::ds()].
#'
#' @inherit Distance::ds return
#'
#' @examples
#' data("camtrapdp")
#' observations <- camtrapdp$data$observations %>%
#'   dplyr::filter(scientificName == "Vulpes vulpes")
#'
#' mm_fit_detmodel(radius ~ 1, data = observations)
#'
#' # For angle
#' mm_fit_detmodel(angle ~ 1, data = observations)
#'
#' @seealso [mm_fit_rem()], [mm_fit_speedmodel()], [mm_fit_activity()]
#'
#' @export

mm_fit_detmodel <- function(formula,
                            data,
                            newdata = NULL,
                            unit = c("m", "km", "cm", "degree", "radian"),
                         ...){
  unit <- match.arg(unit)

  # get and check model variables
  allvars <- all.vars(formula)
  depvar <- allvars[1]
  covars <- allvars[-1]

  not_in <- any(!allvars %in% names(data))
  if(not_in) cli::cli_abort("Can't find {allvars[!allvars %in% names(data)]} variable{?s} in the data.")
  if("distance" %in% covars) cli::cli_abort("Cannot use \"distance\" as a covariate name - rename and try again")

  data <- data %>%
    dplyr::select(dplyr::all_of(allvars)) %>%
    tidyr::drop_na() %>%
    as.data.frame()
  if(nrow(data) == 0) cli::cli_abort("There are no usable position data")

  classes <- dplyr::summarise_all(data, class)
  if(classes[depvar]=="numeric"){
    data <- data %>%
      dplyr::rename(distance = dplyr::all_of(depvar)) %>%
      dplyr::mutate(distance = abs(distance))
  } else{ # in case distance is noted as '12-15'
    cats <- strsplit(as.character(dplyr::pull(data, depvar)), "-")
    data$distbegin <- unlist(lapply(cats, function(x) as.numeric(x[1])))
    data$distend <- unlist(lapply(cats, function(x) as.numeric(x[2])))
    data$distance <- (data$distbegin + data$distend) / 2 # get center
  }

  # model fitting
  type <- if(unit %in% c("m", "km", "cm")) "point" else "line"
  args <- c(data = list(data), formula = formula[-2], transect = type, list(...))
  mod <- suppressWarnings(suppressMessages(do.call(Distance::ds, args)$ddf))

  # esw prediction
  if(length(covars)==0)
    newdata <- data.frame(x=0) else{
      if(is.null(newdata)){
        newdata <- data %>% dplyr::select(dplyr::all_of(covars)) %>%
          lapply(function(x)
            if(is.numeric(x)) mean(x, na.rm=T) else sort(unique(x)))  %>%
          expand.grid()
      } else{
        if(!all(covars %in% names(newdata))) stop("Can't find all model covariates in newdata")
      }}

  prdn <- predict(mod, newdata, esw = TRUE, se.fit = TRUE)
  if(mod$meta.data$point){
    prdn$se.fit <- 0.5 * prdn$se.fit / (pi * prdn$fitted)^0.5
    prdn$fitted <- sqrt(prdn$fitted/pi)
  }
  ed <- cbind(estimate = prdn$fitted, se = prdn$se.fit)
  if(length(covars) >= 1) ed <- cbind(newdata, ed)
  mod$edd <- ed
  mod$unit <- unit
  mod$proportion_used <- nrow(mod$data) / nrow(data)

  return(mod)
}

#' @keywords internal
#' @noRd
rem <- function (parameters){
  required_rows <- c("trap_rate", "overall_speed", "radius",
                     "angle")
  required_cols <- c("estimate", "se", "unit")
  if (!all(required_rows %in% rownames(parameters)) | !all(required_cols %in%
                                                           colnames(parameters))){

  cli::cli_abort("Parameters must have at least {.field {required_rows}} row{?s},
                 and {.field {required_cols}} column{?s}.")
  }


  param <- convert_units(parameters[required_rows, ])
  wtd_est <- param$estimate + c(0, 0, 0, 2)
  pwr_est <- wtd_est^c(1, -1, -1, -1)
  CVs <- param$se/wtd_est
  density <- pi * prod(pwr_est)
  cv <- sqrt(sum(CVs^2))
  se <- density * cv
  ci <- unname(lnorm_confint(density, se))
  parameters["density", "estimate"] <- density
  parameters["density", "se"] <- se
  if ("cv" %in% names(parameters))
    parameters["density", "cv"] <- cv
  if ("lower_ci" %in% names(parameters))
    parameters["density", "lower_ci"] <- ci[1]
  if ("upper_ci" %in% names(parameters))
    parameters["density", "upper_ci"] <- ci[2]
  parameters["density", "unit"] <- "n/km2"
  parameters
}


#' @keywords internal
#' @noRd
get_parameter_table <- function (traprate_data,
                                 radius_model,
                                 angle_model,
                                 speed_model,
                                 activity_model,
                                 strata = NULL,
                                 reps = 999) {
  rad <- radius_model$edd
  ang <- angle_model$edd * 2
  spd <- dplyr::select(speed_model$estimate, dplyr::all_of(c("est",
                                                             "se")))
  act <- activity_model@act[1:2]
  names(act) <- names(spd) <- dimnames(rad)[[2]]
  res <- data.frame(rbind(rad, ang, spd, act))
  ospd <- res[3, 1] * res[4, 1]
  se_ospd <- ospd * sqrt(sum((res[3:4, 2]/res[3:4, 1])^2))
  res <- rbind(res, c(ospd, se_ospd))
  res$cv <- res$se/res$estimate
  res$lower_ci <- res$estimate - 1.96 * res$se
  res$upper_ci <- res$estimate + 1.96 * res$se
  res$n <- c(nrow(radius_model$data), nrow(angle_model$data),
             nrow(speed_model$data), length(activity_model@data),
             NA)
  res$unit <- c(radius_model$unit, angle_model$unit, speed_model$unit,
                "none", speed_model$unit)
  rownames(res) <- c("radius", "angle", "active_speed", "activity_level",
                     "overall_speed")
  traprate <- mm_traprate_estimate(traprate_data, strata, reps)
  j <- c("estimate", "se", "lower_ci", "upper_ci")
  traprate[, j] <- traprate[, j] * radius_model$proportion_used
  res <- rbind(res, traprate)
  convert_units(res)
}


#' @keywords internal
#' @noRd
convert_units <- function (param,
                           radius_unit = c("km", "m", "cm"),
                           angle_unit = c("radian", "degree"),
                           active_speed_unit = c("km/day", "km/hour", "m/hour", "m/second"),
                           overall_speed_unit = c("km/day", "km/hour", "m/hour", "m/second"),
                           trap_rate_unit = c("n/day", "n/100day", "n/hour", "n/minute", "n/second"),
                           density_unit = c("n/km2", "n/ha", "n/100km2"))
{
  uOUT <- c(radius = match.arg(radius_unit), angle = match.arg(angle_unit),
            activity_level = "none", active_speed = match.arg(active_speed_unit),
            overall_speed = match.arg(overall_speed_unit), trap_rate = match.arg(trap_rate_unit),
            density = match.arg(density_unit))
  convert_fields <- c("estimate", "se", "lcl95", "ucl95")
  j <- names(param) %in% convert_fields
  if (sum(j) == 0 | !"unit" %in% names(param))
    stop(paste("Fields in param dataframe must include unit and at least one of:\n",
               paste(head(convert_fields, -1), collapse = ", ")))
  if (!all(rownames(param) %in% names(uOUT)))
    stop(paste("Row names in param dataframe must be among:\n",
               paste(names(uOUT), collapse = ", ")))
  uOUT <- uOUT[rownames(param)]
  m <- get_multiplier(param$unit, uOUT)
  param[, j] <- m * param[, j]
  param$unit <- uOUT
  param
}

#' @keywords internal
#' @noRd
get_multiplier <- function (unitIN, unitOUT)
  {
  lookup <- data.frame(unit = c("cm", "m", "km",
                                "second","minute", "hour", "day", "days", "100day",
                                "radian", "degree",
                                "ha", "km2", "100km2", "n", "none"),
                       mult = c(1, 100, 1e+05, 1, 60, 60^2, 24 * 60^2, 24 * 60^2, 2400 * 60^2, 1, pi/180, 1, 100, 10000, 1, 1),
                       type = rep(c("distance", "time", "angle", "area", "unit"), c(3, 6, 2, 3, 2)))
  recast <- function(u) {
    unlist(strsplit(paste0(u, ifelse(grepl("/", u), "", "/n")),
                    "/"))
  }
  if (length(unitIN) != length(unitOUT))
    stop("unitIN and unitOUT have different lengths")
  if (!all(grepl("/", unitIN) == grepl("/", unitOUT)))
    stop("unitIN and unitOUT have mismatched types")
  uIN <- recast(unitIN)
  uOUT <- recast(unitOUT)
  typeIN <- lookup$type[match(unlist(uIN), lookup$unit)]
  typeOUT <- lookup$type[match(unlist(uOUT), lookup$unit)]
  if (any(is.na(c(typeIN, typeOUT))))
    stop(paste("Units not recognised:", paste(c(uIN, uOUT)[is.na(c(typeIN,
                                                                   typeOUT))], collapse = ", ")))
  if (!all(typeIN == typeOUT))
    stop("unitIN and unitOUT have mismatched types")
  m <- lookup$mult[match(uIN, lookup$unit)]/lookup$mult[match(uOUT, lookup$unit)]
  i <- 2 * (1:length(unitIN))
  m[i - 1]/m[i]
}
