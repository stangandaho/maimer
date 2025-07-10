#' @inherit activity::fitact title
#'
#' @inherit activity::fitact description
#'
#' @param time_of_day A numeric vector of radian time-of-day data
#' @param weights A numeric vector of weights for each dat value.
#' @param n_boostrap Number of bootstrap iterations to perform. Ignored if sample=="none"
#' @param bandwidth Numeric value for kernel bandwidth. If NULL, calculated internally.
#' @param adjustment Numeric bandwidth adjustment multiplier.
#'
#' @inheritParams activity::fitact
#'
#' @inherit activity::fitact details
#'
#' @return A list
#' @export
mm_fit_activity <- function(time_of_day,
                            weights = NULL,
                            n_boostrap = 1000,
                            bandwidth  = NULL,
                            adjustment = 1,
                            sample = c("none", "data", "model"),
                            bounds = NULL,
                            show = TRUE
                            ) {
  fit_act <- activity::fitact(dat = time_of_day,
                              wt = weights,
                              reps = n_boostrap,
                              bw = bandwidth,
                              adj = adjustment,
                              sample = sample,
                              bounds = bounds,
                              show = show)

  return(list(
    Data = dplyr::tibble(time_of_day = fit_act@data),
    Weight = fit_act@wt,
    Bandwidth = fit_act@bw,
    Adjustement = fit_act@adj,
    `Probabilty Density Function` = dplyr::as_tibble(fit_act@pdf),
    Activity = t(fit_act@act) %>% as.data.frame() %>% dplyr::as_tibble()
  ))
}

