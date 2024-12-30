#' Estimate Overlap Coefficients for Multiple Species
#'
#' This function calculates pairwise overlap coefficients for activity patterns of multiple species
#' using their time data.
#'
#' @param data A `data.frame` or `tibble` containing species and time information.
#' @param species_column A column in `data` indicating species names.
#' @param time_column A column in `data` containing time data (either as radians or in a time format to be converted).
#' @param convert_time Logical. If `TRUE`, the time data will be converted to radians using the `mm_to_radian` function.
#' @param format A character string specifying the time format (e.g., `"%H:%M:%S"`) if [mm_to_radian()] is `TRUE`. Defaults to `"%H:%M:%S"`.
#' @param fill_na Optional. A numeric value used to fill `NA` values in the overlap coefficient matrix. Defaults to `NULL` (does not fill `NA` values).
#' @param ... Additional arguments passed to [overlap::overlapEst()]` for overlap estimation.
#'
#' @details
#' The function calculates pairwise overlap coefficients for all species in the dataset.
#' The overlap coefficients are estimated using the `overlap` package:
#' - For species pairs with sample sizes of at least 50 observations each, the `Dhat4` estimator is used.
#' - For smaller sample sizes, the `Dhat1` estimator is used (Schmid & Schmidt, 2006).
#'
#' @return A square matrix of pairwise overlap coefficients, where rows and columns represent species.
#'
#' @examples
#' # Example dataset
#' data <- data.frame(
#'   species = c("SpeciesA", "SpeciesA", "SpeciesB", "SpeciesB"),
#'   time = c("10:30:00", "11:45:00", "22:15:00", "23:30:00")
#' )
#'
#' # Calculate overlap coefficients with time conversion
#' overlap_matrix <- mm_overlap_matrix(
#'   data = data,
#'   species_column = species,
#'   time_column = time,
#'   convert_time = TRUE,
#'   format = "%H:%M:%S"
#' )
#'
#' # Fill missing values in the matrix with 0
#' overlap_matrix_filled <- mm_overlap_matrix(
#'   data = data,
#'   species_column = species,
#'   time_column = time,
#'   convert_time = TRUE,
#'   fill_na = 0
#' )
#'
#'@references
#'Schmid & Schmidt (2006) Nonparametric estimation of the coefficient of
#'overlapping - theory and empirical application, Computational Statistics and
#'Data Analysis, 50:1583-1596.
#'
#' @seealso [overlap::overlapEst()] for overlap coefficient estimation.
#'
#' @export
#'
#'
mm_overlap_matrix <- function(data,
                              species_column,
                              time_column,
                              convert_time = F,
                              format = "%H:%M:%S",
                              fill_na = NULL,
                              ...){

  sp_col_ <- paste0(dplyr::ensym(species_column))
  tm_col_ <- paste0(dplyr::ensym(time_column))

  if (!sp_col_%in% colnames(data) ) {
    stop(sprintf("%s not found in the data", sp_col_))
  }
  if (!tm_col_%in% colnames(data) ) {
    stop(sprintf("%s not found in the data", tm_col_))
  }

  if (convert_time) {
    data[[tm_col_]] <- mm_to_radian(times = data[[tm_col_]],
                                          format = format)
  }

  data <- data %>% dplyr::select(dplyr::all_of(c(sp_col_, tm_col_)))

  sp_tm <- list() # list to store species and time
  for(sp in unique(data[[sp_col_]])){
    each_sp <- data %>%
      dplyr::filter(!!dplyr::sym(sp_col_) == sp)
    sp_tm[[sp]] <- each_sp %>% dplyr::pull(tm_col_)

  }
  # Match length of items in the sp_tm list
  lens <- sapply(sp_tm, length)

  data <- lapply(sp_tm,
                    FUN = function(x){
                      c(x, rep(NA, max(lens) - length(x)))}
                 ) %>%
    dplyr::bind_cols()

  ## Create matrix to store coeficient
  coef_matrix <- matrix(data = rep(0, ncol(data)^2),
                        ncol = ncol(data), nrow = ncol(data))
  colnames(coef_matrix) <- colnames(data)
  rownames(coef_matrix) <- colnames(data)

  #return(data)

  # select pair of column
  for (first_sp in colnames(data)) {
    for (second_sp in colnames(data)){

      if (first_sp != second_sp) {
        spA <- data[[first_sp]][!is.na(data[[first_sp]])]
        spB <- data[[second_sp]][!is.na(data[[second_sp]])]

        if ((length(spA) >= 50 ) && (length(spB) >= 50)) {
          overlap_coef <- overlap::overlapEst(A = spA,
                                              B = spB,
                                              type = "Dhat4",
                                              ...)
        }else{
          overlap_coef <- overlap::overlapEst(A = spA,
                                              B = spB,
                                              type = "Dhat1",
                                              ...)
        }

        coef_matrix[second_sp, first_sp] <- round(overlap_coef, 3)

      }
    }
  }

  if (!is.null(fill_na)) {
    if(! class(fill_na) %in% c("numeric", "double")){stop("'fill_na' must be a numeric")}
    for (c_ in 1:ncol(coef_matrix)) {
      for (r_ in 1:nrow(coef_matrix)) {
        if (is.na(coef_matrix[c_, r_])) {
          coef_matrix[c_, r_] <- fill_na
        }
      }
    }
  }

  return(coef_matrix)
}


