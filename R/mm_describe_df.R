#' Descriptive statistic on dataset
#'
#' This function provides a summary of a dataset, including both numeric and
#' non-numeric variables. For numeric variables, it calculates basic descriptive
#' statistics such as minimum, maximum, median, mean, and count of non-missing
#' values. Additionally, users can pass custom functions via the `fn` argument to
#' compute additional statistics for numeric variables. For non-numeric variables,
#' it provides frequency
#' counts and proportions for each unique value.
#'
#' @param data A data frame containing the dataset to be summarized.
#' @param ... (Optional) Column to include in the summary. If no column
#' is specifie, all columns in the data will be included.
#' @param fn A named list of functions to apply to numeric variables. Each function
#' must accept `x` as a vector of numeric values and return a single value or a
#' named vector. Additional arguments for these functions can be specified as a list.
#' For example: `fn = list('sum' = list(na.rm = TRUE), 'sd')`.
#' @return A tibble
#'
#' @examples
#' mm_describe_df(data = data.frame(x = c(1:3, NA),
#'                                  y = c(3:4, NA, NA),
#'                                  z = c("A", "A", "B", "A")),
#'                y, x, z,
#'                fn = list('sum' = list(na.rm = TRUE), 'sd' = list(na.rm = TRUE))
#'               )
#'
#' @seealso parse_list_fn
#'
#' @export
mm_describe_df <- function(data, ..., fn = NULL) {
  if (...length() == 0) {
    col_oi <- colnames(data)
  }else{
    col_oi <- suppressWarnings(colnames(data %>% dplyr::select(...)))
  }

  if (any(!col_oi %in% colnames(data))) {
    missed <- col_oi[!col_oi %in% colnames(data)]
    rlang::abort(paste0("Some column missed: ", paste0(missed, collapse = ", ")))
  }

  # Get var type
  has_numeric <- lapply(col_oi, function(x){
    class(data[[x]])
  }) %>% unlist() %in% c("numeric", "integer")

  alltab <- list()
  if (any(has_numeric)) {
    num_var <- col_oi[has_numeric]

    for_num <- lapply(num_var, function(x){
      vec <- data[[x]]
      summ <- summary(vec)
      dit <- dplyr::tibble(Variable = x,
                           N = length(vec[!is.na(vec)]),
                           Min = summ[[1]],
                           Max = summ[[6]],
                           Median = summ[[3]],
                           Mean = summ[[4]],
                           `CI Left` = mm_ci(vec, side = "left"),
                           `CI Right` = mm_ci(vec, side = "right"))

      # Complete stat columns if necessary
      if (!is.null(fn)) {
        dit <- dit %>% dplyr::bind_cols(parse_list_fn(fn = fn, data = vec))
      }
      dit
    }) %>% dplyr::bind_rows()

    alltab[[1]] <- for_num
    nonnum_var <- col_oi[!has_numeric]
  }else{
    nonnum_var <- col_oi
  }

  # character var
  if (length(nonnum_var) > 0) {
    for_char <- lapply(nonnum_var, function(x){
      acol <- data %>%
        dplyr::filter(!is.na(.data[[x]])) %>%
        dplyr::count(.data[[x]], name = "N") %>%
        dplyr::mutate(
          Prop = round(100 * .data$N / sum(.data$N), 1),
          Variable = x,
          Group = .data[[x]]
        ) %>%
        dplyr::select(Variable, Group, Prop, N)
    }) %>% dplyr::bind_rows()

    if (length(alltab) != 0) {
      alltab[[2]] <- for_char
    }else{
      alltab[[1]] <- for_char
    }

  }

  # Stack and return result
  if (length(alltab) == 2) {
    toreturn <- mm_stack_df(df_list = alltab)
  }else{
    toreturn <- alltab[[1]]
  }

  # Arrange column
  if ("Group" %in% colnames(toreturn)) {
    toreturn <- toreturn %>% dplyr::relocate(Group, Prop, N, .before = 2)
  }

  return(dplyr::tibble(toreturn))
}

#' @noRd
parse_list_fn <- function(fn, data){
  args_names <- fn
  ori_names <- names(fn)

  if (is.null(ori_names)) {
    ori_names <- lapply(fn, function(x)x) %>% unlist()
  }
  if (any(ori_names %in% c(''))) {
    fn_ <- lapply(fn, function(x)x) %>% unlist()
    ori_names[names(fn) %in% c('')] <- fn_[names(fn) %in% c('')]
    fn_names <- ori_names
  }

  all_parser <- list()
  for (nm in ori_names) {
    if (is.null(fn[[nm]])) {
      args <- list('x' = data)
    }else{

      fn[[nm]][['x']] <- data
      args <- fn[[nm]]
    }

    all_parser[[nm]] <- do.call(what = nm, args = args)
  }

  return(dplyr::bind_cols(all_parser))
}
