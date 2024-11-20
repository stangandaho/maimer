#' Stack a list of data frame
#'
#'The function takes a list of data frames and stacks them into a
#'single data frame. It ensures that all columns from the input data frames in the list are
#'included in the output, filling in missing columns with NA values where necessary.
#'
#' @param df_list list of data frame to be stacked
#'
#' @return data frame
#' @examples
#'
#' x <- data.frame(age = 15, fruit = "Apple", weight = 12)
#' y <- data.frame(age = 51, fruit = "Tomato")
#' z <- data.frame(age = 26, fruit = "Lemo", weight = 12, height = 45)
#' alldf <- list(x,y,z)
#' mm_stack_df(alldf)
#' @export
#'

mm_stack_df <- function(df_list) {
  if (!is.list(df_list)) {
    stop(sprintf("Input is %s but should be a plain list of dataframe items to be stacked",
                 class(df_list)[1L]))
  }

  col_size <- list()
  for (cl in seq_along(df_list)) {
    col_size[[cl]] <- colnames(df_list[[cl]])
  }
  uniq_colname <- unique(unlist(col_size))

  rebuilded <- list()
  for (df in seq_along(df_list)) {
    interdf <- list()
    for (cln in uniq_colname) {
      interdf[[cln]] <- ifelse(is.null(df_list[[df]][[cln]]), NA, df_list[[df]][[cln]])
    }
    rebuilded[[df]] <- do.call(cbind, interdf)
  }

  return(
    dplyr::tibble(
      data.frame(do.call(rbind, rebuilded), check.names = FALSE)
    )
  )

}
