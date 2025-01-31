#' Read a Delimited File into a Tibble
#'
#' The `mm_read` function reads a delimited text file.
#' It automatically detects the delimiter if not specified and provides an easy-to-use
#' interface for importing data with additional customization options.
#'
#' @param file_path A string specifying the path to the file to be read.
#' @param header A logical value indicating whether the file contains a header row. Defaults to `TRUE`.
#' @param sep The field separator character. If not provided, the function automatically detects the separator.
#' @param ... Additional arguments passed to the `read.table` function for fine-tuned control over file reading.
#'
#' @return A tibble containing the data from the specified file.
#'
#' @import dplyr
#' @export

mm_read <- function(file_path,
                    header = TRUE,
                    sep,
                    ...) {

  if (!hasArg(sep)) {
    sep <- check_sep(file_path = file_path)
  }

  data_read <- read.table(file = file_path,
                          sep = sep,
                          header = header,
                          ...)

  return(data_read %>% dplyr::as_tibble())
}
