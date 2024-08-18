#' Retrieve Hierarchical Subject Values from Image Metadata
#'
#'The function retrieves the values from the `Hierarchical Subject`
#' field of an image's metadata. It uses exiftool to read the metadata and processes
#' the results to extract and return the unique hierarchical subjects.
#'
#' @param path A character vector specifying the full path of the image file.
#'
#' @return A character vector of unique hierarchical subjects if they exist, otherwise NULL.
#'
#' @examples
#'
#' # Image path
#' image_path <- file.path(system.file("img", package = "maimer"), "large.jpeg")
#'
#' # Get Hierarchical Subject from the image
#' mm_get_hs(path = image_path)
#'
#' @export
#'

mm_get_hs <- function(path){

  current_values <- suppressMessages(
    exifr::exiftool_call(args = "-HierarchicalSubject", fnames = path, intern = TRUE, quiet = TRUE)
  )
  hierar_exist <- "Hierarchical Subject" %in% colnames(get_metadata(path))

  if (hierar_exist) {
    current_subjects <- unlist(lapply(current_values, function(line) {
      linesplit <- strsplit(sub(".*: ", "", line), split = ",")
      linesplit <- unlist(linesplit)
      return(unique(trimws(linesplit)))
    }))
  }else{
    current_subjects <- NULL
  }

  return(current_subjects)
}

