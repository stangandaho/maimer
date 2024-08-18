#' Remove Specific or All HierarchicalSubject Values from Image Metadata
#'
#'The mm_remove_hs function removes a specific hierarchy from the
#'`Hierarchical Subject` field in an image's metadata, or it removes the entire
#'field if no specific hierarchy is provided. It uses exiftool
#'to manipulate the metadata and ensure that only the desired changes are applied.
#'
#' @inheritParams mm_get_hs
#' @param hierarchy A named character vector, e.g c("Species" = "Vulture") specifying the hierarchy to be removed.
#' If NULL, the entire `Hierarchical Subject` field is removed.
#'
#' @return message indicating image updated
#'
#' @examples
#'
#' # Image path
#' image_path <- file.path(system.file("img", package = "maimer"), "large.jpeg")
#'
#' # Get Hierarchical Subject from the image
#' no_hs <- mm_get_hs(path = image_path)
#' mm_create_hs(image_path, c("A" = "AB"))
#' mm_remove_hs(image_path, c("A" = "AB"))
#'
#' @export
#'
mm_remove_hs <- function(path, hierarchy = NULL) {

  if (is.null(hierarchy)) {
    response <- exifr::exiftool_call(args = " -HierarchicalSubject=", fnames = path, stdout = TRUE)
    return(trimws(response))
  }

  current_hs <- mm_get_hs(path = path)
  hierarchy <- paste0(names(hierarchy), "|", hierarchy)

  if (is.character(hierarchy) & !hierarchy %in% current_hs) {
    return(message(sprintf("Hierarchy %s does not exist. No change applied to %s" ,
                           hierarchy, basename(path = path))))
  }

  if (hierarchy %in% current_hs) {
    updated_hs <- current_hs[current_hs != hierarchy]
    if (length(updated_hs) == 0) {
      updated_hs <- ""
    }

    cmd_update <- sprintf("-HierarchicalSubject='%s'", updated_hs)
    response <- exifr::exiftool_call(args = cmd_update, fnames = path, stdout = TRUE)
  }

  return(trimws(response))
}
