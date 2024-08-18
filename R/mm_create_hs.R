#' Create or Add Hierarchical Subject Values in Image Metadata
#'
#' @inheritParams mm_get_hs
#' @param value named character vector specifying the new hierarchical subjects to add.
#' Each value must have a parent specified as the name, e.g c("Species" = "Vulture").
#'
#' @inheritParams mm_remove_hs
#' @export
#'
#' @examples
#'
#' # Image path
#' image_path <- file.path(system.file("img", package = "maimer"), "large.jpeg")
#'
#' # Get Hierarchical Subject from the image - Before use mm_create_hs()
#' mm_get_hs(path = image_path) #==> NULL
#'
#' mm_create_hs(path = image_path, value = c("Species" = "Vulture"))
#'
#' # Get Hierarchical Subject from the image - Before use mm_create_hs()
#' mm_get_hs(path = image_path) #==> "Species|Vulture"
#'
mm_create_hs <- function(path, value = c()) {

  #exiftoll_path <- paste0(base::system.file(package = "maimer"), "/app/exiftool.exe ")
  if (is.null(value)) {
    stop("Value must be provided, e.g c('Species' = 'Vulture'")
  }

  havenot_name <- is.null(names(value)) | any(names(value) == "")
  if (length(value) > 1) {
    empty_name <- names(value) == ""
  }else{
    empty_name <- is.null(names(value))
  }

  if (havenot_name) {
    stop(sprintf("Hierarchy must have a parent. Give a name to %s. For example, c(\"Parent\" = \"%s\") for %s",
                 paste(value[empty_name], collapse = ", "), value[empty_name][1L], value[empty_name][1L][1L]))
  }

  if (!is.null(mm_get_hs(path = path))) {
    existing <- noquote(paste0(" -HierarchicalSubject=", mm_get_hs(path = path)))
  }else{existing <- NULL}

  if (length(value)> 1) {
    if (!is.null(existing)) {
      parse_value <- paste0(
          unique(c(
            paste0(sprintf(" -HierarchicalSubject='%s|%s' ", names(value), value), collapse = " "),
            existing
          )), collapse = " ")

      parse_value <- noquote(parse_value)
    }else{
      parse_value <- unique(noquote(paste0(sprintf(" -HierarchicalSubject='%s|%s'", names(value), value), collapse = "")))
    }
  }else{

    if (!is.null(existing)) {
      parse_value <- unique(c(noquote(sprintf(" -HierarchicalSubject='%s|%s'", names(value), value)), existing))
    }else{
      parse_value <- unique(noquote(sprintf(" -HierarchicalSubject='%s|%s'", names(value), value)))
    }
  }

  response <- suppressMessages(
    exifr::exiftool_call(args = parse_value, fnames = path, intern = TRUE, quiet = TRUE)
  )

  return(trimws(response))

}
