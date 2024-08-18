## Get metadata from images

get_metadata <- function(path){

  if (!file.exists(path)) {
    stop("Path must be a directory path or file path")
  }else{
    metadata_df <- exiftool_call(args = NULL, fnames = path, stdout = TRUE)
  }

  out_df <- list(); idx <- 0
  for (tgs in metadata_df) {
    idx <- idx + 1
    tgs <- strsplit(tgs, split = ":")
    colname_ <- tgs[[1]][1]; val <- tgs[[1]][2]
    df_ <- data.frame(val); colnames(df_) <- trimws(colname_)
    out_df[[idx]] <- df_
  }
  rm(idx, tgs, colname_, df_, val)

  tags_df <- do.call(cbind, out_df)

  return(tags_df)

}


#' Extract image metadata
#'
#' The function extracts metadata from image files located at a
#' specified path. The function can handle both individual image files and
#' directories containing multiple images. It uses the exiftool utility to read
#' the metadata and can optionally save the extracted metadata to a CSV file.
#'
#' @param path a character vector of full path names. Either a directory path or file path. If the `path` specified is a
#' directory, the function looks for all image (.jpeg/JPEG, jpg/JPG) inside and extract
#' the tags and bind in a single data.frame.
#' @param recursive logical. Should the listing recurse into directories? It is applied if
#' `path` is directory.
#' @param save_file logical. Extracted metadata should be write on disk?
#' @param file_name a character specifying the name of file to save in csv format.
#' If left empty and `save_file` is TRUE, the default name is metadata.csv. Note that
#' the file is not saved if `save_file` is FALSE, even if the file name is provided.
#'
#' @return data.frame
#'
#' @examples
#'
#' # Image path
#' image_path <- file.path(system.file("img", package = "maimer"), "large.jpeg")
#'
#' # Extract metadata from the downloaded image
#' metadata <- mm_get_metadata(path = image_path)
#'
#' # Extract metadata from all images in a directory (non-recursive)
#' file.copy(image_path, file.path(dirname(image_path), "large2.jpeg"))
#' metadata_dir <- mm_get_metadata(path = dirname(image_path), ecursive = FALSE)
#'
#'unlink(file.path(dirname(image_path), "large2.jpeg"))
#'
#' @export
mm_get_metadata <- function(path,
                            recursive = FALSE,
                            save_file = FALSE,
                            file_name = "") {

  if (dir.exists(path)) {
    img_in_path <- list.files(path = path, pattern = "\\.(jpe?g|JPE?G)$",
                              full.names = T, recursive = recursive)

    if (length(img_in_path) == 0) {
      stop(sprintf("Any file ended by jpg|JPG|jpeg|JPEG in %s/", path))
    }
    path <- img_in_path[1]
    rbind_list <- list()
    for (img in 1:length(img_in_path)) {
      message(
        paste0("Processing image ", basename(img_in_path[[img]]),
              " (", img, " of ", length(img_in_path), ")")
      )
      rbind_list[[img]] <- exifr::read_exif(path = img_in_path[[img]])
    }

    metadata_df <- mm_stack_df(rbind_list)

  } else if (file.exists(path)) {
    metadata_df <- exifr::read_exif(path = path)
  }


  if (save_file) {
    if (file_name == "") {
      file_name <- file.path(dirname(path), "metadata.csv")
    }else if( grepl(".csv$", file_name) ){
      file_name <- file.path(dirname(path), file_name)
    }else{
      stop("Specify a correct file name (ended by .csv)")
    }

    write.csv(x = metadata_df, file = file_name)
    message(paste("File saved to ", file_name))
  }

  return(metadata_df)
}
