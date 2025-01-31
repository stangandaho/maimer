#' Deep list
#'
#' @description
#' Convert list to tree object format for jstree library
#' @keywords internal
#' @noRd
#'
deep_list <- function(list_item){
  setlist <- list()
  listname <- names(list_item)

  for (n in listname) {
    val <- list_item[[n]]
    listed <- as.list(setNames(rep("", length(val)), val))

    setlist[[n]] <- listed
  }

  return(setlist)
}

#' Pair to list
#'
#' @description
#' unction that converts a vector into a list with pairs of elements (i.e., two-by-two)
#' @keywords internal
#' @noRd
#'
pair_to_list <- function(vec) {
  if (length(vec) %% 2 != 0) {
    stop("The length of the vector must be even.")
  }

  result_list <- list()
  for (i in seq(1, length(vec), by = 2)) {
    result_list[[vec[i]]] <- vec[i + 1]
  }

  return(result_list)
}


#' Check seperator
#' @description
#' Check and return the seperator in the file (dataset to read)
#' @keywords internal
#' @noRd
#'
check_sep <- function(file_path){

  readed_line <- readLines(con = file_path, n = 1)

  seps <- c(",", ";", "\\|")
  separators_is_in <- list()
  for (sep in seps) {
    lgc <- grepl(sep, readed_line)
    separators_is_in[[sep]] <- lgc
  }
  separators_is_in <- unlist(separators_is_in)
  names(separators_is_in) <- NULL

  if (all(separators_is_in == FALSE)) {
    stop("Unknow seperator in file")
  }

  sep <- seps[separators_is_in][1L]

  if (sep == "\\|") {
    sep <- "|"
  }
  return(sep)
}

#' Update list
#' @description
#' Update an existing list basing on item in second list.
#' If the element is in the list, append the value from second list
#' and ensure the values are unique. If the element is not in the list, add it with its value
#' @keywords internal
#' @noRd
#'
update_list <- function(first_list, second_list) {
  for (name in names(second_list)) {
    if (name %in% names(first_list)) {
      #
      combined_values <- unique(c(first_list[[name]], second_list[[name]]))
      first_list[[name]] <- combined_values
    } else {
      #
      first_list[[name]] <- second_list[[name]]
    }
  }
  return(first_list)
}


#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @export
magrittr::`%>%`

#' Pipe operator
#'
#' @name %<>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %<>%
#' @usage lhs \%<>\% rhs
#' @export
magrittr::`%<>%`



################
#' Parse datetime
#'
#' @description
#' Parses an input vector into POSIXct date-time object.
#' @keywords internal
#' @noRd
#'
parse_datetime <- function (datetime,
                            format,
                            time_zone,
                            check_na = TRUE,
                            check_empty = TRUE,
                            check_na_out = TRUE,
                            allow_empty_output = FALSE,
                            quiet = FALSE) {

  if (inherits(datetime, c("POSIXct", "POSIXlt"))) {
    datetime <- format(datetime, format = "%Y-%m-%d %H:%M:%S")
  }
  else {
    if (!inherits(datetime, "character"))
      stop(paste("datetime must be a character:",
                 deparse(substitute(datetime))), call. = FALSE)
  }
  if (check_na & any(is.na(datetime)))
    stop(paste("there are NAs in", deparse(substitute(datetime))), call. = FALSE)
  if (check_empty & any(datetime == ""))
    stop(paste("there are blank values in", deparse(substitute(datetime))), call. = FALSE)
  if (all(datetime == "") & allow_empty_output)
    return(NA)
  datetime_char <- as.character(datetime)

  if (grepl(pattern = "%", x = format, fixed = TRUE)) {
    out <- as.POSIXct(datetime_char, tz = time_zone, format = format)
  }

  if (all(is.na(out)))
    stop(paste0("Cannot read datetime format in ", deparse(substitute(datetime)),
                ". Output is all NA.\n", "expected:  ", format,
                "\nactual:    ", datetime[1]), call. = FALSE)
  if (check_na_out & any(is.na(out)))
    stop(paste(sum(is.na(out)), "out of", length(out), "records in",
               deparse(substitute(datetime)), "cannot be interpreted using format:",
               format, "\n", "rows", paste(which(is.na(out)),
                                           collapse = ", ")), call. = FALSE)
  if (inherits(datetime, c("POSIXct", "POSIXlt")))
    stop("couldn't interpret datetime using specified format. Output is not POSIX object")
  return(out)
}



########################
#' Select column
#'
#' @description
#' Select column from data
#' @keywords internal
#' @noRd
#'
select_col <- function(data, cols){
  #select_type <- class(cols)

  is_num <- tryCatch({
    suppressWarnings(ifelse(class(cols) %in% c("numeric", "integer"), TRUE, FALSE))
  }, error = function(e)FALSE)


  if (any(is_num)) {

    selected_col <- colnames(data)[cols]

  }else{

    qu <- paste0(rlang::quo_get_expr(dplyr::enquo(cols)))
    qu <- qu[qu != ":"]

    if(length(qu) == 2){
      selected_col <- suppressWarnings(colnames(data %>% dplyr::select(qu[1]:qu[2])))
    }else{
      selected_col <- colnames(data %>% dplyr::select(all_of(qu)))
    }
  }

  return(selected_col)
}

#' Melt data
#'
#' @description
#' Convert a matrix into a molten data frame
#' @keywords internal
#' @noRd
#'

melt <- function(data){
  if (any(!class(data) %in% c("matrix", "array"))) {
    stop("Data must be a matrix", call. = FALSE)
  }

  if (is.null(colnames(data))) {
    stop("The data must have column names", call. = FALSE)
  }

  if (is.null(rownames(data))) {
    stop("The data must have row names",  call. = FALSE)
  }

  tbl_list <- list()

  for (r in rownames(data)) {
    for (c in colnames(data)) {
      tbl <- tibble(Var1 = c, Var2 = r, value = data[r, c])
      tbl_list[[paste0(r,c)]] <- tbl
    }
  }
  melt_data <- dplyr::bind_rows(tbl_list)

  return(melt_data)
}
