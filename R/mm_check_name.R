#' Check species name and retrieve Taxonomic Serial Number (TSN) from ITIS
#'
#' This function queries the **Integrated Taxonomic Information System (ITIS)**
#' to find taxonomic details for a given species name.  It can search using
#' either a **scientific name** or a **common name** and return relevant
#' taxonomic information, including the TSN.
#'
#' @param species_name A character string specifying the species name to search
#' for. Only a **single name** is allowed.
#' @param search_type A character string specifying the type of search. Options:
#'   - `"sientific_name"`: Search by scientific name.
#'   - `"common_name"`: Search by common name.
#' @param ask A logical value (`TRUE` or `FALSE`). If `TRUE`, allows interactive selection when multiple matches are found.
#'
#' @return A tibble containing taxonomic details:
#'   - `search`: The original species name queried.
#'   - `tsn`: The **Taxonomic Serial Number** (TSN) from ITIS.
#'   - `common_name`: The common name of the species (if available).
#'   - `scientific_name`: The scientific name of the species.
#'   - `author`: The author who classified the species.
#'   - `itis_url`: A direct link to the species report on ITIS.
#'   - `taxon_status`: The taxonomic status of the species.
#'
#' @details
#' - If the necessary packages (`httr2`, `xml2`) are not installed, the function prompts the user to install them.
#' - If multiple results are found and `ask = TRUE`, the user is prompted to select the correct match.
#' - If no exact match is found, all results are displayed for manual selection.
#'
#' @examples
#' \dontrun{
#' # Search for a species by scientific name
#' mm_check_name("Panthera leo", search_type = "sientific_name")
#'
#' # Search by common name with interactive selection
#' mm_check_name("Lion", search_type = "common_name", ask = TRUE)
#' }
#'
#'
#' @seealso \url{https://www.itis.gov}
#'
#' @export
#'
mm_check_name <- function(species_name,
                          search_type,
                          ask = FALSE) {
  suggested_pkg <- c("httr2", "xml2")
  installed <- rownames(installed.packages())
  not_in <- !suggested_pkg %in% installed

  if (any(not_in)) {
    custom_cli(sprintf("You need to install: %s \n1. Install\n2. Exit\n", suggested_pkg[not_in]))
    action <- readline(prompt = "Action: ")

    if (action %in% c("1", "install", "Install", "Yes", "yes")) {
      for (pkg in suggested_pkg[not_in]) {
        install.packages(pkg)
      }
    }else{
      return(invisible(NULL))
    }

  }
  ## Start data retrieve
  if (length(species_name) >= 2) {
    rlang::abort(sprintf("No search possible for %s species", length(species_name)))
  }
  base_url_sci <- "https://www.itis.gov/ITISWebService/services/ITISService/getITISTermsFromScientificName?srchKey="
  search_url_sci <- paste0(base_url_sci, gsub("\\s+", "%20",  species_name))

  base_url_common <- "https://www.itis.gov/ITISWebService/services/ITISService/getITISTermsFromCommonName?srchKey="
  search_url_common <- paste0(base_url_common, gsub("\\s+", "%20",  species_name))

  URL <- switch (search_type,
                 "sientific_name" = search_url_sci,
                 "common_name" = search_url_common
  )

  # Fetch the XML response using httr2
  response <- request(URL) %>%
    req_perform() %>%
    resp_body_xml()

  # Extract TSN (Taxonomic Serial Number)
  tsn <- xml_text(xml_find_all(response, ".//ax21:tsn"))
  common <- xml_text(xml_find_all(response, ".//ax21:commonNames"))
  usage <- xml_text(xml_find_all(response, ".//ax21:nameUsage"))
  scientific_name <- xml_text(xml_find_all(response, ".//ax21:scientificName"))
  author <- gsub("\\(|\\)", "", xml_text(xml_find_all(response, ".//ax21:author")))

  out_tbl <- dplyr::tibble(
    search = rep(species_name, length(tsn)), tsn = tsn, common_name = common, scientific_name = scientific_name, author = author,
    itis_url = paste0("https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value", tsn),
    taxon_status = usage
  )

  ## Filter to choose correct observation
  pre_filter <- switch (search_type,
                        "sientific_name" = out_tbl %>% dplyr::filter(scientific_name == species_name),
                        "common_name" =  out_tbl %>% dplyr::filter(common_name == species_name)
  )

  if (ask & nrow(out_tbl) > 1 & nrow(pre_filter) == 0) {
    print(out_tbl %>% dplyr::select(scientific_name, common_name))
    msg <- sprintf("\n%s TSN found for taxon %s!\nEnter the row number of taxon you want:",
                   nrow(out_tbl), species_name)
    custom_cli(msg, color = "red")
    #action <- readline(prompt = "Select row: ")

    ## Check right selection
    right_sel <- TRUE
    while (any(sel)) {
      action <- readline(prompt = "Select row: ")
      right_sel <- any(c(suppressWarnings(is.na(as.numeric(action))),
                         as.numeric(action) > nrow(out_tbl),
                         as.numeric(action) <= 0))
    }

    out_tbl <- out_tbl %>% dplyr::slice(as.numeric(action))

  }else if(nrow(pre_filter) == 1){
    out_tbl <- pre_filter
  }else{

  }


  return(out_tbl)

}
