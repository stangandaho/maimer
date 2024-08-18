#' Run App
#'
#' Launch maimer GUI for image/video management
#'
#' @export
#'
#'
mm_app <- function() {

  source(paste0(system.file("app", package = "maimer"), "/packages.R"))
  app_dir <- system.file("app", package = "maimer")
  shiny::runApp(appDir = app_dir)
}




