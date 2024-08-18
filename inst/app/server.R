
server <- function(input, output, session) {
  volumes <- getVolumes()()
  shinyDirChoose(input, "dir", roots = volumes, session = session)

  observeEvent(input$dir, {
  folderPath <- parseDirPath(volumes, input$dir)
    session$sendCustomMessage(type = "dir_selected", message = folderPath)
    if (!is.null(folderPath)) {
      if (length(folderPath) == 1) {addResourcePath("images", folderPath)}
      files <- list.files(folderPath, pattern = "\\.(jpg|jpeg|JPEG|JPG)$", full.names = TRUE)
      if (length(files) > 0) {
        output$image_list <- renderUI({
          lapply(files, function(file) {
            img_src <- file.path("images", basename(file))
            tags$div(
              tags$img(src = img_src, width = "95%", height = "95%",
                       onclick = sprintf("Shiny.setInputValue('image_clicked', '%s')", file)
                       ),
              id = basename(file), class = "imlist", onclick = 'changeBackground(this)',
              style = "margin-bottom: 10px; margin-right: 10%; cursor:pointer;" # Space between images
            )
          })
        })
      }#else{showNotification("Any image in the folder", type = "error")}
    }
  })


  observeEvent(input$image_clicked, {
    output$selectedImage <- renderImage({
      list(src = input$image_clicked, contentType = 'image/png', width = "85%", height = "85%")
    }, deleteFile = FALSE)
  })

  ## METADATA TABLE
  observeEvent(input$image_clicked, {
    metadata <- maimer::mm_get_metadata(path = input$image_clicked)
    output$metadata_table <- renderTable(metadata, spacing = "l")
  })


  ### ADD TAGS MODAL ##
  tag_name <- reactiveValues();  tag_h <- reactiveValues(l = list())
  observeEvent(input$add_tags, {
    shiny::showModal(
      modalDialog(
        tags$span(textInput("tags_parents", label = "Tags name", placeholder = "e.g species"),
                  actionButton("add_tags_parent", label = "", icon = icon("add"), style = "margin-left:10px; margin-top:15px"),
                  style = "display: flex; align-items: center;"
            ),
        tags$span(selectInput("tag_set", "", choices = c()),
                  textInput("tags_values", label = "Tags value", placeholder = "e.g Kob"),
                  actionButton("add_tags_value", label = "", icon = icon("add"), style = "margin-left:10px; margin-top:15px"),
                  style = "display: flex; align-items: center;"
        ),

        tableOutput("imported_tag"),
        tags$br(),

        title = "Customize tags",
        footer = tagList(
          shinyFilesButton("import_tag", "Import", "Choose tag file", multiple = F, icon = icon("upload")),
          modalButton("Ok")
          #actionButton("save_tag", "Save", icon = icon("save"))
        ),
        fade = FALSE,
        size = "l")
    )
  })

  ## ADD PARENT
  observeEvent(input$add_tags_parent, {
    tags_parents <- input$tags_parents
    if (as.character(tags_parents) != "") {
      tag_name[[tags_parents]] <-  tags_parents
      updateTextInput(session = session, inputId = "tags_parents", value = "")
      updateSelectInput(session = session, inputId = "tag_set", choices = sort(names(tag_name)))

    }else{
      showNotification(sprintf("You cannot add empty name"), type = "error")
    }
  })

  ## ADD VALUE TO PARENT
  observeEvent(input$add_tags_value, {
    tag_val <- reactiveValues()
    tags_value <- input$tags_values

    if (as.character(tags_value) != "") {
      tag_val[[tags_value]] <-  tags_value
      updateTextInput(session = session, inputId = "tags_values", value = "")

      ## Set list
      req(input$tag_set)
      if (length(input$tag_set != 0) > 0 && input$tag_set != 0 && length(names(tag_name) > 1)) {
        tag_set <- input$tag_set

        if (is.null(tag_h$l[[tag_set]])) {
          tag_h$l[[tag_set]] <- names(tag_val)
        }else{
          tag_h$l[[tag_set]] <- unique(c(tag_h$l[[tag_set]], names(tag_val)))
        }
      }
      for (tag in names(tag_val)) {tag_val[[tag]] <- NULL}
    }else{
      showNotification(sprintf("You cannot add empty name"), type = "error")
    }
  })

  ## IMPORT PARENT AND VALUE
  shinyFileChoose(input = input, id = "import_tag", session = session,
                  root = volumes, filetypes = c("txt", "csv"))
  tag_file_path <- reactive(parseFilePaths(roots = volumes, selection = input$import_tag))

  observeEvent(input$import_tag, {
  tag_file_path <- tag_file_path()$datapath
  req(tag_file_path)
  tryCatch({
    tag_data <- read.csv(file = tag_file_path, sep = maimer:::check_sep(tag_file_path))

    if (is.data.frame(tag_data)) {
      output$imported_tag <- renderTable(tag_data)
    }
    tag_colname <- colnames(tag_data)

    if (length(tag_h$l) >= 1 ) {
      for (tag in names(tag_h$l)) {tag_h$l[[tag]] <- NULL}
    }

    for (tcl in tag_colname) {
      tag_h$l[[tcl]] <- tag_data[[tcl]][tag_data[[tcl]] != ""]
    }

  }, error = function(e){showNotification("Cannot read file", type = "error")})

  })


  each_species_hs <- reactiveValues(hs = list());
  tojson <- reactiveValues(tojson = list())
  observeEvent(input$apply_insertion, {

    if (length(tag_h$l) > 0) {
      js_geths <- input$js_geths
      names(js_geths) <- NULL
      js_geths <- maimer:::pair_to_list(js_geths) # from JS

      if (length(each_species_hs$hs) == 0) {
        each_species_hs$hs <- js_geths
      }else{
        each_species_hs$hs <- maimer:::update_list(each_species_hs$hs, js_geths)
      }

      js_geths <- maimer:::deep_list(each_species_hs$hs)

      # Add
      if (length(input$image_clicked) == 0) {
        shiny::showNotification("No image selected", type = "message")
      }
      req(input$image_clicked)
      clicked_image_path <- input$image_clicked

      tojson$tojson[[clicked_image_path]] <- each_species_hs$hs #✅

      #print(jsonlite::toJSON(tojson$tojson))#✅

      output$tag_hierarchy <- shinyTree::renderTree({js_geths})
    }
  })

  observeEvent(input$delete_tag, {
    each_species_hs$hs <- NULL
    output$tag_hierarchy <- shinyTree::renderTree({each_species_hs$hs})
    if (length(input$image_clicked) == 0) {
      shiny::showNotification("No image selected", type = "message")
    }
    tojson$tojson[[input$image_clicked]] <- NULL
  })

  # CUSTOMIZE SUBJECT HIERARCHICAL (SH)
  all_hs <- reactiveValues(l = list());
  observeEvent(input$add_hs, {

    if (length(tag_h$l) > 0) {
      tag_len <- length(tag_h$l)
      for (tgs in 1:tag_len) {
        name <- names(tag_h$l[tgs])
        an_input <- selectInput(inputId = tolower(paste0(name, "_", tgs)),
                                label = name, choices = tag_h$l[[tgs]])
        all_hs$l[[tgs]] <- an_input

      }
      output$hs_set <- renderUI({lapply(all_hs$l, div)})

    } else {
      showNotification("Add tag to insert", type = "error")
    }
  })

  ## ADD TAG TO LIST FOR JSON
  observeEvent(input$save_json, {
    folderPath <- parseDirPath(volumes, input$dir)
    current_date <- paste0(strsplit(as.character(Sys.Date()), "\\-")[[1]], collapse = "_")
    file_name <- file.path(folderPath, paste0("maimer_", current_date, ".json"))
    jsonlite::write_json(x = tojson$tojson, path = file_name)
    each_species_hs$hs <- NULL
  })

}
