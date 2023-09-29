#' This function launches the risk assessment Shiny app .
#'
#' @importFrom utils write.csv
#'
#' @author Finlay Campbell
#'
#' @export
#'
run_riskassess <- function() {

  ## Define the UI
  ui <- fluidPage(
    titlePanel(p(strong("Risk Assessment"))),
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          tabPanel(
            title = "Upload/Download",
            fileInput("upload_data", "Upload Risk Scores", accept = ".xlsx"),
            fileInput(
              "upload_shape",
              "Upload Joint Shape Files (.shp, .shx, .prj, .dbf)",
              accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg'),
              multiple = TRUE
            ),
            downloadButton("download_data", "Download Scores")
          ),
          tabPanel(
            title = "Select Overall Weightings",
            uiOutput("weightings")
          ),
          tabPanel(
            title = "Select Pillar Weightings",
            uiOutput("pillars")
          )
        )
      ),
      uiOutput("plots")
    )
  )

  ## Define the server
  server <- function(input, output) {

    ## upload data
    data <- reactive({
      req(input$upload_data)
      read_data(input$upload_data$datapath)
    })

    ## upload shapefile
    shape <- reactive({
      req(input$upload_shape)
      oldfiles <- list.files(dirname(input$upload_shape$datapath), full.names = TRUE)
      newfiles <- file.path(dirname(oldfiles)[1], gsub(".*\\.", "shapefile.", oldfiles))
      changed <- file.rename(oldfiles, newfiles)
      newfiles <- newfiles[changed]
      read_shape(newfiles[grepl("shp", newfiles)], scores = data()$scores)
    })

    ## define risk as a reactive value depending on sliders
    ## values <- reactiveValues(risks = get_risks(data()$groupings, data()$scores))
    values <- reactiveValues(risks = NULL)

    ## re-calculate risk when needed
    observe({
      values$risks <- get_risks(
        groupings = map(
          data()$groupings,
          ~ imap_dbl(.x, function(x, y) if(!is.null(input[[y]])) input[[y]] else(x))
        ),
        scores = data()$scores,
        weightings = map_dbl(
          names(data()$groupings),
          ~ if(!is.null(input[[.x]])) input[[.x]] else 1/length(data()$groupings)
        )
      )
    })

    ## define slider for overall weightings
    output$weightings <- renderUI({
      map(names(data()$groupings), function(i) {
        sliderInput(inputId = i, label = i, min = 0, max = 10, value = 5)
      })
    })


    observe({

      output$pillars <- renderUI(
        do.call(
          tabsetPanel,
          map(
            names(data()$groupings),
            function(group)
              tabPanel(
                title = group,
                map(names(data()$groupings[[group]]), function(j) {
                  sliderInput(inputId = j, label = j, min = 0, max = 10, value = 5)
                })
              )
          )
        )
      )

      output$plots <- renderUI(
        do.call(
          mainPanel,
          map(
            seq_len(ceiling((length(data()$groupings) + 1)/2)),
            function(rownumber) {
              n <- length(data()$groupings) + 1
              nms <- c(names(data()$groupings), "Total")
              index <- seq(1, n, by = 2)[rownumber]
              fluidRow(
                column(6, renderPlot(vis_scores(values$risks, shape(), nms[index], title = nms[index]))),
                column(6, renderPlot(vis_scores(values$risks, shape(), nms[index+1], title = nms[index+1])))
              )
            }
          )
        )
      )

    })

    output$download_data <- downloadHandler(
      filename = "risk_scores.csv",
      content = function(file) write.csv(values$risks, file, row.names = FALSE, na = "NA")
    )

  }

  ## Run the Shiny app
  shinyApp(ui = ui, server = server)

}
