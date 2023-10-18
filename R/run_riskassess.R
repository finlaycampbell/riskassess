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
              "Upload Shape Files (.shp, .shx, .prj, .dbf)",
              accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg'),
              multiple = TRUE
            ),
            downloadButton("download_data", "Download Scores"),
            downloadButton("download_weightings", "Download Weightings")
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
      if(all(file.exists(input$upload_shape$datapath))) {
        changed <- suppressWarnings(file.rename(oldfiles, newfiles))
        newfiles <- newfiles[changed]
      }
      read_shape(unique(newfiles[grepl("shp", newfiles)]), scores = data()$scores)
    })

    ## define risk as a reactive value depending on sliders
    ## values <- reactiveValues(risks = get_risks(data()$groupings, data()$scores))
    values <- reactiveValues(risks = NULL)

    ## re-calculate risk when needed
    observe({

      ## read groupings from sliders
      groupings <- map(
        data()$groupings,
        ~ imap_dbl(.x, function(x, y) if(!is.null(input[[y]])) input[[y]] else(x))
      ) %>%
        map(~.x/sum(.x))

      ## read pillar weightings from sliders
      weightings <- map_dbl(
        names(data()$groupings),
        ~ if(!is.null(input[[.x]])) input[[.x]] else 1/length(data()$groupings)
      ) %>%
        (\(x) x/sum(x))

      ## calculate risk scores from weightings and metrics
      values$risks <- get_risks(
        groupings = groupings,
        scores = data()$scores,
        weightings = weightings
      )

      ## generate weightings table
      values$weightings_table <- map_dfr(
        seq_along(weightings),
        \(x) tibble(
               pillar = names(groupings)[x],
               metric = names(groupings[[x]]),
               pillar_weight = weightings[x],
               metric_weight = groupings[[x]],
               total_weight = pillar_weight*metric_weight
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

      ## manual hack for generating two columns of plots
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
                column(
                  6, renderPlot(vis_scores(
                       values$risks, shape(), nms[index], title = nms[index]
                     ))
                ),
                column(
                  6, renderPlot(vis_scores(
                       values$risks, shape(), nms[index+1], title = nms[index+1]
                     ))
                )
              )
            }
          )
        )
      )

    })

    output$download_data <- downloadHandler(
      filename = "risk_scores.csv",
      content = \(file) write.csv(values$risks, file, row.names = FALSE, na = "NA")
    )

    output$download_weightings <- downloadHandler(
      filename = "weightings.csv",
      content = \(file) write.csv(values$weightings_table, file,
                                  row.names = FALSE, na = "NA")
    )

  }

  ## Run the Shiny app
  shinyApp(ui = ui, server = server, options = list(quiet = TRUE))

}
