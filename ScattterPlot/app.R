library(shiny)
library(ggplot2)
library(readr)
library(dplyr)

ui <- fluidPage(
  titlePanel("CSV → Scatter Plot (with optional inverted Y)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV", accept = c(".csv")),
      uiOutput("var_ui"),
      checkboxInput("invert_y", "Invert Y axis", value = TRUE),
      radioButtons(
        "fit_type", 
        "Add fit line",
        choices = c(
          "None" = "none",
          "Linear (lm)" = "lm",
          "Smooth (loess)" = "loess"
        ),
        inline = FALSE
      ),
      sliderInput("alpha", "Point alpha", min = 0.1, max = 1, value = 0.8),
      sliderInput("size", "Point size", min = 0.5, max = 5, value = 2),
      downloadButton("download_plot", "Download PNG")
    ),
    mainPanel(
      tags$p("Tip: Make sure your CSV has a header row. Numeric columns will appear in the X/Y selectors."),
      plotOutput("scatter", height = "520px")
    )
  )
)

server <- function(input, output, session) {

  dat <- reactive({
    req(input$file)
    df <- read_csv(input$file$datapath, show_col_types = FALSE)
    validate(need(ncol(df) >= 2, "CSV needs at least two columns."))
    df
  })

  output$var_ui <- renderUI({
    req(dat())
    df <- dat()
    nums <- names(df)[sapply(df, is.numeric)]
    facs <- names(df)[!sapply(df, is.numeric)]

    tagList(
      selectInput("x", "X variable", choices = nums, selected = nums[1]),
      selectInput("y", "Y variable", choices = nums, selected = nums[min(2, length(nums))]),
      selectInput("color", "Color (optional)", choices = c("None", names(df)), selected = "None")
    )
  })

  plot_obj <- reactive({
    req(dat(), input$x, input$y)
    df <- dat()

    aes_color <- if (!is.null(input$color) && input$color != "None") {
      aes(color = .data[[input$color]])
    } else {
      NULL
    }

    p <- ggplot(df, aes(x = .data[[input$x]], y = .data[[input$y]])) +
      aes_color +
      geom_point(alpha = input$alpha, size = input$size) +
      theme_bw(base_size = 13) +
      labs(x = input$x, y = input$y)

    # Add fit if requested
    if (input$fit_type == "lm") {
      p <- p + geom_smooth(method = "lm", se = TRUE, color = "steelblue", linewidth = 1)
    } else if (input$fit_type == "loess") {
      p <- p + geom_smooth(method = "loess", se = TRUE, color = "firebrick", linewidth = 1)
    }

    # Invert Y axis if requested
    if (isTRUE(input$invert_y)) {
      p <- p + scale_y_reverse()
    }

    p
  })

  output$scatter <- renderPlot({
    plot_obj()
  })

  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("scatter_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = plot_obj(), width = 7, height = 5, dpi = 300)
    }
  )
}

shinyApp(ui, server)
