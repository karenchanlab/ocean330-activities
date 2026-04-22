# app.R
# Tracers vs Depth Shiny app for marine biogeochemistry
# Accepts CSV, XLSX, XLS, XLSM (macros ignored)
# Robust to messy column names
# Editable axis labels and profile lines

# install.packages(c("shiny","ggplot2","readr","readxl","dplyr","tidyr"))

library(shiny)
library(ggplot2)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(tools)

ui <- fluidPage(

  titlePanel("CSV or Excel → Tracers vs Depth"),

  sidebarLayout(
    sidebarPanel(

      fileInput(
        "file",
        "Upload data file (CSV or Excel)",
        accept = c(".csv", ".xlsx", ".xls", ".xlsm")
      ),

      uiOutput("var_ui"),

      hr(),

      textInput(
        "x_label",
        "X-axis label",
        value = "Tracer value"
      ),

      textInput(
        "y_label",
        "Y-axis label",
        value = "Depth"
      ),

      hr(),

      checkboxInput(
        "invert_y",
        "Invert Y axis (depth increases downward)",
        value = TRUE
      ),

      radioButtons(
        "fit_type",
        "Line option",
        choices = c(
          "None" = "none",
          "Connect points (profile)" = "line",
          "Linear fit (lm)" = "lm",
          "Smooth (loess)" = "loess"
        )
      ),

      sliderInput("alpha", "Point alpha", 0.1, 1, 0.8),
      sliderInput("size", "Point size", 0.5, 5, 2),

      downloadButton("download_plot", "Download PNG")
    ),

    mainPanel(
      plotOutput("scatter", height = "520px")
    )
  )
)

server <- function(input, output, session) {

  # ---- Read & sanitize data ----------------------------------------------
  dat <- reactive({
    req(input$file)

    ext <- tolower(file_ext(input$file$name))

    raw <- switch(
      ext,
      csv  = read_csv(input$file$datapath, show_col_types = FALSE),
      xlsx = read_excel(input$file$datapath),
      xls  = read_excel(input$file$datapath),
      xlsm = read_excel(input$file$datapath),
      validate("Unsupported file type")
    )

    validate(
      need(ncol(raw) >= 2, "File must have at least two columns."),
      need(any(sapply(raw, is.numeric)), "File must contain numeric columns.")
    )

    # Preserve original names for labeling
    label_map <- setNames(
      names(raw),
      make.names(names(raw), unique = TRUE)
    )

    # Clean column names for safe programming
    names(raw) <- make.names(names(raw), unique = TRUE)

    list(
      data   = raw,
      labels = label_map
    )
  })

  # ---- Variable selectors ------------------------------------------------
  output$var_ui <- renderUI({
    d <- dat()
    df <- d$data
    labels <- d$labels

    nums <- names(df)[sapply(df, is.numeric)]

    tagList(
      selectInput(
        "depth",
        "Depth variable",
        choices = setNames(nums, labels[nums]),
        selected = nums[1]
      ),

      selectizeInput(
        "tracers",
        "Tracer variable(s)",
        choices = setNames(
          setdiff(nums, nums[1]),
          labels[setdiff(nums, nums[1])]
        ),
        multiple = TRUE,
        options = list(placeholder = "Select one or more tracers")
      )
    )
  })

  # ---- Update default Y label when depth changes -------------------------
  observeEvent(input$depth, {
    updateTextInput(
      session,
      "y_label",
      value = dat()$labels[input$depth]
    )
  }, ignoreInit = TRUE)

  # ---- Plot --------------------------------------------------------------
  plot_obj <- reactive({
    req(input$depth, input$tracers)

    validate(
      need(length(input$tracers) >= 1, "Select at least one tracer.")
    )

    df <- dat()$data

    df_long <- df %>%
      select(all_of(c(input$depth, input$tracers))) %>%
      pivot_longer(
        cols = all_of(input$tracers),
        names_to = "Tracer",
        values_to = "Value"
      )

    p <- ggplot(
      df_long,
      aes(
        x = Value,
        y = .data[[input$depth]],
        color = Tracer,
        group = Tracer
      )
    ) +
      geom_point(alpha = input$alpha, size = input$size) +
      theme_bw(base_size = 13) +
      labs(
        x = input$x_label,
        y = input$y_label,
        color = "Tracer"
      )

    if (input$fit_type == "line") {
      p <- p + geom_line()
    } else if (input$fit_type == "lm") {
      p <- p + geom_smooth(method = "lm", se = TRUE)
    } else if (input$fit_type == "loess") {
      p <- p + geom_smooth(method = "loess", se = TRUE)
    }

    if (isTRUE(input$invert_y)) {
      p <- p + scale_y_reverse()
    }

    p
  })

  output$scatter <- renderPlot({
    plot_obj()
  })

  # ---- Download ----------------------------------------------------------
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("profile_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(
        file,
        plot = plot_obj(),
        width = 6,
        height = 7,
        dpi = 300
      )
    }
  )
}

shinyApp(ui, server)