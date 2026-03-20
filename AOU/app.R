library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

o2sat_umolkg <- function(T_degC = 4, S = 34.7) {
  TK <- T_degC + 273.15
  term <- -173.9894 +
    255.5907 * (100 / TK) +
    146.4813 * log(TK / 100) -
    22.2040 * (TK / 100) +
    S * (-0.037362 + 0.016504 * (TK / 100) - 0.0020564 * (TK / 100)^2)
  exp(term)
}

run_model <- function(circulation = 1, OUR = 0.12, T = 4, S = 34.7, PO4_0 = 0.20, n = 100) {
  location <- seq_len(n)
  age_max_target <- 1000
  age <- (location - 1) / (n - 1) * (age_max_target / circulation)

  O2_sat <- o2sat_umolkg(T, S)
  O2_raw <- O2_sat - OUR * age
  O2     <- pmax(0, O2_raw)
  AOU    <- pmax(0, O2_sat - O2)
  PO4    <- PO4_0 + (OUR * age) / 138

  tibble(location, age, O2, PO4, AOU)
}

ui <- fluidPage(
  titlePanel("AOU Model"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("circulation", "Circulation rate (higher = faster renewal)",
                  min = 0.4, max = 3, value = 1, step = 0.1),
      sliderInput("temp", "Temperature (°C) — controls O2 saturation via Henry's law",
                  min = -1.8, max = 25, value = 4, step = 0.2),
      sliderInput("OUR", "Average OUR (µmol O2 kg⁻¹ yr⁻¹)",
                  min = 0.02, max = 0.25, value = 0.12, step = 0.005),

      # NEW: Δ toggle (defaults off)
      checkboxInput("show_delta", "Show Δ from Reference (Modified − Reference)", FALSE),

      # Existing controls
      checkboxInput("show_ref", "Show Reference (gray) overlay", TRUE),
      actionButton("reset", "Reset to defaults", class = "btn btn-secondary"),
      tags$hr(),
      tags$small("Fixed parameters: Salinity = 34.7 PSU; Surface PO4 baseline = 0.20 µmol kg⁻¹")
    ),
    mainPanel(
      plotOutput("facet_plot", height = "900px")
    )
  )
)

server <- function(input, output, session) {
  defaults <- list(circulation = 1, temp = 4, OUR = 0.12, show_ref = TRUE, show_delta = FALSE)

  observeEvent(input$reset, {
    updateSliderInput(session, "circulation", value = defaults$circulation)
    updateSliderInput(session, "temp",        value = defaults$temp)
    updateSliderInput(session, "OUR",         value = defaults$OUR)
    updateCheckboxInput(session, "show_ref",  value = defaults$show_ref)
    updateCheckboxInput(session, "show_delta",value = defaults$show_delta)
  })

  # Reference at defaults
  ref <- run_model(circulation = defaults$circulation,
                   OUR = defaults$OUR,
                   T = defaults$temp,
                   S = 34.7,
                   PO4_0 = 0.20)

  # Modified
  mod <- reactive({
    run_model(circulation = input$circulation,
              OUR = input$OUR,
              T = input$temp,
              S = 34.7,
              PO4_0 = 0.20)
  })

  output$facet_plot <- renderPlot({

    # Labels for absolute mode
    var_levels_abs  <- c("age","O2","PO4","AOU")
    var_labels_abs  <- c("Age (years)",
                         "[O\u2082] (µmol kg\u207B\u00B9)",
                         "[PO\u2084] (µmol kg\u207B\u00B9)",
                         "AOU (µmol kg\u207B\u00B9)")

    # Labels for delta mode
    var_levels_del  <- var_levels_abs
    var_labels_del  <- c("\u0394Age (years)",
                         "\u0394[O\u2082] (µmol kg\u207B\u00B9)",
                         "\u0394[PO\u2084] (µmol kg\u207B\u00B9)",
                         "\u0394AOU (µmol kg\u207B\u00B9)")

    if (isTRUE(input$show_delta)) {
      # Compute deltas by matching on location
      df_delta <- mod() %>%
        rename_with(~ paste0(.x, "_mod"), -location) %>%
        inner_join(ref %>% rename_with(~ paste0(.x, "_ref"), -location), by = "location") %>%
        transmute(
          location,
          age = age_mod - age_ref,
          O2  = O2_mod  - O2_ref,
          PO4 = PO4_mod - PO4_ref,
          AOU = AOU_mod - AOU_ref,
          type = "Δ (Modified − Reference)"
        ) %>%
        pivot_longer(cols = c(age, O2, PO4, AOU),
                     names_to = "variable", values_to = "value") %>%
        mutate(variable = factor(variable, levels = var_levels_del, labels = var_labels_del))

      ggplot(df_delta, aes(x = location, y = value, color = type)) +
        geom_hline(yintercept = 0, color = "gray75") +
        geom_line(linetype = "dashed", linewidth = 1.2, lineend = "round") +
        facet_wrap(~ variable, scales = "free_y", ncol = 1) +
        scale_color_manual(values = c("Δ (Modified − Reference)" = "red3")) +
        labs(x = "Location (1 = youngest/near-surface end)", y = NULL, color = NULL) +
        theme_bw(base_size = 13) +
        theme(
          legend.position  = "top",
          strip.background = element_rect(fill = "gray95"),
          strip.text       = element_text(face = "bold", size = 15)
        )

    } else {
      # Absolute mode (as before)
      df_ref <- ref %>% mutate(type = "Reference")
      df_mod <- mod() %>% mutate(type = "Modified")

      df_all <- bind_rows(df_ref, df_mod) %>%
        pivot_longer(cols = all_of(var_levels_abs),
                     names_to = "variable", values_to = "value") %>%
        mutate(variable = factor(variable, levels = var_levels_abs, labels = var_labels_abs))

      # Optionally hide the reference line
      if (!isTRUE(input$show_ref)) df_all <- df_all %>% filter(type == "Modified")

      ggplot(df_all, aes(x = location, y = value, color = type)) +
        geom_line(linewidth = 1.1, lineend = "round") +
        facet_wrap(~ variable, scales = "free_y", ncol = 1) +
        scale_color_manual(values = c("Reference" = "gray60", "Modified" = "#1f78b4"),
                           breaks = c("Reference","Modified")) +
        labs(x = "Location (1 = youngest/near-surface end)", y = NULL, color = NULL) +
        theme_bw(base_size = 13) +
        theme(
          legend.position  = "top",
          strip.background = element_rect(fill = "gray95"),
          strip.text       = element_text(face = "bold", size = 15)
        )
    }
  })
}

shinyApp(ui, server)