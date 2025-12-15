library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(gganimate)
library(binom)
library(bsicons)

# Define brand theme
brand_theme <- bs_theme(
  version = 5,
  bg = "#f2f2f2",
  fg = "#000000",
  primary = "#267326",
  base_font = font_google("Source Sans Pro"),
  heading_font = font_google("Source Sans Pro", wght = 700),
  code_font = font_google("Roboto Mono")
)

ui <- page_sidebar(
  title = "Konfidenzintervall-Simulation",
  theme = brand_theme,
  
  sidebar = sidebar(
    width = 300,
    card(
      card_header("Simulationsparameter"),
      sliderInput(
        "n_samples",
        "Anzahl Stichproben:",
        value = 100,
        min = 50,
        max = 300,
        step = 10
      ),
      numericInput(
        "n_trials",
        "Anzahl Versuche pro Stichprobe:",
        value = 30,
        min = 5,
        max = 200,
        step = 5
      ),
      sliderInput(
        "true_p",
        "Wahre Erfolgswahrscheinlichkeit:",
        value = 0.5,
        min = 0.1,
        max = 0.9,
        step = 0.05
      ),
      sliderInput(
        "conf_level",
        "Konfidenzniveau (%):",
        value = 95,
        min = 80,
        max = 99,
        step = 1,
        post = "%"
      ),
      actionButton(
        "generate",
        "Animation erstellen",
        class = "btn-primary w-100 mt-3"
      )
    )
  ),
  
  layout_columns(
    col_widths = c(8, 4),
    card(
      card_header("Animierte Konfidenzintervalle"),
      card_body(
        conditionalPanel(
          condition = "!output.animation",
          div(
            class = "text-center text-muted p-5",
            style = "min-height: 400px; display: flex; align-items: center; justify-content: center;",
            h4("Klicke 'Animation erstellen' zum Starten")
          )
        ),
        uiOutput("animation_ui")
      )
    ),
    card(
      card_header("Zusammenfassung"),
      card_body(
        uiOutput("summary_stats")
      )
    )
  )
)

server <- function(input, output, session) {
  
  animation_path <- reactiveVal(NULL)
  sim_results <- reactiveVal(NULL)
  trigger <- reactiveVal(0)
  
  observeEvent(input$generate, {
    # Show progress
    withProgress(message = 'Erstelle Animation...', value = 0, {
      
      set.seed(123)
      
      n_samples <- input$n_samples
      n_trials <- input$n_trials
      true_p <- input$true_p
      conf_level <- input$conf_level / 100
      
      incProgress(0.1, detail = "Simuliere Daten...")
      
      # Simulate data
      sim_data <- lapply(1:n_samples, function(i) {
        x <- rbinom(1, n_trials, true_p)
        p_hat <- x / n_trials
        
        ci <- binom.confint(x, n_trials, methods = "wilson", conf.level = conf_level)
        
        tibble(
          sample_id = i,
          p_hat = p_hat,
          ci_low = ci$lower,
          ci_high = ci$upper,
          covers = ci_low <= true_p & ci_high >= true_p
        )
      }) %>% bind_rows()
      
      sim_results(sim_data)
      
      incProgress(0.1, detail = "Bereite Animationsdaten vor...")
      
      # Expand data so all previous CIs exist in future frames
      anim_data <- expand.grid(
        frame = 1:n_samples,
        sample_id = 1:n_samples
      ) %>%
        left_join(sim_data, by = "sample_id") %>%
        filter(sample_id <= frame) %>%
        mutate(
          y = frame - sample_id
        )
      
      incProgress(0.1, detail = "Erstelle Plot...")
      
      # Plot
      p <- ggplot(anim_data, aes(y = y)) +
        geom_segment(
          aes(x = ci_low, xend = ci_high,
              yend = y,
              colour = covers),
          linewidth = 1.2
        ) +
        geom_point(aes(x = p_hat), size = 1.5) +
        geom_vline(
          xintercept = true_p,
          linetype = "dashed",
          colour = "grey30"
        ) +
        scale_colour_manual(
          values = c("TRUE" = "#267326", "FALSE" = "#d77d00")
        ) +
        scale_x_continuous(limits = c(0, 1)) +
        coord_cartesian(ylim = c(0, 40)) +
        labs(
          title = "Konfidenzintervall-Simulation (Binomial)",
          subtitle = paste0("Gezogene Stichproben: {frame} | KI: ", input$conf_level, "%"),
          x = "Anteil ± KI",
          y = NULL,
          colour = "Enthält wahren Anteil"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title = element_text(color = "#267326", face = "bold"),
          legend.position = "bottom",
          plot.background = element_rect(fill = "#f2f2f2", color = NA),
          panel.background = element_rect(fill = "#f2f2f2", color = NA)
        )
      
      # Animate
      anim <- p + transition_manual(frame)
      
      incProgress(0.1, detail = "Initialisiere Rendering...")
      
      incProgress(0.05, detail = "Rendere Animation (kann einen Moment dauern)...")
      
      # Create temporary file
      temp_file <- tempfile(fileext = ".gif")
      
      animate(
        anim,
        nframes = n_samples * 3,
        fps = 20,
        width = 900,
        height = 600,
        renderer = gifski_renderer(temp_file)
      )
      
      incProgress(0.5, detail = "Speichere Animation...")
      
      # Copy to www directory so it can be served
      www_dir <- file.path(getwd(), "www")
      if (!dir.exists(www_dir)) dir.create(www_dir)
      
      final_path <- file.path(www_dir, "ci_animation.gif")
      file.copy(temp_file, final_path, overwrite = TRUE)
      
      incProgress(0.05, detail = "Fertig!")
      
      animation_path(final_path)
      trigger(trigger() + 1)
    })
  })
  
  output$animation_ui <- renderUI({
    req(animation_path())
    trigger()  # Depend on trigger
    
    tags$div(
      class = "text-center",
      tags$img(
        src = paste0("www/ci_animation.gif?t=", trigger()),
        style = "max-width: 100%; height: auto;"
      )
    )
  })
  
  # Add route for serving the image
  addResourcePath("www", file.path(getwd(), "www"))
  
  output$animation <- reactive({
    !is.null(animation_path())
  })
  outputOptions(output, "animation", suspendWhenHidden = FALSE)
  
  output$summary_stats <- renderUI({
    req(sim_results())
    
    data <- sim_results()
    coverage <- mean(data$covers)
    n_cover <- sum(data$covers)
    n_miss <- sum(!data$covers)
    
    tagList(
      value_box(
        title = "KIs enthalten wahres p",
        value = n_cover,
        showcase = bs_icon("check-circle"),
        theme = "success"
      ),
      value_box(
        title = "KIs verfehlen wahres p",
        value = n_miss,
        showcase = bs_icon("x-circle"),
        theme = "warning"
      ),
      tags$p(
        class = "text-muted mt-3",
        paste0(
          "Erwartete Abdeckung: ", input$conf_level, "%. ",
          "Mit ", input$n_samples, " Stichproben sollte die empirische Abdeckung nahe an diesem Wert liegen."
        )
      )
    )
  })
}

shinyApp(ui, server)
