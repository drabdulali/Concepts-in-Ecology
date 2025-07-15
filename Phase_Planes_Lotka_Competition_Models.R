# Phase Plane Analysis in Ecology - Shiny App
# This app demonstrates eigenvalues, eigenvectors, nullclines, and trajectories

#We will need these libraries

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Phase Plane Analysis in Ecology"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Phase Plane", tabName = "phase_plane", icon = icon("chart-line")),
      menuItem("Theory", tabName = "theory", icon = icon("book")),
      menuItem("Parameters", tabName = "parameters", icon = icon("sliders-h"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f4f4f4; }
        .box { border-radius: 10px; }
        .info-box { border-radius: 10px; margin-bottom: 15px; }
      "))
    ),
    
    tabItems(
      # Phase Plane Tab
      tabItem(tabName = "phase_plane",
              fluidRow(
                box(width = 8, status = "primary", solidHeader = TRUE,
                    title = "Phase Plane Visualization",
                    plotlyOutput("phase_plot", height = "500px"),
                    br(),
                    fluidRow(
                      column(4, 
                             actionButton("add_trajectory", "Add Random Trajectory", 
                                          class = "btn-primary btn-block")
                      ),
                      column(4,
                             actionButton("clear_trajectories", "Clear Trajectories", 
                                          class = "btn-warning btn-block")
                      ),
                      column(4,
                             actionButton("animate", "Animate", 
                                          class = "btn-success btn-block",
                                          icon = icon("play"))
                      )
                    )
                ),
                
                box(width = 4, status = "info", solidHeader = TRUE,
                    title = "System Controls",
                    selectInput("system_type", "Ecological System:",
                                choices = list(
                                  "Predator-Prey (Lotka-Volterra)" = "predator_prey",
                                  "Competition Model" = "competition",
                                  "Mutualism Model" = "mutualism"
                                ),
                                selected = "predator_prey"),
                    
                    h4("Display Options"),
                    checkboxInput("show_nullclines", "Show Nullclines", value = TRUE),
                    checkboxInput("show_eigenvectors", "Show Eigenvectors", value = TRUE),
                    checkboxInput("show_vector_field", "Show Vector Field", value = TRUE),
                    checkboxInput("show_equilibrium", "Show Equilibrium", value = TRUE),
                    
                    h4("Current System Info"),
                    verbatimTextOutput("system_info")
                )
              ),
              
              fluidRow(
                box(width = 12, status = "success", solidHeader = TRUE,
                    title = "System Analysis",
                    dataTableOutput("analysis_table")
                )
              )
      ),
      
      # Theory Tab
      tabItem(tabName = "theory",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Phase Plane Analysis Theory",
                    
                    h3("Key Concepts"),
                    
                    h4("1. Nullclines"),
                    p("Nullclines are curves where the rate of change of one variable equals zero:"),
                    tags$ul(
                      tags$li("X-nullcline: dx/dt = 0 (red lines)"),
                      tags$li("Y-nullcline: dy/dt = 0 (blue lines)"),
                      tags$li("Equilibrium points occur at intersections of nullclines")
                    ),
                    
                    h4("2. Eigenvalues and Eigenvectors"),
                    p("Near equilibrium, the system can be linearized. The Jacobian matrix J has:"),
                    tags$ul(
                      tags$li("Eigenvalues (λ): Determine stability and behavior type"),
                      tags$li("Eigenvectors: Principal directions of system evolution")
                    ),
                    
                    h4("3. Stability Classification"),
                    tags$table(class = "table table-striped",
                               tags$tr(tags$th("Eigenvalues"), tags$th("Type"), tags$th("Stability")),
                               tags$tr(tags$td("Both λ < 0"), tags$td("Stable Node"), tags$td("Stable")),
                               tags$tr(tags$td("Both λ > 0"), tags$td("Unstable Node"), tags$td("Unstable")),
                               tags$tr(tags$td("λ₁ < 0, λ₂ > 0"), tags$td("Saddle Point"), tags$td("Unstable")),
                               tags$tr(tags$td("Complex, Re(λ) < 0"), tags$td("Stable Spiral"), tags$td("Stable")),
                               tags$tr(tags$td("Complex, Re(λ) > 0"), tags$td("Unstable Spiral"), tags$td("Unstable")),
                               tags$tr(tags$td("Pure Imaginary"), tags$td("Center"), tags$td("Neutral"))
                    ),
                    
                    h4("4. Ecological Applications"),
                    
                    h5("Predator-Prey Systems"),
                    p("Classical Lotka-Volterra equations show neutral cycles:"),
                    tags$div(style = "font-family: monospace; background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
                             "dx/dt = ax - bxy", tags$br(),
                             "dy/dt = -cy + dxy"
                    ),
                    
                    h5("Competition Models"),
                    p("Two species competing for resources:"),
                    tags$div(style = "font-family: monospace; background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
                             "dx/dt = r₁x(1 - x/K₁ - α₁₂y/K₁)", tags$br(),
                             "dy/dt = r₂y(1 - y/K₂ - α₂₁x/K₂)"
                    ),
                    
                    h5("Mutualism Models"),
                    p("Species that benefit each other:"),
                    tags$div(style = "font-family: monospace; background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
                             "dx/dt = r₁x(1 - x/K₁ + β₁₂y/K₁)", tags$br(),
                             "dy/dt = r₂y(1 - y/K₂ + β₂₁x/K₂)"
                    )
                )
              )
      ),
      
      # Parameters Tab
      tabItem(tabName = "parameters",
              fluidRow(
                box(width = 4, status = "primary", solidHeader = TRUE,
                    title = "Predator-Prey Parameters",
                    numericInput("pp_a", "Prey growth rate (a):", value = 0.8, min = 0.1, max = 2, step = 0.1),
                    numericInput("pp_b", "Predation rate (b):", value = 0.4, min = 0.1, max = 1, step = 0.1),
                    numericInput("pp_c", "Predator death rate (c):", value = 0.3, min = 0.1, max = 1, step = 0.1),
                    numericInput("pp_d", "Conversion efficiency (d):", value = 0.6, min = 0.1, max = 1, step = 0.1)
                ),
                
                box(width = 4, status = "success", solidHeader = TRUE,
                    title = "Competition Parameters",
                    numericInput("comp_r1", "Species 1 growth rate (r₁):", value = 1.0, min = 0.1, max = 2, step = 0.1),
                    numericInput("comp_r2", "Species 2 growth rate (r₂):", value = 0.8, min = 0.1, max = 2, step = 0.1),
                    numericInput("comp_K1", "Species 1 carrying capacity (K₁):", value = 2.0, min = 0.5, max = 5, step = 0.1),
                    numericInput("comp_K2", "Species 2 carrying capacity (K₂):", value = 1.5, min = 0.5, max = 5, step = 0.1),
                    numericInput("comp_a12", "Competition coefficient (α₁₂):", value = 0.8, min = 0.1, max = 2, step = 0.1),
                    numericInput("comp_a21", "Competition coefficient (α₂₁):", value = 0.6, min = 0.1, max = 2, step = 0.1)
                ),
                
                box(width = 4, status = "warning", solidHeader = TRUE,
                    title = "Mutualism Parameters",
                    numericInput("mut_r1", "Species 1 growth rate (r₁):", value = 0.5, min = 0.1, max = 2, step = 0.1),
                    numericInput("mut_r2", "Species 2 growth rate (r₂):", value = 0.4, min = 0.1, max = 2, step = 0.1),
                    numericInput("mut_K1", "Species 1 carrying capacity (K₁):", value = 2.0, min = 0.5, max = 5, step = 0.1),
                    numericInput("mut_K2", "Species 2 carrying capacity (K₂):", value = 1.5, min = 0.5, max = 5, step = 0.1),
                    numericInput("mut_b12", "Mutualism coefficient (β₁₂):", value = 0.3, min = 0.1, max = 1, step = 0.1),
                    numericInput("mut_b21", "Mutualism coefficient (β₂₁):", value = 0.4, min = 0.1, max = 1, step = 0.1)
                )
              ),
              
              fluidRow(
                box(width = 12, status = "info", solidHeader = TRUE,
                    title = "Parameter Effects",
                    h4("Understanding Parameters:"),
                    tags$ul(
                      tags$li("Growth rates (r): How fast populations grow in absence of interactions"),
                      tags$li("Carrying capacities (K): Maximum sustainable population sizes"),
                      tags$li("Competition coefficients (α): How much one species affects another negatively"),
                      tags$li("Mutualism coefficients (β): How much one species benefits another"),
                      tags$li("Predation parameters: Control predator-prey interaction strength")
                    )
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values for trajectories and animation
  values <- reactiveValues(
    trajectories = data.frame(),
    trajectory_counter = 0,
    animating = FALSE,
    animation_step = 0,
    animation_trajectories = list(),
    animation_current_step = 1,
    animation_max_steps = 100  # Much shorter for super fast animation
  )
  
  # System parameters
  get_params <- reactive({
    switch(input$system_type,
           "predator_prey" = list(
             a = input$pp_a, b = input$pp_b, c = input$pp_c, d = input$pp_d
           ),
           "competition" = list(
             r1 = input$comp_r1, r2 = input$comp_r2, K1 = input$comp_K1, K2 = input$comp_K2,
             a12 = input$comp_a12, a21 = input$comp_a21
           ),
           "mutualism" = list(
             r1 = input$mut_r1, r2 = input$mut_r2, K1 = input$mut_K1, K2 = input$mut_K2,
             b12 = input$mut_b12, b21 = input$mut_b21
           )
    )
  })
  
  # Differential equations
  derivatives <- function(x, y, system_type, params) {
    switch(system_type,
           "predator_prey" = {
             dx <- params$a * x - params$b * x * y
             dy <- -params$c * y + params$d * x * y
             list(dx = dx, dy = dy)
           },
           "competition" = {
             dx <- params$r1 * x * (1 - x/params$K1 - params$a12 * y/params$K1)
             dy <- params$r2 * y * (1 - y/params$K2 - params$a21 * x/params$K2)
             list(dx = dx, dy = dy)
           },
           "mutualism" = {
             dx <- params$r1 * x * (1 - x/params$K1 + params$b12 * y/params$K1)
             dy <- params$r2 * y * (1 - y/params$K2 + params$b21 * x/params$K2)
             list(dx = dx, dy = dy)
           }
    )
  }
  
  # Calculate equilibrium points
  get_equilibrium <- reactive({
    params <- get_params()
    switch(input$system_type,
           "predator_prey" = {
             x_eq <- params$c / params$d
             y_eq <- params$a / params$b
             list(x = x_eq, y = y_eq)
           },
           "competition" = {
             # Solve system of linear equations for interior equilibrium
             denom <- 1 - params$a12 * params$a21 / (params$K1 * params$K2)
             if (abs(denom) > 1e-10) {
               x_eq <- (params$K1 - params$a12 * params$K2) / (params$K1 * denom)
               y_eq <- (params$K2 - params$a21 * params$K1) / (params$K2 * denom)
               list(x = max(0, x_eq), y = max(0, y_eq))
             } else {
               list(x = params$K1/2, y = params$K2/2)
             }
           },
           "mutualism" = {
             # Approximate equilibrium for mutualism
             x_eq <- params$K1 * 1.2
             y_eq <- params$K2 * 1.2
             list(x = x_eq, y = y_eq)
           }
    )
  })
  
  # Calculate Jacobian matrix and eigenvalues
  get_eigenanalysis <- reactive({
    params <- get_params()
    eq <- get_equilibrium()
    
    # Calculate Jacobian matrix at equilibrium
    J <- switch(input$system_type,
                "predator_prey" = {
                  matrix(c(
                    params$a - params$b * eq$y, -params$b * eq$x,
                    params$d * eq$y, -params$c + params$d * eq$x
                  ), nrow = 2, byrow = TRUE)
                },
                "competition" = {
                  matrix(c(
                    params$r1 * (1 - 2*eq$x/params$K1 - params$a12*eq$y/params$K1),
                    -params$r1 * params$a12 * eq$x / params$K1,
                    -params$r2 * params$a21 * eq$y / params$K2,
                    params$r2 * (1 - 2*eq$y/params$K2 - params$a21*eq$x/params$K2)
                  ), nrow = 2, byrow = TRUE)
                },
                "mutualism" = {
                  matrix(c(
                    params$r1 * (1 - 2*eq$x/params$K1 + params$b12*eq$y/params$K1),
                    params$r1 * params$b12 * eq$x / params$K1,
                    params$r2 * params$b21 * eq$y / params$K2,
                    params$r2 * (1 - 2*eq$y/params$K2 + params$b21*eq$x/params$K2)
                  ), nrow = 2, byrow = TRUE)
                }
    )
    
    # Calculate eigenvalues and eigenvectors
    eigen_result <- eigen(J)
    eigenvalues <- eigen_result$values
    eigenvectors <- eigen_result$vectors
    
    # Determine stability
    stability <- if (all(Re(eigenvalues) < 0)) {
      "Stable"
    } else if (all(Re(eigenvalues) > 0)) {
      "Unstable"
    } else if (any(Re(eigenvalues) > 0) && any(Re(eigenvalues) < 0)) {
      "Saddle Point"
    } else {
      "Neutral"
    }
    
    list(
      jacobian = J,
      eigenvalues = eigenvalues,
      eigenvectors = eigenvectors,
      stability = stability
    )
  })
  
  # Generate nullclines
  get_nullclines <- reactive({
    params <- get_params()
    x_vals <- seq(0, 5, length.out = 100)
    y_vals <- seq(0, 5, length.out = 100)
    
    switch(input$system_type,
           "predator_prey" = {
             list(
               x_nullcline = list(
                 x = c(rep(0, 100), x_vals),
                 y = c(y_vals, rep(params$a / params$b, 100))
               ),
               y_nullcline = list(
                 x = c(x_vals, rep(params$c / params$d, 100)),
                 y = c(rep(0, 100), y_vals)
               )
             )
           },
           "competition" = {
             list(
               x_nullcline = list(
                 x = c(rep(0, 100), x_vals),
                 y = c(y_vals, pmax(0, params$K1/params$a12 - x_vals/params$a12))
               ),
               y_nullcline = list(
                 x = c(x_vals, pmax(0, params$K2/params$a21 - y_vals/params$a21)),
                 y = c(rep(0, 100), y_vals)
               )
             )
           },
           "mutualism" = {
             list(
               x_nullcline = list(x = c(rep(0, 100)), y = c(y_vals)),
               y_nullcline = list(x = c(x_vals), y = c(rep(0, 100)))
             )
           }
    )
  })
  
  # Main phase plane plot
  output$phase_plot <- renderPlotly({
    params <- get_params()
    eq <- get_equilibrium()
    eigen_analysis <- get_eigenanalysis()
    nullclines <- get_nullclines()
    
    # Create base plot
    p <- plot_ly(type = "scatter", mode = "markers") %>%
      layout(
        title = paste("Phase Plane -", 
                      switch(input$system_type,
                             "predator_prey" = "Predator-Prey System",
                             "competition" = "Competition Model",
                             "mutualism" = "Mutualism Model")),
        xaxis = list(title = "Population X", range = c(0, 5)),
        yaxis = list(title = "Population Y", range = c(0, 5)),
        showlegend = TRUE
      )
    
    # Add vector field
    if (input$show_vector_field) {
      x_grid <- seq(0.2, 4.8, by = 0.4)
      y_grid <- seq(0.2, 4.8, by = 0.4)
      
      for (i in x_grid) {
        for (j in y_grid) {
          derivs <- derivatives(i, j, input$system_type, params)
          if (abs(derivs$dx) > 1e-10 || abs(derivs$dy) > 1e-10) {
            magnitude <- sqrt(derivs$dx^2 + derivs$dy^2)
            scale <- 0.15 / max(magnitude, 0.1)
            
            p <- p %>% add_trace(
              x = c(i, i + derivs$dx * scale),
              y = c(j, j + derivs$dy * scale),
              type = "scatter", mode = "lines",
              line = list(color = "lightgray", width = 1),
              showlegend = FALSE, hoverinfo = "skip"
            )
          }
        }
      }
    }
    
    # Add nullclines
    if (input$show_nullclines) {
      p <- p %>% 
        add_trace(
          x = nullclines$x_nullcline$x,
          y = nullclines$x_nullcline$y,
          type = "scatter", mode = "lines",
          line = list(color = "red", width = 3),
          name = "X-nullcline (dx/dt = 0)"
        ) %>%
        add_trace(
          x = nullclines$y_nullcline$x,
          y = nullclines$y_nullcline$y,
          type = "scatter", mode = "lines",
          line = list(color = "blue", width = 3),
          name = "Y-nullcline (dy/dt = 0)"
        )
    }
    
    # Add equilibrium point
    if (input$show_equilibrium) {
      p <- p %>% add_trace(
        x = eq$x, y = eq$y,
        type = "scatter", mode = "markers",
        marker = list(color = "black", size = 10, symbol = "circle"),
        name = paste("Equilibrium (", round(eq$x, 2), ",", round(eq$y, 2), ")")
      )
    }
    
    # Add eigenvectors
    if (input$show_eigenvectors && eq$x > 0 && eq$y > 0) {
      scale <- 0.5
      for (i in 1:2) {
        eigenvec <- Re(eigen_analysis$eigenvectors[, i])
        if (abs(eigenvec[1]) > 1e-10 || abs(eigenvec[2]) > 1e-10) {
          p <- p %>% add_trace(
            x = c(eq$x - scale * eigenvec[1], eq$x + scale * eigenvec[1]),
            y = c(eq$y - scale * eigenvec[2], eq$y + scale * eigenvec[2]),
            type = "scatter", mode = "lines",
            line = list(color = "purple", width = 3),
            name = paste("Eigenvector", i),
            showlegend = (i == 1)
          )
        }
      }
    }
    
    # Add trajectories (static ones)
    if (nrow(values$trajectories) > 0) {
      for (traj_id in unique(values$trajectories$trajectory_id)) {
        traj_data <- values$trajectories[values$trajectories$trajectory_id == traj_id, ]
        p <- p %>% add_trace(
          x = traj_data$x,
          y = traj_data$y,
          type = "scatter", mode = "lines",
          line = list(color = "darkgreen", width = 2),
          name = paste("Trajectory", traj_id),
          showlegend = FALSE
        )
      }
    }
    
    # Add animated trajectories (progressive)
    if (values$animating && length(values$animation_trajectories) > 0) {
      for (i in 1:length(values$animation_trajectories)) {
        traj <- values$animation_trajectories[[i]]
        
        # Show only up to current animation step
        current_length <- min(values$animation_current_step, nrow(traj))
        
        if (current_length > 1) {
          # Draw the trajectory path up to current step
          p <- p %>% add_trace(
            x = traj$x[1:current_length],
            y = traj$y[1:current_length],
            type = "scatter", mode = "lines",
            line = list(color = "red", width = 3),
            name = paste("Animated Trajectory", i),
            showlegend = FALSE
          )
          
          # Add a moving point at the current position
          p <- p %>% add_trace(
            x = traj$x[current_length],
            y = traj$y[current_length],
            type = "scatter", mode = "markers",
            marker = list(color = "red", size = 8, symbol = "circle"),
            name = paste("Current Position", i),
            showlegend = FALSE
          )
        }
      }
    }
    
    p
  })
  
  # System information output
  output$system_info <- renderText({
    params <- get_params()
    eq <- get_equilibrium()
    eigen_analysis <- get_eigenanalysis()
    
    paste(
      "Equilibrium: (", round(eq$x, 3), ",", round(eq$y, 3), ")\n",
      "Eigenvalues: ", paste(round(eigen_analysis$eigenvalues, 3), collapse = ", "), "\n",
      "Stability: ", eigen_analysis$stability, "\n",
      "System Type: ", switch(input$system_type,
                              "predator_prey" = "Predator-Prey",
                              "competition" = "Competition",
                              "mutualism" = "Mutualism")
    )
  })
  
  # Analysis table
  output$analysis_table <- renderDataTable({
    eigen_analysis <- get_eigenanalysis()
    eq <- get_equilibrium()
    
    analysis_data <- data.frame(
      Property = c("Equilibrium X", "Equilibrium Y", "Eigenvalue 1", "Eigenvalue 2", 
                   "Stability", "System Type"),
      Value = c(
        round(eq$x, 4),
        round(eq$y, 4),
        paste(round(Re(eigen_analysis$eigenvalues[1]), 4), 
              ifelse(Im(eigen_analysis$eigenvalues[1]) != 0, 
                     paste("+", round(Im(eigen_analysis$eigenvalues[1]), 4), "i"), "")),
        paste(round(Re(eigen_analysis$eigenvalues[2]), 4), 
              ifelse(Im(eigen_analysis$eigenvalues[2]) != 0, 
                     paste("+", round(Im(eigen_analysis$eigenvalues[2]), 4), "i"), "")),
        eigen_analysis$stability,
        switch(input$system_type,
               "predator_prey" = "Predator-Prey (Lotka-Volterra)",
               "competition" = "Competition Model",
               "mutualism" = "Mutualism Model")
      )
    )
    
    datatable(analysis_data, options = list(pageLength = 10, dom = 't'))
  })
  
  # Add random trajectory
  observeEvent(input$add_trajectory, {
    params <- get_params()
    
    # Generate random starting point
    x0 <- runif(1, 0.1, 3)
    y0 <- runif(1, 0.1, 3)
    
    # Integrate trajectory using simple Euler method
    dt <- 0.02
    time_steps <- 500
    
    trajectory <- data.frame(
      x = numeric(time_steps),
      y = numeric(time_steps),
      trajectory_id = values$trajectory_counter + 1
    )
    
    trajectory$x[1] <- x0
    trajectory$y[1] <- y0
    
    for (i in 2:time_steps) {
      derivs <- derivatives(trajectory$x[i-1], trajectory$y[i-1], input$system_type, params)
      trajectory$x[i] <- trajectory$x[i-1] + derivs$dx * dt
      trajectory$y[i] <- trajectory$y[i-1] + derivs$dy * dt
      
      # Stop if trajectory goes out of bounds
      if (trajectory$x[i] < 0 || trajectory$y[i] < 0 || 
          trajectory$x[i] > 5 || trajectory$y[i] > 5) {
        trajectory <- trajectory[1:(i-1), ]
        break
      }
    }
    
    values$trajectories <- rbind(values$trajectories, trajectory)
    values$trajectory_counter <- values$trajectory_counter + 1
  })
  
  # Clear trajectories (including animated ones)
  observeEvent(input$clear_trajectories, {
    values$trajectories <- data.frame()
    values$trajectory_counter <- 0
    values$animating <- FALSE
    values$animation_trajectories <- list()
    values$animation_current_step <- 1
    updateActionButton(session, "animate", label = "Animate", 
                       icon = icon("play"))
  })
  
  # Animation with proper step-by-step trajectory evolution
  animation_timer <- reactiveTimer(intervalMs = 10, session = session)  # Super fast: 10ms
  
  # Animation control with proper stop functionality
  observeEvent(input$animate, {
    if (!values$animating) {
      # Start animation
      values$animating <- TRUE
      values$animation_current_step <- 1
      
      # Generate 3 random starting points for animation
      params <- get_params()
      animation_starts <- data.frame(
        x0 = runif(3, 0.1, 3),
        y0 = runif(3, 0.1, 3)
      )
      
      # Pre-calculate full trajectories for animation (much shorter)
      values$animation_trajectories <- list()
      
      for (i in 1:3) {
        x0 <- animation_starts$x0[i]
        y0 <- animation_starts$y0[i]
        
        dt <- 0.05  # Much larger time step
        time_steps <- 100  # Much fewer steps
        
        trajectory <- data.frame(
          x = numeric(time_steps),
          y = numeric(time_steps),
          trajectory_id = paste0("anim_", i)
        )
        
        trajectory$x[1] <- x0
        trajectory$y[1] <- y0
        
        for (j in 2:time_steps) {
          derivs <- derivatives(trajectory$x[j-1], trajectory$y[j-1], input$system_type, params)
          trajectory$x[j] <- trajectory$x[j-1] + derivs$dx * dt
          trajectory$y[j] <- trajectory$y[j-1] + derivs$dy * dt
          
          # Stop if trajectory goes out of bounds
          if (trajectory$x[j] < 0 || trajectory$y[j] < 0 || 
              trajectory$x[j] > 5 || trajectory$y[j] > 5) {
            trajectory <- trajectory[1:(j-1), ]
            break
          }
        }
        
        values$animation_trajectories[[i]] <- trajectory
      }
      
      # Update max steps based on actual trajectory lengths
      values$animation_max_steps <- max(sapply(values$animation_trajectories, nrow))
      
    } else {
      # Stop animation - this is the key fix!
      values$animating <- FALSE
      values$animation_current_step <- 1
      values$animation_trajectories <- list()
    }
  })
  
  # Separate observer to update button label based on animation state
  observe({
    if (values$animating) {
      updateActionButton(session, "animate", label = "Stop Animation", 
                         icon = icon("stop"))
    } else {
      updateActionButton(session, "animate", label = "Animate", 
                         icon = icon("play"))
    }
  })
  
  # Animation step observer - much faster progression
  observe({
    if (exists("values") && !is.null(values$animating) && values$animating) {
      animation_timer()
      
      isolate({
        values$animation_current_step <- values$animation_current_step + 5  # Skip 5 steps for super speed
        
        # Stop animation when we reach the end
        if (values$animation_current_step > values$animation_max_steps) {
          values$animating <- FALSE
          values$animation_current_step <- 1
          values$animation_trajectories <- list()
        }
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
