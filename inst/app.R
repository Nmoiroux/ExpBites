library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ExpBites)  # Make sure your package is installed and loaded

ui <- fluidPage(
  titlePanel("Simulate and Visualize Mosquito Biting Exposure"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("n_ind", "Number of individuals:", value = 100, min = 1),
      sliderInput("bednet_use", "Proportion of bednet users:", min = 0, max = 1, value = 0.5, step = 0.05),
      sliderInput("start_hour", "Interval start hour:", min = 0, max = 23, value = 22),
      sliderInput("end_hour", "Interval end hour:", min = 0, max = 23, value = 5),
      actionButton("runSim", "Run Simulation")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Exposure Plot", plotOutput("exposurePlot")),
        tabPanel("Behavior Plot", plotOutput("behaviorPlot")),
        tabPanel("Summary Table", tableOutput("summaryTable"))
      )
    )
  )
)

server <- function(input, output) {
  sim_data <- eventReactive(input$runSim, {
    df <- gen_df_human(n_individuals = input$n_ind, prob_use = input$bednet_use)
    df_bites <- gen_df_mosquito()
    exp_result <- calculate_Exp(df, df_bites)
    list(df = df, bites = df_bites, exposure = exp_result)
  })
  
  output$exposurePlot <- renderPlot({
    req(sim_data())
    plot_exposure(sim_data()$exposure)
  })
  
  output$behaviorPlot <- renderPlot({
    req(sim_data())
    plot_behaviors(sim_data()$exposure)
  })
  
  output$summaryTable <- renderTable({
    req(sim_data())
    raw_summary <- summarise_exposure(sim_data()$exposure, interval = c(input$start_hour, input$end_hour))
    
    description_lookup <- c(
      Eui = "Non-user exposure indoors",
      Euo = "Non-user exposure outdoors",
      Eu = "Total non-user exposure",
      prop_indoor = "% exposure indoors",
      Epi = "User exposure indoors (no net)",
      Epn = "User exposure indoors (under net)",
      Epo = "User exposure outdoors",
      Epp = "Exposure prevented by net",
      Ep = "Total user exposure",
      prop_interval_non_user = "% of non-user exposure in interval",
      prop_interval_user = "% of user exposure in interval",
      prop_prevented = "% of user exposure prevented by net"
    )
    
    type_lookup <- c(
      non_user_daily = "Daily (non-users)",
      user_daily = "Daily (users)",
      non_user_interval = "Interval (non-users)",
      user_interval = "Interval (users)",
      interval_vs_daily = "Interval / Daily", 
      net_efficacy = "Net efficacy"
    )
    
    raw_summary %>%
      mutate(
        output = ifelse(output %in% names(description_lookup), description_lookup[output], output),
        type = ifelse(type %in% names(type_lookup), type_lookup[type], type),
        value = ifelse(grepl("%", output), round(value * 100, 1), round(value, 3))
      ) %>%
      rename(`Summary type` = type, `Metric` = output, `Value` = value)
  })
}

shinyApp(ui, server)
