# load packages
library(shiny)
library(shinyalert)
library(readxl)
library(tidyr)
library(ggplot2)
library(plotly)
library(dplyr)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

#welcome message
pop_up_message <- "Welcome to <b>Australia Energy Transition Trends</b>. 
                   This data visualization provides useful insights into the transition from non-renewables 
                   to renewables for energy generation across states in Australia."

# Load data
carbon_category <- read_excel("carbon_perc_aus.xlsx", sheet = 'Sheet1')
carbon_emission <- read_excel("co2_aus_2000-22.xlsx")
data_elec <- read_excel("power_gen_aus.xlsx", sheet = 'Sheet1')
data_elec_perc <- read_excel("power_gen_aus.xlsx", sheet = 'Sheet2')
data_co2_conc <- read.csv("global_co2_concentration.csv")

# Fill Null values
data_elec <- data_elec %>%
  mutate(across(everything(), ~ replace_na(., 0)))
data_elec_perc <- data_elec_perc %>% na.omit()

# Reshape electricity data
elec_data_long <- data_elec %>%
  pivot_longer(
    cols = starts_with("2008"):starts_with("2022"),
    names_to = "Year",
    values_to = "Value"
  )

# Convert Year to numeric
elec_data_long$Year <- as.numeric(elec_data_long$Year)

# Filter non-renewable energy sources
non_renewables <- elec_data_long %>%
  filter(Fuels %in% c("Coal", "Natural gas", "Oil products"))

# hover text for non-renewable energy data
non_renewables <- non_renewables %>%
  mutate(HoverText = paste("Year:", Year, "<br>Fuel:", Fuels, "<br>Electricity:", Value))

# convert fuels to factor levels
non_renewables$Fuels <- factor(non_renewables$Fuels, levels = c("Coal", "Natural gas", "Oil products"))

# Filter for renewable energy sources
renewables <- elec_data_long %>%
  filter(Fuels %in% c("Hydro", "Solar", "Wind"))

# Create hover text for renewable data
renewables <- renewables %>%
  mutate(HoverText = paste("Year:", Year, "<br>Fuel:", Fuels, "<br>Electricity:", Value))

# convert fuels to factor levels
renewables$Fuels <- factor(renewables$Fuels, levels = c("Hydro", "Solar", "Wind"))

# Hover text for carbon category data
carbon_category <- carbon_category %>%
  mutate(HoverText = paste("Sector:", co2_contribution, "<br>Percentage:", percentage))

# Extract year from global co2 concentration
data_co2_conc$Day <- as.Date(data_co2_conc$Day, format="%d/%m/%y")
data_co2_conc$Year <- format(data_co2_conc$Day, "%Y")

# Calculate yearly average
yearly_avg_data <- data_co2_conc %>%
  group_by(Year) %>%
  summarise(yearly_avg_co2_conc = mean(yearly_avg_co2_conc))

# spatial data for Australia
australia <- ne_states(country = "Australia", returnclass = "sf")

# Merge renewable energy with spatial data
merged_data <- australia %>%
  left_join(data_elec_perc, by = c("name" = "State")) %>% filter(!is.na(total_energy_perc))

# User Interface page

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .select-input {
        float: right;
        width: 150px;
      }
      .main-title {
        text-align: center;
      }
      .full-width {
        width: 100%;
      }
    "))
  ),
  
  titlePanel(
    div("Energy and Carbon Emissions Dashboard", class = "main-title")
  ),
  
  tabsetPanel(id = "trans_tab",
    tabPanel("Dashboard",
             fluidRow(
               column(6, plotlyOutput("carbonEmissionsPlot")),
               column(6, plotlyOutput("barPlot"))),
             
             div(
               selectInput("state", "Select State:", 
                           choices = unique(non_renewables$State), 
                           selected = "Australia"),
               class = "select-input"),
             
             fluidRow(
               column(5, plotlyOutput("co2ConcPlot")),
               column(7, plotlyOutput("nonRenewablePlot"))
               )
    ),
    tabPanel("Transition",
             fluidRow(
               column(6, leafletOutput("NonRenewablesTransitionMap")),
               column(6, leafletOutput("RenewablesTransitionMap"))
             ),
             div(
               selectInput("transition_state", "Select State:", 
                           choices = unique(renewables$State), 
                           selected = "Australia"),
               class = "select-input"
             ),
             fluidRow(
               column(12, plotlyOutput("renewablePlot")))
    ),
    tabPanel("References",
                tags$ul(
                  tags$li("Australian Energy Statistics, Table O Electricity generation by fuel type 2022-23 and 2023 | energy.gov.au. (2024, April 23).", tags$a(href="https://www.energy.gov.au/publications/australian-energy-statistics-table-o-electricity-generation-fuel-type-2022-23-and-2023", target="_blank", "https://www.energy.gov.au/publications/australian-energy-statistics-table-o-electricity-generation-fuel-type-2022-23-and-2023")),
                  tags$li("CSIRO. (n.d.). What are the sources of carbon dioxide in the atmosphere? Www.csiro.au.", tags$a(href="https://www.csiro.au/en/research/environmental-impacts/climate-change/climate-change-qa/sources-of-co2#:~:text=energy%20(burning%20fossil%20fuels%20to", target="_blank", "https://www.csiro.au/en/research/environmental-impacts/climate-change/climate-change-qa/sources-of-co2#:~:text=energy%20(burning%20fossil%20fuels%20to")),
                  tags$li("Global atmospheric CO2 concentration. (n.d.). Our World in Data.", tags$a(href="https://ourworldindata.org/grapher/global-co2-concentration", target="_blank", "https://ourworldindata.org/grapher/global-co2-concentration")),
                  tags$li("Ritchie, H., & Roser, M. (2020). Australia: CO2 Country Profile. Our World in Data.", tags$a(href="https://ourworldindata.org/co2/country/australia", target="_blank", "https://ourworldindata.org/co2/country/australia"))
                  )
             )
  )
)

# server
server <- function(input, output) {
  
  observeEvent(input$trans_tab, {
    if(input$trans_tab == "Transition") {
      shinyalert::shinyalert(
        title = "Did you know?",
        text = "In 2023, Australia achieved <b> almost 40% of energy generation from renewable energy sources, Tasmania state almost achieved a 100% renewable energy generation</b>, which shows the transition of Australia in becoming net free non-renewable energy consumption country in the coming years.",
        type = "info",
        timer = 20000,
        showConfirmButton = TRUE,
        html = TRUE
      )
    }
  })
  
  shinyalert(" ",pop_up_message, html = TRUE)
  
  elec_filter_data <- reactive({
    req(input$state) # Ensure state input is available
    subset(non_renewables, State == input$state)
  })
  
# line plot for non-renewable energy fuels
  output$nonRenewablePlot <- renderPlotly({
    df <- elec_filter_data()
    p <- ggplot(df, aes(x = Year, y = Value, color = Fuels, group = Fuels, text = HoverText)) +
      geom_line(size = 1) +
      geom_point(size = 1.5, show.legend = FALSE) +
      scale_color_manual(values = c("Coal" = "yellow4", "Natural gas" = "purple", "Oil products" = "seagreen")) +
      ggtitle(paste("Non-Renewable Energy Generation Trends in", input$state)) +
      xlab("Year") +
      ylab("Electricity (in GWh)") +
      theme_minimal() +
      theme(plot.title = element_text(size = 11))
    
    ggplotly(p, tooltip = "text")
  })

# line plot for co2 emissions
  output$carbonEmissionsPlot <- renderPlotly({
    p <- ggplot(carbon_emission, aes(x = Year, y = CO2)) +
      geom_line(color = "royalblue1", size = 0.8) +
      geom_point(color = "royalblue3", size=1) +
      ggtitle("CO2 Emissions Over Time in Australia") +
      xlab("Year") +
      ylab("CO2 Emissions (in million tonnes)") +
      theme_minimal()
    
    ggplotly(p)
  })

# bar plot for co2 emissions
  output$barPlot <- renderPlotly({
    p <- ggplot(carbon_category, aes(x = percentage , y = reorder(co2_contribution, percentage), text = HoverText)) +
      geom_bar(stat = "identity", fill= "darkcyan") +
      ggtitle("Australia CO2 Emissions 2022     ") +
      xlab("Percentage (%)") +
      ylab("Sectors") +
      theme_minimal() 
    
    ggplotly(p, tooltip = "text")
  })

# line plot for atmospheric co2 concentration   
  output$co2ConcPlot <- renderPlotly({
    yearly_avg_data$Year <- as.numeric(yearly_avg_data$Year)
    
    p <- ggplot(yearly_avg_data, aes(x = Year, y = yearly_avg_co2_conc)) +
      geom_line(color = "maroon4", size = 0.8) +
      geom_point(color = "maroon", size = 1) +
      ggtitle("Atmospheric CO2 Concentration Trends") +
      xlab("Year") +
      ylab("Yearly Average CO2 Concentration (ppm)") +
      theme_minimal() +
      theme(plot.title = element_text(size = 11))
    
    ggplotly(p)
  })

#  Non-renewable energy map plot
  output$NonRenewablesTransitionMap <- renderLeaflet({
    
    bins <- c(0, 20, 40, 60, 80, 100)
    pal <- colorBin(palette = "YlOrRd", domain = merged_data$non_renewables_perc, bins = bins)
    
    leaflet(merged_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(non_renewables_perc),
        weight = 2,
        opacity = 1,
        color = 'white',
        dashArray = '3',
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "grey",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        
        label = ~paste(
          name, "|",
          "Renewables: ", round(non_renewables, 2), "gWh |",
          "State Energy: ", round(total_state_energy, 2), "gWh"),
        
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 6px"),
          textsize = "8px",
          direction = "auto")) %>%
      
      addLegend(pal = pal, values = ~non_renewables_perc, title = "Non-Renewable Energy Generation (%)", opacity = 0.7, position = "bottomright",
        labFormat = labelFormat(suffix = "%")
      )
  })

# renewable energy map plot
  output$RenewablesTransitionMap <- renderLeaflet({
    
    bins <- c(0, 20, 40, 60, 80, 100)
    pal <- colorBin(palette = "YlOrRd", domain = merged_data$renewables_perc, bins = bins)
    
    leaflet(merged_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(renewables_perc),
        weight = 2,
        opacity = 1,
        color = 'white',
        dashArray = '3',
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "grey",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        
        label = ~paste(
          name, "|",
          "Renewables: ", round(renewables, 2), "gWh |",
          "State Energy: ", round(total_state_energy, 2), "gWh"),
        
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), 
                                    textsize = "10px", direction = "auto")) %>% 
      addLegend(pal = pal, values = ~renewables_perc, title = "Renewable Energy Generation (%)", opacity = 0.7, position = "bottomright", labFormat = labelFormat(suffix = "%"))
  })

# Enter input state for renewables 
  trans_data <- reactive({
    req(input$transition_state) 
    subset(renewables, State == input$transition_state)
  })
  
# line plot for renewables 
  output$renewablePlot <- renderPlotly({
    renewable_trans_data <- trans_data()
    
    p <- ggplot(renewable_trans_data, aes(x = Year, y = Value, color = Fuels, group = Fuels, text = HoverText)) +
      geom_line(size = 1) +
      geom_point(size = 1.5, show.legend = FALSE) +
      scale_color_manual(values = c("Solar" = "salmon2", "Wind" = "#377EB8", "Hydro" = "#4DAF4A")) +
      ggtitle(paste(" Renewable Energy Generation Trends in", input$transition_state)) +
      xlab("Year") +
      ylab("Electricity (in GWh)") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
}

# Run Shiny app
shinyApp(ui = ui, server = server)

#rsconnect::deployApp('/Users/dinesh/Documents/Data Science/sem 3/Data Viz/Assignment 3/')
