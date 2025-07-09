library(shiny)
library(tidyverse)
library(here)
library(shinythemes)
library(DT)
library(leaflet)

# load escapement data
esc_data_raw <- read_csv(here("data/CompiledEsc.csv"))

# create clean escapement data
esc_data_cleaned <- esc_data_raw %>% 
  select(Species, sampleYear, annualCount, SASAP.Region, DataSource, Lat, Lon)

# create Bristol Bay escapement data
bristol_bay <- esc_data_raw %>% 
  filter(SASAP.Region == "Bristol Bay") %>% 
  select(Species, LocationID, sampleYear, annualCount, Lat, Lon)


ui <- fluidPage(
  
  # set shiny theme
  theme = shinytheme("lumen"),
  
  navbarPage("Alaskan Salmon",
             
             tabPanel("Annual Alaskan Salmon Escapement",
             sidebarLayout(
               sidebarPanel(  
                 h1("Filter Data"),
                 
                 # Species selector
                 radioButtons(inputId = "salmon_species", 
                              label= "Salmon Species:",
                              c("Chinook" = "chinook",
                                "Chum" = "chum",
                                "Coho" = "coho",
                                "Pink" = "pink",
                                "Sockeye" = "sockeye",
                                "All" = "all"), #eventually add an 'All'
                              selected = "all"),
                 
                 # Region selector
                 radioButtons(inputId = "region_selection",
                              label= "Region:",
                              c(
                                "Alaska Peninsula and Aleutian Islands" = "Alaska Peninsula and Aleutian Islands", 
                                "Bristol Bay"  = "Bristol Bay",
                                "Chignik" = "Chignik",
                                "Cook Inlet" = "Cook Inlet",
                                "Copper River"  = "Copper River",
                                "Kodiak" = "Kodiak",
                                "Kotzebue" = "Kotzebue",
                                "Kuskokwim" = "Kuskokwim",
                                "Southeast" = "Southeast",
                                "Prince William Sound"  = "Prince William Sound",
                                "Yukon" = "Yukon",
                                "All" = "all"), #eventually add an 'All'
                              selected = "all"),
                 
                 # year slider
                 sliderInput(inputId = "years",
                             label = "Years:",
                             min = min(esc_data_raw$sampleYear),
                             max = max(esc_data_raw$sampleYear),
                             step = 1,
                             value = c(min(esc_data_raw$sampleYear), max(esc_data_raw$sampleYear)),
                             sep = "")
                 
                 
               ),
               
               mainPanel(
                 
                 leafletOutput(outputId = "salmon_map", height = 400),
                 
                br(),
                
                DTOutput("salmon_table")
                 
               )
             )),
             tabPanel("Bristol Bay Explorer",
                      sidebarLayout(
                        sidebarPanel(
                          h1("Filter Data"),
                          # Species selector (2)
                          radioButtons(inputId = "salmon_species_2", 
                                       label= "Salmon Species:",
                                       c("Chinook" = "chinook",
                                         "Chum" = "chum",
                                         "Coho" = "coho",
                                         "Pink" = "pink",
                                         "Sockeye" = "sockeye",
                                         "All" = "all"), #eventually add an 'All'
                                       selected = "all"),
                          # Location selector
                          radioButtons(inputId = "location_selection",
                                       label= "Location:",
                                       c(
                                         "Alagnak River" = "Alagnak River",
                                         "Egegik River" = "Egegik River",
                                         "Igushik River" = "Igushik River",
                                         "Kvichak River" = "Kvichak River",
                                         "Naknek River" = "Naknek River",
                                         "Nushagak River" = "Nushagak River",
                                         "Nuyakuk River" = "Nuyakuk River",
                                         "Togiak River" = "Togiak River",
                                         "Ugashik River" = "Ugashik River",
                                         "Wood River" = "Wood River",
                                         "All" = "all"), #eventually add an 'All'
                                       selected = "all")
                          
                        ),
                        mainPanel(
                          leafletOutput(outputId = "salmon_map_2", height = 400),
                          
                          br(),
                          
                          h4("Click on a location above to display escapement over time"),
                          
                          br(),
                          
                          plotOutput("clicked_plots")
                        )
                      )
                      )
    
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    
    data <- esc_data_cleaned
    
    if(input$salmon_species != "all"){
      data <- data %>% filter(Species == input$salmon_species)
    } else{
      data
    }
    
    if (input$region_selection != "all"){
      data <- data %>% filter(SASAP.Region == input$region_selection)
    } else {data}
    
    data <- data %>% 
      filter(sampleYear >= input$years[1], sampleYear <= input$years[2])
    
  })
  
  # salmon table output
  output$salmon_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 25))
  })
  
  # Salmon Map (1)
  output$salmon_map <- renderLeaflet({
    
    data_leaflet <- filtered_data()
    
    leaflet(data_leaflet) %>% 
      addTiles() %>% 
      addCircleMarkers(
        lng = ~Lon,
        lat = ~Lat,
        radius = 1
      )
    
  })
  
  # filtered data (2)
  filtered_data_2 <- reactive({
    
    data <- bristol_bay
    
    if(input$salmon_species_2 != "all"){
      data <- data %>% filter(Species == input$salmon_species_2)
    } else{
      data
    }
    
    if (input$location_selection != "all"){
      data <- data %>% filter(LocationID == input$location_selection)
    } else {data}
    
    
  })
  
  
  # Salmon Map (2)
  output$salmon_map_2 <- renderLeaflet({
    
    data_leaflet <- filtered_data_2()
    
    leaflet(data_leaflet) %>% 
      addTiles() %>% 
      addCircleMarkers(
        lng = ~Lon,
        lat = ~Lat,
        layerId = ~LocationID,
        radius = 5,
        stroke = FALSE,
        fillOpacity = 0.7,
        label = ~LocationID
        # labelOptions = labelOptions(noHide = TRUE, direction = "bottom")
      )
    
  })
  
  # value to store click
  clicked_location <- reactiveVal()
  
  # observe click
  observeEvent(input$salmon_map_2_marker_click, {
    click <- input$salmon_map_2_marker_click
    clicked_location(click$id)
  })
  
  # filter data with click location
  click_data <- reactive({
    data <- bristol_bay %>% 
      filter(LocationID == clicked_location())
  })
  
  # create plot
  output$clicked_plots <- renderPlot({
    
    # save click to be able to detect is user has clicked yet
    click <- clicked_location()
    
    if(is.null(click)){
      ggplot() + # create a blank plot to be displayed until user clicks location
        theme_minimal() + # set ggplot theme
        ylim(0,250000) + # set arbitrary limit for y axis
        xlim(1921,2017) + # all will have the same x-axis range
        xlab("Year") +
        ylab("Annual Escapemenet") +
        ggtitle("No Location Selected")
    } else { # if user has click, will generate graph
      
      # getting data
      data <-click_data()
      
      # create plot
      ggplot(data, aes(x = sampleYear, y = annualCount, color = Species)) +
        geom_line() +
        geom_point() +
        theme_minimal() +
        xlim(1921,2017) + # all will have the same x-axis range
        xlab("Year") +
        ylab("Annual Escapemenet") +
        ggtitle(paste("Annual Escapement for", click)) # title changes based on location selected
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
