# Use pacman pload to load the required packages;this is more efficient as it saves time if the package is not installed as pacman installs it automatically
pacman::p_load(tidyverse, ggplot2, plotly, data.table, shinyWidgets, shiny, leaflet, DBI, RSQLite, sf, shinycssloaders, shinysky, DT)



# Connect to the database to source data
con <- dbConnect(RSQLite::SQLite(), "data/Biodiversity.db")

# Subset Poland dataset from the table in the database
my_poland_data <- dbReadTable(con, "Biodiversity")

# disconnect from database
dbDisconnect(con)



# Clean the data
# Change variable type
my_poland_data <- my_poland_data %>% 
  mutate(longitudeDecimal = as.numeric(longitudeDecimal)) %>% 
  mutate(latitudeDecimal = as.numeric(latitudeDecimal)) %>% 
  mutate(individualCount = as.numeric(individualCount)) %>% 
  mutate(scientificName = as.factor(scientificName)) %>% 
  mutate(vernacularName = as.factor(vernacularName)) %>% 
  mutate(eventDate = as.Date(eventDate)) %>% 
  mutate(modified = as.Date(modified))


# Read the poland shape file into R as an object
polshape <- st_read("data/POL_adm1.shp")

# Set the projections
polshape <- st_transform(polshape, '+proj=longlat +datum=WGS84')






#  Assign dynamic color depending on the count of the individual species selected in the map
getColor <- function(my_poland_data) {
  sapply(my_poland_data$individualCount, function(individualCount) {
    if(individualCount <= 4) { # When the count is below or equal to 4 color red 
      "red"
    } else if(individualCount <= 10) { # When the count is below or equal to 10 color orange 
      "orange"
    } else { # When the count is greater than 10 color green 
      "green"
    } })
}

# Add Custom Icons for the markers in leaflet map
icons <- awesomeIcons(
  icon = 'tree', 
  iconColor = 'black',
  library = 'fa',
  markerColor = getColor(my_poland_data), # get color from vector object
  squareMarker = F,
)



module_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
   sidebarLayout(
      sidebarPanel(
        h2(HTML("<strong>Search by species'</strong>")),
        
        # Search bar for Vernacular name
        selectizeInput(ns('ven'), 
                       label = "Vernacular Name", 
                       choices = NULL,
                       multiple = T,
                       options = list(
                         create = T,
                         placeholder = "",
                         maxItems = '1'),
                       width = "350px"
        ),
        
        # Search bar for Scientific name
        selectizeInput(ns('sci'), 
                       label = "Scientific Name", 
                       choices = NULL,
                       multiple = T,
                       options = list(
                         create = T,
                         placeholder = "",
                         maxItems = '1'),
                       width = "350px"
        )
      ),
      
      mainPanel(
        fluidRow(
          column(width = 5, style = "background-color: #026352; color: white; border-radius: 5px 5px 0 0;", # Custom inline css that adds a border and specifies height
                 br(),
                 # Custom dynamic map title input
                 textOutput(ns("map.title")), 
                 br(),
          ),
          
          column(width = 1),
          
          column(width = 5, style = "background-color: #026352; color: white;  border-radius: 5px 5px 0 0;", 
                 br(),
                 # Custom dynamic  plot input
                 textOutput(ns("plot.title")), 
                 br(),
          )
        ), # end of fluidrow 
        
        fluidRow(
          column(width = 5,style = "height:130px; background-color: #99E4D2; color: black;", 
                 br(),
                 # Custom dynamic map info input
                 textOutput(ns("map.info")), 
                 br(),
          ),
          
          column(width = 1),
          
          column(width = 5, style = "height:130px; background-color: #99E4D2; color: black;", 
                 br(),
                 # Custom dynamic plot info input
                 textOutput(ns("plot.info")), 
                 br(),
          )
        ), # end of fluidrow 
        
        
        fluidRow(
          # add input object for the leaflet map
          column(width = 5,  style = "height:450px; border: 4px solid #99E4D2; border-radius: 0 0 5px 5px;",
                 # add input object for the leaflet map
                 wellPanel(withSpinner(leafletOutput(ns("map"))))
          ),
          
          column(width = 1),
          
          # add input object for the time series chart
          column(width = 5,  style = "height:450px; border: 4px solid #99E4D2; border-radius: 0 0 5px 5px;", 
                 # add input object for the leaflet map
                 wellPanel(withSpinner(plotlyOutput(ns("Total_Count"))))
          )
        ) # end of fluidrow 
      ) # end of main pannel
    )  
  )
  
}

# Create the module sever  function
module_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Populate the choices to search by Vernacular name
      updateSelectizeInput(session, 
                           'ven', 
                           choices = unique(my_poland_data$vernacularName), 
                           selected = paste(as.character(my_poland_data$vernacularName[1])),
                           server = TRUE
      )
      
      # Populate the choices to search by Scientific name
      updateSelectizeInput(session, 
                           'sci', 
                           choices = unique(my_poland_data$scientificName), 
                           selected = paste(as.character(my_poland_data$scientificName[1])), 
                           server = TRUE
      )
      
      # Update the search field for Vernacular name when the Scientific name is updated
      observeEvent(input$ven, 
                   updateSelectInput(session, 
                                     input = 'sci', 
                                     selected=my_poland_data[match(input$ven, my_poland_data[, 2]), 1])
      )
      # Update the search field for Scientific name when the Vernacular name is updated
      observeEvent(input$sci, 
                   updateSelectInput(session, 
                                     input = 'ven', 
                                     selected=my_poland_data[match(input$sci, my_poland_data[, 1]), 2])
      )
      
      # Create a reactive data function
      Map.Poland.df <- reactive({
        df <- my_poland_data %>%
          filter(vernacularName  %in%  input$ven) %>% # Filter by inputs from search Vernacular Name
          filter(scientificName  %in%  input$sci) %>% # Filter by inputs from search Vernacular Name
          req(update)
      })
      
      
      # Generate a leaflet output using the reactive data
      output$map<-renderLeaflet({
        
        leaflet(Map.Poland.df()) %>%
          addProviderTiles('CartoDB', 
                           options = providerTileOptions(maxNativeZoom=100,maxZoom=100)) %>% #Use the CartoDB as the background
          addPolygons(data=polshape
                      ,weight=5,
                      col = '#04B699', 
                      fillColor = '#034E73',
                      fillOpacity = 1) %>% #Call the latitude and longitude variables from shapefile object
          addAwesomeMarkers(~longitudeDecimal, ~latitudeDecimal,
                            popup = ~paste0("<h3 align = 'center' style = 'color:red;' ><u>Species</u></h3>",
                                            "<strong>Vernacular Name: </strong>", vernacularName, "<br>",
                                            "<strong>Scientific Name: </strong>", scientificName),
                            icon=icons)
      })
      
      # Generate a ggplotly timeseries  output using the reactive data
      output$Total_Count <- renderPlotly({
        validate(
          need(nrow(Map.Poland.df()) > 0, message = "Loading... ")  # Add a validate message when the plot tile is not populated with data
        )
        ggplot(Map.Poland.df(), aes(modified, individualCount)) + 
          geom_line(color= '#034E73', size = 1.5) +
          geom_point(col = "#04B699", size = 1)+
          scale_x_date(limits = as.Date(c("2014-01-01","2020-03-08"))) + # add a limit to the dates shown
          labs(x = "Time Series", y = paste0("Count of ", input$sci, " by day")) +
          theme_bw()
      })
      
      
      # Add a custom dynamic title for the map
      output$map.title <- renderText({ 
        paste0("Map - ",input$ven, " (vernarcular name) ", input$sci, " (scientific name)") 
      })
      
      # Add a custom dynamic title for the rotisseries plot
      output$plot.title <- renderText({ 
        paste0("Plot - ",input$ven, " (vernarcular name) ", input$sci, " (scientific name)") 
      })
      
      # Add  guide information to the map
      output$map.info <- renderText({ 
        paste0("Map shows the biodiversity species of Poland. 
           The default view is a subset of the species indentifed in the search filed. 
           To change the view; click either the search area for 'Vernacular Name' or 'Scientific Name' and search for the desired species") 
      })
      
      # Add  guide information to the plot
      output$plot.info <- renderText({ 
        paste0("The plot shows a timelime for species in the map from year 2014-2020. 
           The default view is a subset of the species indentifed in the search filed. 
           To change the view; click either the search area for 'Vernacular Name' or 'Scientific Name' and search for the desired species") 
      })
      
    }
  )
}
