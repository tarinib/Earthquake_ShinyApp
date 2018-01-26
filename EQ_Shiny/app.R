
## SHINY APP
## Author: Tarini Bhatnagar
## date: "01/21/2018"

#----------------------------------------------------------------------------------------------------------------
####LOADING REQUIRED PACKAGES
#----------------------------------------------------------------------------------------------------------------
library(shiny)
library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(shinythemes)
library(stringr)
library(readr)
library(leaflet)
library(scales)
library(DT)


options(shiny.sanitize.errors = FALSE)

#----------------------------------------------------------------------------------------------------------------
####DATA WRANGLING
#----------------------------------------------------------------------------------------------------------------
#Loading raw data
dat <- read_csv("database.csv")

#Selecting specific columns

eq_data <- dat %>% 
  filter(Type=="Earthquake") %>% 
  select("Date", "Time", "Latitude", "Longitude", "Type", "Depth", "Magnitude", "Magnitude Type", "ID", "Status")

#Replacing spaces in column names with _
names(eq_data)<-names(eq_data)%>% 
  stringr::str_replace_all("\\s","_")

#Removing rows with NA values in time
eq_data <- eq_data[complete.cases(eq_data),]

#Adding year
eq_data <- eq_data %>% 
  mutate(Year= substring(Date,7,10))

#Map
world <- map_data("world")
world <- world[world$region != "Antarctica",] # intercourse antarctica

#----------------------------------------------------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("sandstone"),
   
   # Application title
   titlePanel("Significant Earthquakes 1965-2016"),
   
   # Sidebar with a slider input 
   
      
   # Main Panel
   #conditionalPanel(#condition = tab name',
     #sudebar

   tabsetPanel(type="tab",
               tabPanel("Data Explorer", 
                        br(),
                        br(),
                        h6("Here, you can explore the entire dataset. Zoom in/out and click on specific pins to get to know the earthquakes personally!"),
                        br(),
                        br(),
                        leafletOutput("leaflet")
                ),
                tabPanel("Interactive Map", fluid=TRUE,
                         sidebarLayout(
                           sidebarPanel(
                             h4("Interactive options"),
                             br(),
                             sliderInput("yearInput", "Year Range", 
                                         min = 1965, max = 2016,
                                         value = c(1966, 2005), sep = "",step=1),
                             sliderInput("magInput", "Magnitude", 
                                         min = 5.5, max = 9.1,
                                         value = c(6.6, 9.1), sep = "",step=0.1),
                             sliderInput("depthInput", "Depth Range (km)", 
                                         min = 0, max = 700,
                                         value = c(110, 680), sep = "",step=5),
                             radioButtons("colorbyInput", "Color By",
                                          choices=c("Magnitude of earthquake"="Magnitude","Depth of epicenter"="Depth"),
                                          selected="Magnitude"),

                             br(),
                             checkboxGroupInput("magtypeInput", "Magnitude Type",
                                                choices=c('MS: Surface Wave' = 'MS',
                                                          'ML: Local (Richter)' = 'ML',
                                                          'MB: Body Wave' = 'MB',
                                                          'MW: Moment' = 'MW',
                                                          'MD: Duration' = 'MD'),
                                                selected=c("MS","ML","MB","MW","MD")),
                             a(href="https://www.usgs.gov/faqs/moment-magnitude-richter-scale-what-are-different-magnitude-scales-and-why-are-there-so-many", "What do these magnitude types mean?")
                             ),
                           mainPanel(
                             br(),
                             br(),
                             h6(" Here, you can play with the interactive options on the menu bar on the left to personalize the dataset. If events do not show up, that means you need to select the correct scale(magnitude type). For example, for depth ~500-700, you need to select MB magnitude."),
                             br(),
                             br(),
                             plotOutput("int_map"), 
                             h6("Try plotting a larger range of years and magnitude. Do you notice any patterns? For more information, check out the informations tab!")
                             )
                           )
                         ),
               tabPanel("Distribution",
                        br(),
                        br(),
                        h6("Play with the magntiude range slider"),
                        br(),
                        h6("Earthquakes are always occurring somewhere in the world. Smaller magntiude earthquakes happen every year, while major earthquakes happen, on average, about once a year. As the magntiude increases, frequency of those events decreases."),
                        br(),
                        br(),
                        br(),
                        plotOutput("hist")
                        ),
               tabPanel("Data",
                        dataTableOutput("table")
               ),
               tabPanel("Information",
                        tags$iframe(style="height:400px; width:100%; scrolling=yes", src="PlateTectonics.pdf")
                        )
               )
)


# Define server logic
server <- function(input, output) {
  
  #First tab(Data Explorer)
  output$leaflet <- renderLeaflet({
    
    eq_data %>%
      leaflet() %>%
      setView(0, 0, zoom = 2) %>% 
      addTiles() %>%
      addMarkers(lat=eq_data$Latitude, lng=eq_data$Longitude, clusterOptions = markerClusterOptions(),
                 popup= paste(eq_data$Type,
                              "<br><strong>Date: </strong>", eq_data$Date,
                              "<br><strong>Time: </strong>", eq_data$Time,
                              "<br><strong>Type: </strong>", eq_data$Type,
                              "<br><strong>Magnitude: </strong>", eq_data$Magnitude,
                              "<br><strong>Magntiude Type: </strong>", eq_data$Magntiude_Type,
                              "<br><strong>Depth: </strong>", eq_data$Depth
                 ))
  })
  
  #Second tab(Interactive Map)
  output$int_map <- renderPlot({
    
    filter_year <-eq_data %>%
      filter(Year >= input$yearInput[1],
             Year <= input$yearInput[2],
             Magnitude >= input$magInput[1],
             Magnitude <= input$magInput[2],
             Magnitude_Type %in% input$magtypeInput,
             Depth >= input$depthInput[1],
             Depth <= input$depthInput[2])
    
    if (input$colorbyInput=="Depth"){
      ggplot()+
        geom_map(data=world, map=world,aes(x=long, y=lat, map_id=region), colour="white", fill="gray50", alpha=0.5)+
        geom_point(data=filter_year,aes(x = Longitude, y = Latitude, color=Depth),alpha=0.8)+
        labs(title="Earthquakes by Depth (km)",x="Latitude (degrees)", y="Longitude (degrees)")+
        scale_colour_gradient(low = "orange1", high = "green4",guide = guide_colorbar(reverse=TRUE))+
        theme_classic()
      
    }
    else
      ggplot()+
        geom_map(data=world, map=world,aes(x=long, y=lat, map_id=region), colour="white", fill="gray50", alpha=0.5)+
        geom_point(data=filter_year,aes(x = Longitude, y = Latitude,color=Magnitude),alpha=0.8)+
        labs(title="Earthquakes by Magnitude",x="Latitude (degrees)", y="Longitude (degrees)")+
        scale_colour_gradient(low = "red", high = "blue", guide = guide_colorbar(reverse=TRUE))+
        theme_classic()
    
  })
  
  #Third tab(Distribution)
  output$hist <- renderPlot({
    
    filter_year <-eq_data %>%
      filter(Year >= input$yearInput[1],
             Year <= input$yearInput[2],
             Magnitude >= input$magInput[1],
             Magnitude <= input$magInput[2],
             Magnitude_Type %in% input$magtypeInput,
             Depth >= input$depthInput[1],
             Depth <= input$depthInput[2])
    
      ggplot(filter_year, aes(Magnitude))+
        geom_histogram(bins=50, binwidth=0.1,fill="grey50", color="purple", alpha=0.5)+
        labs(title="Frequency of Earthquakes by Magnitude")+
        theme_classic()
      
  })
  
 #Fourth tab(Rendered Data)
 output$table <- renderDataTable({
  filter_year <-eq_data %>%
    filter(Year >= input$yearInput[1],
           Year <= input$yearInput[2],
           Magnitude >= input$magInput[1],
           Magnitude <= input$magInput[2],
           Magnitude_Type %in% input$magtypeInput,
           Depth >= input$depthInput[1],
           Depth <= input$depthInput[2]) %>% 
    select("Date","Time", "Latitude", "Longitude", "Depth", "Magnitude", "Magnitude_Type", "ID")
  datatable(filter_year, option = list(scrollX = TRUE, pageLength = 10))  
 })
 
 #Fifth tab(Information)
 #This has already been included in the UI.
}

# Run the application 
shinyApp(ui = ui, server = server)

