
## SHINY APP
## Author: Tarini Bhatnagar
## date: "01/25/2018"

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
   
   # Adding data source
   h6("Data: "),
   a(href="https://earthquake.usgs.gov/", "USGS (United States Geological Survey)"),
   h6("Data provided by: "),
   a(href="https://www.kaggle.com/usgs/earthquake-database", "Kaggle"),
   br(),

   # Defining various tabs
   tabsetPanel(type="tab",
               
               # Tab1: Data Explorer with leaflet worldmap
               
               tabPanel("Data Explorer", 
                        br(),
                        br(),
                        h5("Here, you can explore the entire dataset. Zoom in/out and click on specific pins to get to know the earthquakes personally!"),
                        br(),
                        br(),
                        leafletOutput("leaflet")
                ),
               
               # Tab2: Interactive map with sidebar options
               
                tabPanel("Interactive Map", fluid=TRUE,
                         
                         #Defining sidebar input parameters
                         
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
                             a(href="https://earthquake.usgs.gov/learn/glossary/?term=magnitude", "What is magnitude?"),
                             br(),
                             a(href="https://earthquake.usgs.gov/learn/topics/determining_depth.php", "How do we determine depth?"),
                             br(),
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
                           
                           #Defining main page plot
                           
                           mainPanel(
                             br(),
                             br(),
                             h5(" Here, you can play with the interactive options on the menu bar on the left to personalize the dataset"),
                             br(),
                             h5("If events do not show up, that means you need to select the correct scale(magnitude type). For example, for depth ~500-700, you need to select MB magnitude."),
                             br(),
                             br(),
                             h5("Try plotting a larger range of years and magnitude. Do you notice any patterns? For more information, check out the informations tab!"),
                             plotOutput("int_map")
                             )
                           )
                         ),
               
               # Tab3: Frequency plot with sidebar options
               
               tabPanel("Frequency Plot", fluid=TRUE,
                        
                        #Defining sidebar input parameters
                        
                        sidebarLayout(
                          sidebarPanel(
                            h4("Interactive options"),
                            br(),
                            sliderInput("yearInput_f", "Year Range", 
                                        min = 1965, max = 2016,
                                        value = c(1966, 2005), sep = "",step=1),
                            sliderInput("magInput_f", "Magnitude", 
                                        min = 5.5, max = 9.1,
                                        value = c(6.6, 9.1), sep = "",step=0.1),
                            sliderInput("depthInput_f", "Depth Range (km)", 
                                        min = 0, max = 700,
                                        value = c(110, 680), sep = "",step=5),
                            a(href="https://earthquake.usgs.gov/learn/glossary/?term=magnitude", "What is magnitude?"),
                            br(),
                            a(href="https://earthquake.usgs.gov/learn/topics/determining_depth.php", "How do we determine depth?"),
                            br(),
                            br(),
                            checkboxGroupInput("magtypeInput_f", "Magnitude Type",
                                               choices=c('MS: Surface Wave' = 'MS',
                                                         'ML: Local (Richter)' = 'ML',
                                                         'MB: Body Wave' = 'MB',
                                                         'MW: Moment' = 'MW',
                                                         'MD: Duration' = 'MD'),
                                               selected=c("MS","ML","MB","MW","MD")),
                            a(href="https://www.usgs.gov/faqs/moment-magnitude-richter-scale-what-are-different-magnitude-scales-and-why-are-there-so-many", "What do these magnitude types mean?")
                          ),
                          
                          #Defining main page plot
                          
                        mainPanel(
                          br(),
                          br(),
                          h5("Play with the magntiude range slider and other filters on the left and observe the frequency of different magnitude earthquakes."),
                          br(),
                          h5("Earthquakes are always occurring somewhere in the world. Smaller magntiude earthquakes happen every year, while major earthquakes happen, on average, about once a year. As the magntiude increases, frequency of those events decreases."),
                          br(),
                          br(),
                          br(),
                          plotOutput("hist")
                          )
                        )
               ),
               
               # Tab4: Render Data with sidebar options
               
               tabPanel("Data", fluid=TRUE,
                        
                        #Defining sidebar input parameters
                        
                        sidebarLayout(
                          sidebarPanel(
                            h4("Interactive options"),
                            br(),
                            sliderInput("yearInput_d", "Year Range", 
                                        min = 1965, max = 2016,
                                        value = c(1966, 2005), sep = "",step=1),
                            sliderInput("magInput_d", "Magnitude", 
                                        min = 5.5, max = 9.1,
                                        value = c(6.6, 9.1), sep = "",step=0.1),
                            sliderInput("depthInput_d", "Depth Range (km)", 
                                        min = 0, max = 700,
                                        value = c(110, 680), sep = "",step=5),
                            a(href="https://earthquake.usgs.gov/learn/glossary/?term=magnitude", "What is magnitude?"),
                            br(),
                            a(href="https://earthquake.usgs.gov/learn/topics/determining_depth.php", "How do we determine depth?"),
                            br(),
                            br(),
                            checkboxGroupInput("magtypeInput_d", "Magnitude Type",
                                               choices=c('MS: Surface Wave' = 'MS',
                                                         'ML: Local (Richter)' = 'ML',
                                                         'MB: Body Wave' = 'MB',
                                                         'MW: Moment' = 'MW',
                                                         'MD: Duration' = 'MD'),
                                               selected=c("MS","ML","MB","MW","MD")),
                            a(href="https://www.usgs.gov/faqs/moment-magnitude-richter-scale-what-are-different-magnitude-scales-and-why-are-there-so-many", "What do these magnitude types mean?")
                          ),
                          
                          #Defining main page plot
                          
                          mainPanel(
                            br(),
                            br(),
                            h5(" Here, you can play with the interactive options on the menu bar on the left to filter the dataset and search for specific events."),
                            br(),
                            dataTableOutput("table")
                            )
                        )
               ),
               
               # Tab5: Information tab with an embedded pdf
               
               tabPanel("Information",
                        tags$iframe(style="height:400px; width:100%; scrolling=yes", src="PlateTectonics.pdf")
                        )
               )
)


# Define server logic
server <- function(input, output) {
  
  #First tab(Data Explorer)
  output$leaflet <- renderLeaflet({
    
    #Adding world map leaflet and even markers
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
    
    #Filtering data according to input parameters
    filter_year <-eq_data %>%
      filter(Year >= input$yearInput[1],
             Year <= input$yearInput[2],
             Magnitude >= input$magInput[1],
             Magnitude <= input$magInput[2],
             Magnitude_Type %in% input$magtypeInput,
             Depth >= input$depthInput[1],
             Depth <= input$depthInput[2])
    
    #Plot for events color coded by depth
    if (input$colorbyInput=="Depth"){
      ggplot()+
        geom_map(data=world, map=world,aes(x=long, y=lat, map_id=region), colour="white", fill="gray50", alpha=0.5)+
        geom_point(data=filter_year,aes(x = Longitude, y = Latitude, color=Depth),alpha=0.7)+
        labs(title="Earthquakes by Depth (km)",x="Latitude (degrees)", y="Longitude (degrees)")+
        scale_colour_gradient(low = "orange1", high = "green4",guide = guide_colorbar(reverse=TRUE))+
        theme_classic()
      
    }
    else
      
      #Plot for events color coded by magnitude
      ggplot()+
        geom_map(data=world, map=world,aes(x=long, y=lat, map_id=region), colour="white", fill="gray50", alpha=0.5)+
        geom_point(data=filter_year,aes(x = Longitude, y = Latitude,color=Magnitude),alpha=0.7)+
        labs(title="Earthquakes by Magnitude",x="Latitude (degrees)", y="Longitude (degrees)")+
        scale_colour_gradient(low = "red", high = "blue", guide = guide_colorbar(reverse=TRUE))+
        theme_classic()
    
  })
  
  #Third tab(Distribution)
  output$hist <- renderPlot({
    
    #Filtering data according to input parameters
    filter_year <-eq_data %>%
      filter(Year >= input$yearInput_f[1],
             Year <= input$yearInput_f[2],
             Magnitude >= input$magInput_f[1],
             Magnitude <= input$magInput_f[2],
             Magnitude_Type %in% input$magtypeInput_f,
             Depth >= input$depthInput_f[1],
             Depth <= input$depthInput_f[2])
    
    #Plot for frequency ditribution of events
      ggplot(filter_year, aes(Magnitude))+
        geom_histogram(bins=50, binwidth=0.1,fill="navy", color="yellow3")+
        labs(title="Frequency of Earthquakes by Magnitude", y="Number of events")+
        theme_classic()
      
  })
  
 #Fourth tab(Rendered Data)
 output$table <- renderDataTable({
   
  #Filtering data according to input parameters
  filter_year <-eq_data %>%
    filter(Year >= input$yearInput_d[1],
           Year <= input$yearInput_d[2],
           Magnitude >= input$magInput_d[1],
           Magnitude <= input$magInput_d[2],
           Magnitude_Type %in% input$magtypeInput_d,
           Depth >= input$depthInput_d[1],
           Depth <= input$depthInput_d[2]) %>% 
    select("Year", "Date","Time", "Latitude", "Longitude", "Magnitude", "Magnitude_Type", "Depth","ID")
  
  #Producing DT scrollable table filtered according to inputs above
  datatable(filter_year, option = list(scrollX = TRUE, pageLength =11))  
 })
 
 #Fifth tab(Information)
 #This has already been included in the UI.
}

# Run the application 
shinyApp(ui = ui, server = server)

