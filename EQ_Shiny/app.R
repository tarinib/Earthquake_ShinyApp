
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
library(gganimate)
devtools::install_github("dgrtwo/gganimate")
library(leaflet)
library(scales)


options(shiny.sanitize.errors = FALSE)

#----------------------------------------------------------------------------------------------------------------
####DATA WRANGLING
#----------------------------------------------------------------------------------------------------------------
#Loading raw data
dat <- read_csv("database.csv")

#Selecting specific columns

eq_data <- dat %>% 
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
   sidebarLayout(
      sidebarPanel(
        h4("Interactive options"),
        h6("(To be used with animated and static map)"),
        br(),
        sliderInput("yearInput", "Year Range", 
                    min = 1965, max = 2016,
                    value = c(1966, 2005), sep = "",step=1),
        sliderInput("magInput", "Magnitude", 
                    min = 5.5, max = 9.1,
                    value = c(6.6, 9.1), sep = "",step=0.1),
        checkboxGroupInput("magtypeInput", "Magnitude Type",
                           choices=c("MS","ML","MB","MW","MD"),
                           selected=c("MS","ML","MB","MW","MD")),
        h6("(MS: Surface Wave, ML: Local (Richter), MB: Body Wave, MW: Moment , MD: Duration)"),
        br(),
        sliderInput("depthInput", "Depth Range (km)", 
                    min = 0, max = 700,
                    value = c(110, 680), sep = "",step=5),
        radioButtons("colorbyInput", "Color By",
                     choices=c("Depth","Magnitude"),
                     selected="Magnitude"),
        checkboxGroupInput("typeInput", "Type of event",
                           choices=c("Earthquake","Nuclear Explosion"),
                           selected="Earthquake")
      ),
      
      # Main Panel
      mainPanel(
        tabsetPanel(type="tab",
                    tabPanel("Data Explorer", 
                             br(),
                             br(),
                             h6("Here, you can explore the entire dataset. Zoom in/out and click on specific pins to get to know the earthquakes personally!"),
                             br(),
                             br(),
                             leafletOutput("leaflet")),
                    tabPanel("Animated Map",
                             br(),
                             br(),
                             h6("Here, you can see the animation for your selected parameters changing over time(years)"),
                             imageOutput("anim_map")),
                    tabPanel("Static Map", 
                             br(),
                             br(),
                             h6(" Here, you can play with the interactive options on the menu bar on the left to personalize the dataset. If events do not show up, that means you need to select the correct scale(magnitude type). For example, for depth ~500-700, you need to select MB magnitude."),
                             br(),
                             br(),
                             plotOutput("stat_map"), 
                             h6("Try plotting a larger range of years and magnitude. Do you notice any patterns? For more information, check out the informations tab!")),
                    tabPanel("Distribution",
                             br(),
                             br(),
                             h6("Play with the magntiude range slider"),
                             br(),
                             h6("Earthquakes are always occurring somewhere in the world. Smaller magntiude earthquakes happen every year, while major earthquakes happen, on average, about once a year. As the magntiude increases, frequency of those events decreases."),
                             br(),
                             br(),
                             br(),
                             plotOutput("hist")),
                    tabPanel("Information",
                             tags$iframe(style="height:400px; width:100%; scrolling=yes", src="PlateTectonics.pdf")),
                    tabPanel("Data",
                             tableOutput("table"))
                    )
      )
   )
)

# Define server logic
server <- function(input, output) {
  
  #First tab(Data Explorer)
  output$leaflet <- renderLeaflet({
    
    eq_data %>%
      leaflet() %>%
      addTiles() %>%
      addProviderTiles("Esri.WorldImagery") %>% 
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
  
  #Second tab(Animated Map)
  output$anim_map <- renderImage({

    filter_year <-eq_data %>%
      filter(Year >= input$yearInput[1],
             Year <= input$yearInput[2],
             Magnitude >= input$magInput[1],
             Magnitude <= input$magInput[2],
             Magnitude_Type %in% input$magtypeInput,
             Depth >= input$depthInput[1],
             Depth <= input$depthInput[2],
             Type %in% input$typeInput)

    outfile <- tempfile(fileext='.gif')

    if (input$colorbyInput=="Depth"){
      p <- ggplot()+
        geom_map(data=world, map=world,aes(x=long, y=lat, map_id=region), colour="black", fill="gray50",alpha=0.5)+
        geom_point(data=filter_year,aes(x = Longitude, y = Latitude, frame=Year, color=Depth, size=Depth, alpha=0.1))+
        scale_colour_gradient2(midpoint=mean(filter_year$Depth), low="black", mid="blue",high="red")+
        labs(title="Earthquakes by Depth (km)",x="Latitude (degrees)", y="Longitude (degrees)")+
        theme_classic()
      gganimate(p,interval = 0.3,ani.width=900,ani.res = 1500,"outfile.gif")
    }
    else
      p <- ggplot()+
        geom_map(data=world, map=world,aes(x=long, y=lat, map_id=region), colour="black", fill="gray50",alpha=0.5)+
        geom_point(data=filter_year,aes(x = Longitude, y = Latitude, frame=Year, color=Magnitude, size=Magnitude, alpha=0.1))+
        scale_colour_gradient2(midpoint=mean(filter_year$Magnitude), low="black", mid="purple",high="gold")+
        labs(title="Earthquakes by Magnitude",x="Latitude (degrees)", y="Longitude (degrees)")+
        theme_classic()
      gganimate(p,interval = 0.1,ani.width=750,ani.res = 1500,"outfile.gif")

      # Return a list containing the filename
      list(src = "outfile.gif",
           contentType = 'image/gif'
           #width = 750,
           #height = 450,
           #alt = "This is alternate text"
      )}, deleteFile = TRUE)

  #Third tab(Static Map)
  output$stat_map <- renderPlot({
    
    filter_year <-eq_data %>%
      filter(Year >= input$yearInput[1],
             Year <= input$yearInput[2],
             Magnitude >= input$magInput[1],
             Magnitude <= input$magInput[2],
             Magnitude_Type %in% input$magtypeInput,
             Depth >= input$depthInput[1],
             Depth <= input$depthInput[2],
             Type %in% input$typeInput)
    
    if (input$colorbyInput=="Depth"){
      ggplot()+
        geom_map(data=world, map=world,aes(x=long, y=lat, map_id=region), colour="black", fill="gray50", alpha=0.5)+
        geom_point(data=filter_year,aes(x = Longitude, y = Latitude, color=Depth, size=Depth))+
        scale_colour_gradient2(midpoint=mean(filter_year$Depth), low="black", mid="blue",high="red")+
        labs(title="Earthquakes by Depth (km)",x="Latitude (degrees)", y="Longitude (degrees)")+
        theme_classic()
      
    }
    else
      ggplot()+
        geom_map(data=world, map=world,aes(x=long, y=lat, map_id=region), colour="black", fill="gray50", alpha=0.5)+
        geom_point(data=filter_year,aes(x = Longitude, y = Latitude,color=Magnitude, size=Magnitude))+
        scale_colour_gradient2(midpoint=mean(filter_year$Magnitude), low="black", mid="purple",high="gold")+
        labs(title="Earthquakes by Magnitude",x="Latitude (degrees)", y="Longitude (degrees)")+
        theme_classic()
    
  })
  
  #Fourth tab(Distribution)
  output$hist <- renderPlot({
    
    filter_year <-eq_data %>%
      filter(Year >= input$yearInput[1],
             Year <= input$yearInput[2],
             Magnitude >= input$magInput[1],
             Magnitude <= input$magInput[2],
             Magnitude_Type %in% input$magtypeInput,
             Depth >= input$depthInput[1],
             Depth <= input$depthInput[2],
             Type %in% input$typeInput)
    
      ggplot(filter_year, aes(Magnitude))+
        geom_histogram(bins=50, binwidth=0.1,fill="grey50", color="purple", alpha=0.5)+
        labs(title="Frequency of Earthquakes by Magnitude")+
        theme_classic()
      
  })
    
 #Fifth tab(Information)
 #This has already been included in the UI.
  
 #Sixth tab(Rendered Data)
 output$table <- renderTable({
  filter_year <-eq_data %>%
    filter(Year >= input$yearInput[1],
           Year <= input$yearInput[2],
           Magnitude >= input$magInput[1],
           Magnitude <= input$magInput[2],
           Magnitude_Type %in% input$magtypeInput,
           Depth >= input$depthInput[1],
           Depth <= input$depthInput[2],
           Type %in% input$typeInput)
    
  print(filter_year)    
 })
}

# Run the application 
shinyApp(ui = ui, server = server)

