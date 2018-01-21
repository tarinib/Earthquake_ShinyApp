
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
ui <- fluidPage(theme = shinytheme("slate"),
   
   # Application title
   titlePanel("Significant Earthquakes 1965-2016"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        h4("Interactive options"),
        h6("(To be used with animated and interactive map)"),
        sliderInput("yearInput", "Year", 
                    min = 1967, max = 2016,
                    value = c(1975, 2005), sep = "",step=1),
        sliderInput("magInput", "Magnitude", 
                    min = 5.5, max = 9.1,
                    value = c(5.5, 9.1), sep = "",step=0.1),
        checkboxGroupInput("magtypeInput", "Magnitude Type",
                           choices=c("ML","MS","MB","MW","MD"),
                           selected="ML"),
        radioButtons("colorbyInput", "Color By",
                     choices=c("Depth","Magnitude"),
                     selected="Magnitude"),
        checkboxGroupInput("typeInput", "Type",
                           choices=c("Earthquake","Nuclear Explosion"),
                           selected="Earthquake")
        
        #dateRangeInput("dateInput", "Date range:",
                       #start  = "1967-01-01",
                       #end    = "2015-12-31",
                       #min    = "1967-01-01",
                       #max    = "2015-12-31",
                       #format = "mm/dd/yyyy",
                       #separator = " - ")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type="tab",
                    tabPanel("Data Explorer", leafletOutput("leaflet")),
                    #tabPanel("Animated Map", imageOutput("anim_map")),
                    tabPanel("Static Map", plotOutput("stat_map")),
                    tabPanel("Distribution",plotOutput("hist")),
                    tabPanel("Information",tags$iframe(style="height:400px; width:100%; scrolling=yes", 
                                                       src="PlateTectonics.pdf")),
                    tabPanel("Data",tableOutput("table"))
                    )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # #output$anim_map <- renderImage({
  #   
  #   filter_year <-eq_data %>%
  #     filter(Year >= input$yearInput[1],
  #            Year <= input$yearInput[2],
  #            Magnitude >= input$magInput[1],
  #            Magnitude <= input$magInput[2],
  #            Magnitude_Type %in% input$magtypeInput,
  #            Type %in% input$typeInput)
  #   
  #   outfile <- tempfile(fileext='.gif')
  #   
  #   if (input$colorbyInput=="Depth"){
  #     p <- ggplot()+
  #       geom_map(data=world, map=world,aes(x=long, y=lat, map_id=region), colour="black", fill="gray50")+
  #       geom_point(data=filter_year,aes(x = Longitude, y = Latitude, frame=Year, color=Depth, size=Depth, alpha=0.1))+
  #       scale_colour_gradient(low="yellow", high="red")+
  #       theme_classic()
  #     gganimate(p,interval = 0.3,ani.width=900,ani.res = 1500,"outfile.gif")
  #   }
  #   else
  #     p <- ggplot()+
  #       geom_map(data=world, map=world,aes(x=long, y=lat, map_id=region), colour="black", fill="gray50")+
  #       geom_point(data=filter_year,aes(x = Longitude, y = Latitude, frame=Year, color=Magnitude, size=Magnitude, alpha=0.1))+
  #       scale_colour_gradient(low="gold", high="green")+
  #       theme_classic()
  #     gganimate(p,interval = 0.1,ani.width=750,ani.res = 1500,"outfile.gif")
  #     
  #     # Return a list containing the filename
  #     list(src = "outfile.gif",
  #          contentType = 'image/gif'
  #          #width = 750,
  #          #height = 450,
  #          #alt = "This is alternate text"
  #     )}, deleteFile = TRUE)

  output$stat_map <- renderPlot({
    
    filter_year <-eq_data %>%
      filter(Year >= input$yearInput[1],
             Year <= input$yearInput[2],
             Magnitude >= input$magInput[1],
             Magnitude <= input$magInput[2],
             Magnitude_Type %in% input$magtypeInput,
             Type %in% input$typeInput)
    
    if (input$colorbyInput=="Depth"){
      ggplot()+
        geom_map(data=world, map=world,aes(x=long, y=lat, map_id=region), colour="black", fill="gray50")+
        geom_point(data=filter_year,aes(x = Longitude, y = Latitude, color=Depth, size=Depth, alpha=0.1))+
        scale_colour_gradient(low="yellow", high="red")+
        theme_classic()
    }
    else
      ggplot()+
        geom_map(data=world, map=world,aes(x=long, y=lat, map_id=region), colour="black", fill="gray50")+
        geom_point(data=filter_year,aes(x = Longitude, y = Latitude,color=Magnitude, size=Magnitude, alpha=0.1))+
        scale_colour_gradient(low="gold", high="green")+
        theme_classic()
  })

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
  
  output$hist <- renderPlot({
    
    filter_year <-eq_data %>%
      filter(Year >= input$yearInput[1],
             Year <= input$yearInput[2],
             Magnitude >= input$magInput[1],
             Magnitude <= input$magInput[2],
             Magnitude_Type %in% input$magtypeInput,
             Type %in% input$typeInput)
    
      ggplot(filter_year, aes(Magnitude))+
        geom_density(fill="deepskyblue4", color="cyan", alpha=0.5)+
        theme_classic()
      
  })
    
 
 output$table <- renderTable({
  filter_year <-eq_data %>%
    filter(Year >= input$yearInput[1],
           Year <= input$yearInput[2],
           Magnitude >= input$magInput[1],
           Magnitude <= input$magInput[2],
           Magnitude_Type %in% input$magtypeInput,
           Type %in% input$typeInput)
    
  print(filter_year)    
 })
}

# Run the application 
shinyApp(ui = ui, server = server)

