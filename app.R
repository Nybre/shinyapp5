library(geojsonio)
library(leaflet)
library(shiny)
library(shinyWidgets)
library(sp) 

states <-
    geojsonio::geojson_read("json/gz_2010_us_040_00_20m.json", what = "sp")

#ideally this must not sit on the global variable
table <- read.csv("StateMortalityData.csv", header = TRUE)
df <- na.omit(as.data.frame(table))
df <-
    df[!(df$Gender == "Overall" | df$Race_Ethnicity == "Overall"),]
df <-
    df[!(
        df$LocationDesc == "American Samoa" |
            df$LocationDesc == "Guam" |
            df$LocationDesc == "Northern Mariana Islands" |
            df$LocationDesc == "Virgin Islands of the U.S."
    ),]
#subset by both male and female and both black and white to check the state of cali
 
#state_names <- df$LocationDesc
sex <- df$Gender
race <- df$Race_Ethnicity
 

# Define UI for application
ui <- fluidPage(titlePanel("Project Step 4"),
                
                sidebarLayout(
                    sidebarPanel(
                        checkboxGroupInput(
                            "sexInput",
                            "Display values by sex",
                            #ideally your choices here must be string with a selected default, 
                            #since the file reading can be made reactive on the server side
                            #at the moment your reading it as a global variable which might not be ideal
                            #same will apply for race
                            choices = unique(sex),
                            #choices = c("male","female"),
                            selected = unique(sex)
                            #selected = "male"
                        ), 
                        checkboxGroupInput(
                            "raceInput",
                            "Display values by race", 
                            choices = unique(race), 
                            selected = unique(race)
                        ),
                    ),
                    
                    mainPanel(leafletOutput("map")),
                    
                    position = c("right")
                ))
getwd()
# Define server logic
server <- function(input, output) {
    
    #reactive filter (sex)
    gender_filter_output <- reactive({ 
        input$sexInput 
    })
    #reactive filter (race)
    race_filter_output <- reactive({ 
        input$raceInput 
    })
    US_State_data <-reactive({
        #source data
        #file must be in the working directory, otherwise you can allow user to upload
        table <- read.csv("StateMortalityData.csv", header = TRUE)
        
        #table reprocessing
        df <- na.omit(as.data.frame(table))
        df <-
            df[!(df$Gender == "Overall" | df$Race_Ethnicity == "Overall"),]
        df <-
            df[!(
                df$LocationDesc == "American Samoa" |
                    df$LocationDesc == "Guam" |
                    df$LocationDesc == "Northern Mariana Islands" |
                    df$LocationDesc == "Virgin Islands of the U.S."
            ),]
        
        df=subset(df, df$Gender %in% c(gender_filter_output()))
       
        df=subset(df,df$Race_Ethnicity %in% c(race_filter_output()) ) 
       
        
        data_values = data.frame(df$LocationDesc,df$Data_Value)
        colnames(data_values) <- c("LocationDesc","Data_Value")  
        data_values<-reshape2::melt(data_values,id=c("LocationDesc"))
        data_values_df<-reshape2::dcast(data_values,LocationDesc~variable,sum)    
        
        data_values <- data_values_df$Data_Value
        names(data_values) <- data_values_df$LocationDesc
      
        
        US_States <- states[states$NAME %in% unique(data_values_df$LocationDesc),]
        US_States$DataValue <- data_values[US_States$NAME]
        
        return(US_States)
    })
    
    labels_data<-reactive({
        labels <- sprintf(
            "<strong>%s</strong><br/>%g deaths per 100,000 population</sup>",
            US_State_data()$NAME,
            US_State_data()$DataValue
        ) %>% lapply(htmltools::HTML)  
        return(labels)
    }) 
    
    pal_data <-reactive({
        bins <- c(50, 100, 200, 300, 400, 500, 1000)
        pal <-
            colorBin("YlOrRd", domain = US_State_data()$DataValue, bins = bins) 
        return(pal)
    }) 
    output$map <- renderLeaflet({ 
        leaflet(US_State_data()) %>%
            setView(-96, 37.8, 4) %>% addPolygons(
                fillColor = ~ pal_data()(DataValue),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE
                ),
                label = labels_data(),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                )
            ) %>%
            addLegend(
                pal = pal_data(),
                values = ~ DataValue,
                opacity = 0.7,
                title = NULL,
                position = "bottomright"
            )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
