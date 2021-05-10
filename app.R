library(geojsonio)
library(leaflet)
library(shiny)
library(shinyWidgets)
library(sp)
devtools::install_github("ropensci/geojsonio")

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
 #output plot
    output$map <- renderLeaflet({
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
        #apply filters to df
        #filter df to either male, female
        #you will need to control the error message if neither gender/race is selected
        #subset main database based on checkbox clicks
        df=subset(df,df$Gender == gender_filter_output() |
                      df$Race_Ethnicity == race_filter_output())
        #preview filter output on R output terminal
        print(gender_filter_output())
        print(race_filter_output())
        
        data_values <- df$Data_Value
        state_names <- df$LocationDesc
        names(data_values) <- state_names
        US_States <- states[states$NAME %in% unique(df$LocationDesc),]
        US_States$DataValue <- data_values[US_States$NAME]
        
        labels <- sprintf(
            "<strong>%s</strong><br/>%g deaths per 100,000 population</sup>",
            US_States$NAME,
            US_States$DataValue
        ) %>% lapply(htmltools::HTML)
        
        bins <- c(50, 100, 200, 300, 400, 500, 1000)
        pal <-
            colorBin("YlOrRd", domain = US_States$DataValue, bins = bins)
        
        leaflet(US_States) %>%
            setView(-96, 37.8, 4) %>% addPolygons(
                fillColor = ~ pal(DataValue),
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
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                )
            ) %>%
            addLegend(
                pal = pal,
                values = ~ DataValue,
                opacity = 0.7,
                title = NULL,
                position = "bottomright"
            )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
