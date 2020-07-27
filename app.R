library(shiny)
library(googleVis)

source("data/geo_data/state_codes.R")

county_data <- read.csv("data_sets/county_data.csv")
state_data <- read.csv("data_sets/state_data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
        
        # Application title
        titlePanel("Daily Snapshots of SARS-COV-2 in the United States"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
                sidebarPanel(
                        h4("Date Options:"),
                        p("The spread of the virus at a previous date ",
                          "can be observed by ",
                          "adjusting the slider below."),
                        
                        sliderInput("date",
                                    label = NULL,
                                    min = as.Date("2020-04-15"),
                                    max = as.Date("2020-07-25"),
                                    value = as.Date("2020-07-25"),
                                    timeFormat = "%Y-%m-%d",
                                    ticks = FALSE
                        ),
                        br(),
                        h4("Indicator Options:"),
                        p("Four indicators are available in the drop-down ",
                          "menu."),
                        p("As their names imply, the indicators show ",
                          tags$i("total "),
                          "deaths and ",
                          tags$i("total"),
                          "cases for a given region tallied from the beginning ",
                          "of the outbreak until the date selected by the user."
                        ),
                        br(),
                        selectInput("var",
                                    label = NULL,
                                    
                                    choices = c("Total Deaths" = "Deaths",
                                                "Deaths per capita" = 
                                                        "Deaths.per.capita",
                                                "Total Confirmed Cases" = 
                                                        "Confirmed",
                                                "Confirmed Cases per capita" = 
                                                        "Confirmed.per.capita"
                                    )
                        ),
                        br(),
                        h4("Map Options:"),
                        p("To switch the map to a sub-region of the United States ",
                          "select a region from the drop-down menu then click the ",
                          tags$i("Change Map"),
                          " action button."),
                        br(),
                        p("Changing the map to view a sub-region will display ",
                          "data from individual counties and cities indicated by markers scaled to show population size."),
                        br(),
                        p("The ", 
                          tags$i("Show Neighboring States"),
                          " checkbox ",
                          "will display counties and cities of neighboring ",
                          "States that are within the window of the State's map.",
                          "Note that this will also rescale the size and ",
                          "color of the markers to the largest values ",
                          "among all adjacent States whether that value is ",
                          "shown on the map or not."),
                        br(),
                        br(),
                        
                        selectInput("reg",
                                    label = NULL,
                                    
                                    choices = State_Codes),
                        checkboxInput("nbrs",label = "Show Neighboring States"),
                        actionButton("update_reg",label = "Change Map")
                        
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                        htmlOutput("view"),
                        p(
                                "Data from",
                                a("JHU CSSE COVID-19 Data", 
                                  href = "https://github.com/CSSEGISandData/COVID-19"),
                                "and",
                                a("US Census Bureau.",
                                  href = "https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-total.html")
                        ),
                        p(
                                a("More information about this project.",
                                  href = "https://1earning-r.github.io/Dev_Data_Products_Project_COVID/pitch.html")
                        )
                )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        
        sub_data <- reactiveVal(state_data)
        region <- reactiveVal("US")
        resolution <- reactiveVal("provinces")
        locationvar <- reactiveVal("State")
        displayMode <- reactiveVal("regions")
        
        observeEvent(input$update_reg,{
                
                region(input$reg)
                if (region() != 'US'){
                        show_regs <- if (input$nbrs) Neighbors[[region()]] else region()
                        resolution('provinces')
                        locationvar('LatLong')
                        displayMode('markers')
                        sub_data(county_data[county_data$State_Code %in% show_regs,])
                } else {
                        region(input$reg)
                        resolution('provinces')
                        locationvar('Name')
                        displayMode('regions')
                        sub_data(state_data)
                }
        })
        
        output$view <- renderGvis({
                plot_data <- sub_data()
                plot_data <- plot_data[plot_data$Date == as.character(input$date),]
                gvisGeoChart(plot_data,
                             locationvar = locationvar(),
                             colorvar = input$var,
                             sizevar = "Population",
                             hovervar = "Name",
                             options = list(region=region(),
                                            resolution = resolution(),
                                            displayMode = displayMode(),
                                            height = 570,
                                            width = 900,
                                            keepAspectRatio = TRUE
                             )
                )
        })
        
        
}

# Run the application 
shinyApp(ui = ui, server = server)
