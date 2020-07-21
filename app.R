library(shiny)
library(googleVis)

source("geo_data/state_codes.R")

county_data <- read.csv("data_sets/county_data2.csv")
state_data <- read.csv("data_sets/state_data2.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
        
        # Application title
        titlePanel("SARS-COV-2 in the United States"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
                sidebarPanel(
                        h3("User Inputs"),
                        p("This interactive map can display four different",
                          " indicators of SARS-COV-2's spread within the ",
                          "United States and its sub-regions."),
                        br(),
                        h4("Date Options:"),
                        p("The spread of the virus can be observed by ",
                          "adjusting the date using the slider below."),
                        
                        sliderInput("date",
                                    label = NULL,
                                    min = as.Date("2020-04-15"),
                                    max = as.Date("2020-07-12"),
                                    value = as.Date("2020-04-15"),
                                    timeFormat = "%Y-%m-%d",
                                    animate = TRUE
                        ),
                        br(),
                        h4("Indicator Options:"),
                        p("The four indicators available in the drop-down ",
                          "menu are as follows:"),
                        tags$ol(tags$li("Total Deaths to date"),
                                tags$li("Total Deaths to date per capita"),
                                tags$li("Total Confirmed Cases to date"),
                                tags$li("Total Confirmed Cases to date per ",
                                        "capita")),
                        p("As the name implies, the data shown on the map are ",
                          tags$i("total "),
                          "deaths and ",
                          tags$i("total"),
                          "cases for a given region tallied from the beginning ",
                          "of the outbreak until the date selected by the user."),
                        br(),
                        selectInput("var",
                                    label = NULL,
                                    
                                    choices = c("Total Deaths" = "Deaths",
                                                "Deaths per capita" = 
                                                        "Deaths.per.capita",
                                                "Total Confirmed Cases" = 
                                                        "Confirmed",
                                                "Confirmed Cases per capita" = 
                                                        "Confirmed.per.capita",
                                                "Total Active Cases" = 
                                                        "Active",
                                                "Active Cases per capita" = 
                                                        "Active.per.capita")
                        ),
                        br(),
                        h4("Map Options:"),
                        p("The user may also wish to see a sub-region of the ",
                          "United States.",
                          "Any of the 50 States as well as the ",
                          "Washington-Douglass Capital Region may be selected ",
                          "from the drop-down menu.",
                          "The map will not be updated until the ",
                          tags$i("Change Map"),
                          " action button is clicked."),
                        br(),
                        p("Changing the map to view a sub-region will display ",
                          "data from individual counties and cities indicated by markers scaled to show population size.",
                          "The user may observe the name of the county or city ",
                          "by hovering the mouse over the appropriate marker on the map."),
                        br(),
                        p("Finally, the user may also wish to compare the data ",
                          "in a State to its neighboring States.",
                          "To do so, the user may select the ", 
                          tags$i("Show Neighboring States"),
                          " checkbox and update the map using the ",
                          tags$i("Change Map"),
                          " action button.",
                          "This will display counties and cities of neighboring ",
                          "States that are within the window of the State's map.",
                          "Note that this will also rescale the population and ",
                          "color of counties and cities to the largest values ",
                          "among the adjacent States whether that value is ",
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
                        htmlOutput("view")
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
                                            height = 760,
                                            width = 1200,
                                            keepAspectRatio = TRUE
                             )
                )
        })
        
        
}

# Run the application 
shinyApp(ui = ui, server = server)
