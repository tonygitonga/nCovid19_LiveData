#####################################################################################################################
# ---------------------------------- Duck Pawn Analysis  ----------------------------------
## Author: Antony Gitonga
## Date:    20th February 2019       
## Tel:     (254) 717252575
## Emai1:   agitonga01@outlook.com

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(scales)
library(sf)
library(maps)
library(RColorBrewer)
library(forcats)

source(file = "Data_Sourcing.R")

# -------------------------------------- Define UI for application ------------------------------------------------
ui = dashboardPagePlus(header = dashboardHeaderPlus(enable_rightsidebar = FALSE, fixed = FALSE,
                                                    rightSidebarIcon = "gears", title = "Novel CoViD 2019"),
                       skin = "blue", 
                       footer = dashboardFooter(
                         left_text = "By Antony Gitonga",
                         right_text = "Nairobi, 2020"
                       ),
                       #--------- Define the sidebar --------------------------------
                       sidebar = dashboardSidebar(
                         sidebarMenu(
                           id = "tabs",
                           menuItem("Flattening the Curve", tabName = "country", icon = icon("line-chart")),
                           menuItem("Global Data", tabName = "global", icon = icon("globe")),
                           menuItem("Country Statistics", tabName = "demographic", icon = icon("american-sign-language-interpreting"))
                         )
                       ),
                       #---------- Defining the body area ---------------------------------
                       body = dashboardBody(
                         tabItems(
                           tabItem(tabName = "global",
                                   fluidRow(
                                     box(width = 12, title = "Global Novel Coronavirus Pneumonia (NCP-2019)", status = "warning",
                                         selectInput(
                                           inputId = "Categories",
                                           label = "Choose a Category:",
                                           choices = c("Infected", "Dead", "Recovered")
                                         ),
                                         sliderInput("Date",
                                                     "Dates:",
                                                     min = as.Date(min(Recovered$Date),"%Y-%m-%d"),
                                                     max = as.Date(max(Recovered$Date),"%Y-%m-%d"),
                                                     value = as.Date(max(Recovered$Date)),timeFormat = "%Y-%m-%d"
                                         )
                                     ),
                                     box(width = 12, title = "Global Numbers", status = "warning",
                                         highchartOutput(outputId =  "global_numbers", width = "100%", height = "500px")
                                     ),
                                     mainPanel(
                                     )
                                   )
                           ),
                           tabItem(tabName = "country",
                                   fluidRow(
                                     box(width = 12, title = "Current Covid 19 Statistics", status = "warning",
                                         valueBoxOutput("Infections"),
                                         valueBoxOutput("Deaths"),
                                         valueBoxOutput("Recoveries")
                                     ),
                                     box(width = 6, title = "Countries Novel Coronavirus Pneumonia (NCP-2019)", status = "warning",
                                         selectInput(
                                           inputId = "dataset",
                                           label = "Select a Dataset:",
                                           choices = c("Infected", "Dead", "Recovered")
                                         ),
                                         multiInput(
                                           inputId = "countries",
                                           label = "Select a Country:",
                                           choices = "",
                                           selected = "Kenya", width = "400px",
                                           options = list(
                                             enable_search = FALSE,
                                             non_selected_header = "Choose between:",
                                             selected_header = "You have selected:"
                                           )
                                         ),
                                         verbatimTextOutput(outputId = "res")
                                     ),
                                     box(width = 6, title = "Plot of Cumulative Observations", status = "warning",
                                         highchartOutput(outputId =  "country_curve", width = "100%", height = "500px")
                                     ),
                                     mainPanel(
                                     )
                                   )
                           ),
                           tabItem(tabName = "demographic",
                                   fluidRow(
                                     box(width = 12, title = paste0("Current Covid 19 numbers for selected country"), status = "warning",
                                         valueBoxOutput("Infections_c"),
                                         valueBoxOutput("Deaths_c"),
                                         valueBoxOutput("Recoveries_c")
                                     ),
                                     box(width = 6, title = "Countries Novel Coronavirus Pneumonia (NCP-2019)", status = "warning",
                                         selectInput(
                                           inputId = "category",
                                           label = "Select a Category:",
                                           choices = ""
                                         ),
                                         selectInput(
                                           inputId = "country",
                                           label = "Select a Country:",
                                           choices = ""
                                         )
                                     ),
                                     box(width = 6, title = "The Country Plot by Category", status = "warning",
                                         plotOutput(outputId = "country_plot", width = "100%", height = "500px")
                                     ),
                                     mainPanel(
                                     )
                                   )
                           )
                         )
                       )
)

server <- function(input, output, session){
  
  #--------------- Render the high chart for global daa ------------------------------------------------------
  output$global_numbers <- renderHighchart({
    hcmap("custom/world", showInLegend = FALSE, borderColor = "#FAFAFA", borderWidth = 0.1) %>% 
      hc_add_series(data = allData_pt[which(allData_pt$type == input$Categories & allData_pt$date == input$Date),][, c("name", "lat", "lon", "z")], type = "mapbubble", value = "z", name = "Covid 19 Gloaol Statistics", maxSize = '20%') %>% 
      hc_mapNavigation(enabled = TRUE)
    
  })
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Infected" = Infected,
           "Dead" = Dead,
           "Recovered" = Recovered)
  })
  
  #--------------- Render the high chart for country daa ------------------------------------------------------
  outVar = reactive({
    dataSet <- datasetInput()
    sort(unique(dataSet$Country))
  })
  
  observe({
    updateSelectInput(session, "countries", choices = outVar())
    # updateSelectInput(session, "country", choices = outVar_1())
    })

  output$res <- renderPrint({
    input$countries
  })
  # ------------------- The curve we need to flatten
  output$country_curve <- renderHighchart({
    dataSet <- datasetInput()
    country_data <- dataSet[which(dataSet$Country %in% input$countries),][,c("Date", "Country", "Count")]
    colnames(country_data) <- c("date", "country", "infections")
    country_data = plyr::ddply(country_data, ~date + country, function(x){x[which.max(x$infections),]})
    country_data <- tibble::as_tibble(country_data)
    
    country_data %>% 
      hchart(., 
             type = "line", 
             hcaes(x = date, 
                   y = infections,
                   group = forcats::fct_explicit_na(country))
      )
  })
  
  # ------------------ These are the infected people in the world
  output$Infections <- renderValueBox({
    shinydashboard::valueBox(
      paste0(format(as.numeric(tabConfirmed), nsmall = 0, big.mark = ",")), "Number of Infections", color = "aqua", icon = icon("users")
    )
  })
  
  # ------------------ These are the number of deaths
  output$Deaths <- renderValueBox({
    shinydashboard::valueBox(
      paste0(format(as.numeric(tabDead), nsmall = 0, big.mark = ",")), "Number of Deaths", icon = icon("cloud-upload"), color = "red"
    )
  })
  
  # ------------------ These are the number of recoveries
  output$Recoveries <- renderValueBox({
    shinydashboard::valueBox(
      paste0(format(as.numeric(tabRecovered), nsmall = 0, big.mark = ",")), "Number of Recoveries", icon = icon("download"), color = "teal"
    )
  })
  
  
  outVar_1 = reactive({
    sort(unique(county_data$region))
  })
  
  outVar_2 = reactive({
    c("Infected", "Dead", "Recovered")
  })
  
  observe({
    updateSelectInput(session, "country", choices = outVar_1())
    updateSelectInput(session, "category", choices = outVar_2())
  })
  
  # output$dynamicCountry <- renderUI({
  #   selectInput(inputId = "country", label = "Choose a Country:", choices = outVar_1())
  # })
  
  # ------------------ These are the infected people in the world
  output$Infections_c <- renderValueBox({
    tabConfirmed_c <- sum(Infected[which(Infected$Date == max(Infected$Date) & Infected$Country == input$country),]$Count)
    shinydashboard::valueBox(
      paste0(format(as.numeric(tabConfirmed_c), nsmall = 0, big.mark = ",")), "Number of Infections", color = "aqua", icon = icon("users")
    )
  })
  
  # ------------------ These are the number of deaths
  output$Deaths_c <- renderValueBox({
    tabDead_c <- sum(Dead[which(Dead$Date == max(Dead$Date) & Dead$Country == input$country),]$Count)
    shinydashboard::valueBox(
      paste0(format(as.numeric(tabDead_c), nsmall = 0, big.mark = ",")), "Number of Deaths", icon = icon("cloud-upload"), color = "red"
    )
  })
  
  # ------------------ These are the number of recoveries
  output$Recoveries_c <- renderValueBox({
    tabRecovered_c <- sum(Recovered[which(Recovered$Date == max(Recovered$Date) & Recovered$Country == input$country),]$Count)
    shinydashboard::valueBox(
      paste0(format(as.numeric(tabRecovered_c), nsmall = 0, big.mark = ",")), "Number of Recoveries", icon = icon("download"), color = "teal"
    )
  })
  
  # ------------------ Country Plot for choosen category 
  output$country_plot <- renderPlot({
    plot_data <- county_data[which(county_data$region == input$country & county_data$Date == max(county_data$Date) & county_data$Type == input$category),]
    
    ggplot(plot_data, aes(long, lat, group = group)) + geom_polygon(aes(fill = Count), color = "white") +
      scale_fill_viridis_c(option = "C", name = "Number of Cases") + 
      labs(title =  paste0("These are the ", input$category, " in ", input$country), caption  = paste0("Last updated on: ", max(plot_data$Date))) +
      theme_minimal()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

  
  