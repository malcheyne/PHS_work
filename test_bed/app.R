

library(janitor)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(shiny)

raw_activity <- read_csv("data/inpatient_and_daycase_by_nhs_board_of_treatment_and_specialty.csv")

activity_no_gj <- raw_activity %>% 
  clean_names() %>% 
  # Create new column for quarters
  mutate(q = str_sub(quarter, -2)) %>% 
  # Change to date format
  mutate(quarter = yq(quarter)) %>% 
  # rename the columns above 
  mutate(date = quarter,
         quarter = q) %>% 
  # Change Heath Board names to read better
  mutate(
    hb = if_else(hb %in% "S08000015", "Ayrshire & Arran",
               if_else(hb %in% "S08000016", "Borders",
               if_else(hb %in% "S08000017", "Dumfries & Galloway",
               if_else(hb %in% "S08000019", "Forth Valley",
               if_else(hb %in% "S08000020", "Grampian",
               if_else(hb %in% "S08000022", "Highland",
               if_else(hb %in% "S08000024", "Lothian",
               if_else(hb %in% "S08000025", "Orkney",
               if_else(hb %in% "S08000026", "Shetland",
               if_else(hb %in% "S08000028", "Western Isles",
               if_else(hb %in% "S08000029", "Fife",
               if_else(hb %in% "S08000030", "Tayside",
               if_else(hb %in% "S08000031", "Greater Glasgow & Clyde",
               if_else(hb %in% "S08000032", "Lanarkshire",
               if_else(hb %in% "S92000003", "Scotland", hb)
               ))))))))))))))) %>% 
  # take out location names to take out duplicate
  filter(location_qf == "d")  

ui <- fluidPage(
  
  titlePanel("Test"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("specialty_input",
                  "Which Specialty?",
                  choices = 
                    unique(activity_no_gj$specialty_name), 
                  selected = "Infectious Diseases"
        
      ),
      
      selectInput("hb_input",
                  "Which Health Board Area?",
                  choices = 
                    unique(activity_no_gj$hb), 
                    selected = "Scotland"
                  
      ),
      
      sliderInput("coivd_date_range", label = "Date Range",
                  min = as.Date("2016-01-01","%Y-%m-%d"),
                  max = as.Date("2021-12-31","%Y-%m-%d"),
                  value = c(as.Date("2016-01-01"),
                            as.Date("2021-12-31")),
                  timeFormat="%Y-%m",
                  step = 91.25, ticks = TRUE
      ),
      
      # ACTION BUTTON
      actionButton("update", "Polt"
      )
    ),
    
    mainPanel(
      plotOutput("specialty_episodes_plot")
    )
  )
)

server <- function(input, output) {
  
  quart <- reactive({
    seq(input$coivd_date_range[1], input$coivd_date_range[2], by = 1)
  })
  
  # ACTION BUTTON
  action_but <- eventReactive(input$update,{
    activity_no_gj %>% 
      filter(date %in% quart(),
             specialty_name %in% input$specialty_input,
             hb %in% input$hb_input,
             admission_type %in% c("Elective Inpatients", 
                                   "Emergency Inpatients", 
                                   "Transfers"))
  })
  
  output$specialty_episodes_plot <- renderPlot({
    action_but() %>% 
      # filter(specialty_name %in% input$specialty_input,
      #        hb %in% input$hb_input,
      #        admission_type %in% c("Elective Inpatients", 
      #                              "Emergency Inpatients", 
      #                              "Transfers")
      #        # ,
      #        # date %in% between(date, input$coivd_date_range[1], 
      #        #                 input$coivd_date_range[2])
      #        ) %>% 
      ggplot(aes(x = date, y = episodes, col = admission_type)) +
      geom_point() +
      geom_line()
  })
}

shinyApp(ui = ui, server = server)


