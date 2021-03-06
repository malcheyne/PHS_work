## ui.R ##

# creates the sidebar menu -----------------------------------------------------
sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("columns")),
      menuItem("ICU Admissions", tabName = "icu", icon = icon("poll")),
      menuItem("A&E Admissions", tabName = "ae", icon = icon("poll")),
      menuItem("Statistics", tabName = "stats", icon = icon("chart-line")),
      setSliderColor(c("#7CB342", "#7CB342", "#7CB342", "#7CB342"), c(1, 2, 3, 4)),
      sliderInput("date_range", label = "Date Range",
                  min = as.Date("2016-01-01","%Y-%m-%d"),
                  max = as.Date("2021-12-31","%Y-%m-%d"),
                  value = c(as.Date("2016-01-01"),
                            as.Date("2021-12-31")),
                  timeFormat="%Y-%m",
                  width = "85%",
                  step = 90),
      actionButton("applyButton",
                   "Apply Changes")
    )
)

# creates the main body --------------------------------------------------------
body <- dashboardBody(
  tabItems(

# Overview tab ----------------------------------------------------------------
    tabItem(tabName = "overview",
            
            # creates a space between the top bar and the main page
            br(),
            
            # creates the element for the top row
            fluidRow(column(width = 3,
                            br(),
                            radioButtons("icu_choice_1", "ICU choice",
                                         choices = c("Yes", "No"))),
                     column(width = 3,
                            br(),
                            numericInput("age_range_1", "Age",
                                         value = 1, min = 1, max = 100)),
                     column(width = 3,
                            br(),
                            sliderInput("date_range", label = "Date Range",
                                        min = as.Date("2016-01-01","%Y-%m-%d"),
                                        max = as.Date("2021-12-31","%Y-%m-%d"),
                                        value = c(as.Date("2016-01-01"),
                                                  as.Date("2021-12-31")),
                                        timeFormat="%Y-%m",
                                        step = 90
                            )
                     ),
                     column(width = 3,
                            br(),
                            actionButton("applyButton",
                                         "Apply Changes")
                     )
            ),
            
            # creates the element for the main row, first half (this is a placeholder plot)
            fluidRow(column(width = 5,
                            br(),
                            plotOutput("beds_percentage_plot")
            ),
            
            # creates the element for the main row, second half - (this is a placeholder plot)
            column(width = 6,
                   br(),
                   plotOutput("admissions_episodes_plot")
            )
            ),
    ),
            

# COVID tab ----------------------------------------------------------
    tabItem(tabName = "covid",
            
            # creates a space between the top bar and the main page
            br(),
            
            # creates the element for the top row
            fluidRow(column(width = 4,
                            br(),
                            selectInput("hb_input",
                                        "Which Health Board Area?",
                                        choices = 
                                          unique(clean_admissions$hb), 
                                        selected = "Scotland")
                     ),
                     column(width = 4,
                            br(),
                            selectInput("specialty_input",
                                        "Which Specialty?",
                                        choices = 
                                          unique(clean_admissions$specialty_name), 
                                        selected = "Infectious Diseases")
                     ),
                     column(width = 4,
                            br(),
                            sliderInput("coivd_date_range", label = "Date Range",
                                        min = as.Date("2016-01-01","%Y-%m-%d"),
                                        max = as.Date("2021-12-31","%Y-%m-%d"),
                                        value = c(as.Date("2016-01-01"),
                                                  as.Date("2021-12-31")),
                                        timeFormat="%Y-%m",
                                        step = 90,
                                        ticks = FALSE
                            )
                     ),
                     column(width = 4,
                            br(),
                            actionButton("update",
                                         "Plot")
                     )
            ),
            
            # creates the element for the main row, first half (this is a placeholder plot)
            fluidRow(column(width = 5,
                            br(),
                            plotOutput("beds_percentage_plot"),
                            
                            # create the bottom right box with text description
                            textOutput("icu_text_placeholder"),
            ),
            
            # creates the element for the main row, second half - (this is a placeholder plot)
            column(width = 6,
                   br(),
                   plotOutput("admissions_episodes_plot")
            )
            ),
    ),


# A&E tab-----------------------------------------------------------
    tabItem(tabName = "ae",

            # creates a space between the top bar and the main page
            br(),

            # create the element for the left column
            sidebarLayout(

              # creates the sidebar panel with the date, age and dept selection
              sidebarPanel(
                numericInput("age_range_1", "Age",
                             value = 1, min = 1, max = 100),
                
                radioButtons("dept_type", "Department Type",
                             choices = c("Accident and Emergency",
                                        "Minor Injuries")),

                sliderInput("date_range", label = "Date Range",
                            min = as.Date("2016-01-01","%Y-%m-%d"),
                            max = as.Date("2021-12-31","%Y-%m-%d"),
                            value = c(as.Date("2016-01-01"),
                                      as.Date("2021-12-31")),
                            timeFormat="%Y-%m",
                            step = 90
                ),
                actionButton("applyButton",
                             "Apply Changes")
              ),

              # creates the main panel
              mainPanel(

                # create the element for the right column -(this is a placeholder plot)
                fluidRow(column(width = 5,
                                br(),
                                plotOutput("ae_emergency_plot"),
                                
                                # create the bottom right box with text description
                                textOutput("ae_text_placeholder")
                              ),
                         
                         column(width = 5,
                                br(),
                                plotOutput("urology_plot"),
                                
                                # create the bottom right box with text description
                                # textOutput("ae_text_placeholder")
                                )
                          )
                      )
              )

    ),
    
    # Statistics tab -----------------------------------------------------------
    # creates the navigation for statistics tab
    tabItem(tabName = "stats",

         # create the element for the left column
          br(),
         fluidRow(
           column(width = 4,
                  radioButtons("plot_input",
                               "Select plot type",
                               choices = c("Box plot", "Histogram"))
           ),

           # this is a placeholder plot
           column(width = 8,
                  plotOutput("some_plot"))
         ),

         # create the left box with date selection
         fluidRow(
           column(width = 4,
                  radioButtons("variable_input",
                               "Analysis on which variable?",
                               choices = c("ICU", "A&E"))
           ),

           # this is a placeholder plot
           column(width = 8,
                  plotOutput("null_plot"))

         ),

         fluidRow(  # create the bottom right box with text description
           column(width = 8,
                  textOutput("stat_text")))
        )
  )
)

# Main dashboard and CSS -------------------------------------------------------
dashboardPage(skin = "purple",
    dashboardHeader(title = "PHS Project"),
    sidebar,
    body,
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  )
)