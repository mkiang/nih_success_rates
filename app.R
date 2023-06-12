#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(here)
source(here("code", "utils.R"))
source(here("code", "mk_nytimes.R"))
k_df <- readRDS(here("data", "working_data.RDS"))
first_sub <- read_csv(here("data", "first_submissions_all.csv")) %>%
    select(year = fy,
           activity_code = activity,
           app_first_sub = n_first_sub,
           institute)

first_sub <- bind_rows(
    first_sub,
    first_sub %>%
        group_by(year, activity_code) %>%
        summarize(app_first_sub = sum(app_first_sub)) %>%
        mutate(institute = "All")
)

k_df <- k_df %>%
    left_join(first_sub) %>%
    mutate(success_rate_first = app_first_sub / apps_received * 100)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Submissions and success rates over time"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            award_selectize(name_x = "activity_codes", label_x = "Award Mechanism\n(four max):"),
            institute_selector(name_x = "fig1ic", label_x = "Institute/Center:") #,
            # initial_sub_only(name_x = "first_sub1", label_x = "Plot initial submissions:")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("k_success")
        )
    ),
    
    # Application title
    titlePanel("Award-specific variation across institutes/centers"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            award_selector(name_x = "k_award", label_x = "Award Mechanism:"),
            institute_selector(
                name_x = "fig2ic",
                label_x = "Institute/Center:",
                selected_x = "NIDA"
            ) #,
           # initial_sub_only(name_x = "first_sub2")
        ), 
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("k_circles")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$k_success <- renderCachedPlot({
        plot_apps_and_success(k_df,
                              activity_codes = input$activity_codes,
                              ics = input$fig1ic,
                              first_sub = FALSE) # input$first_sub1)
    }, cacheKeyExpr = list(input$activity_codes, input$fig1ic)) # , input$first_sub1))
    
    output$k_circles <- renderCachedPlot({
        plot_circles(k_df,
                     activity_code_x = input$k_award,
                     highlight_institute = input$fig2ic,
                     first_sub = FALSE) #input$first_sub2)
    }, cacheKeyExpr = list(input$k_award, input$fig2ic)) # , input$first_sub2))
}

# Run the application 
shinyApp(ui = ui, server = server)
