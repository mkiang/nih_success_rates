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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Submissions and success rates over time"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            award_selectize(name_x = "k_awards", label_x = "Award Mechanism\n(four max):"),
            institute_selector(name_x = "fig1ic", label_x = "Institute/Center:")
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
            )
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
                              k_types = input$k_awards,
                              ics = input$fig1ic)
    }, cacheKeyExpr = list(input$k_awards, input$fig1ic))
    
    output$k_circles <- renderCachedPlot({
        plot_circles(k_df,
                     k_type_x = input$k_award,
                     highlight_institute = input$fig2ic)
    }, cacheKeyExpr = list(input$k_award, input$fig2ic))
}

# Run the application 
shinyApp(ui = ui, server = server)
