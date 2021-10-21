#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)



# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Input predictor"),
    tags$hr(style="border-color: black;"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            tags$b("Usage example:"),
            helpText("Entered text phrase:"), tags$i("You re the reason why I smile everyday. Can you follow me please? It would mean the"),
            helpText("Predicted:"), tags$i("You re the reason why I smile everyday. Can you follow me please? It would mean the world"),
            ),
        
        

        # Show a plot of the generated distribution
        mainPanel(
            # Input box to enter text phrase without last word
            br(),  
            textAreaInput("Inputtext", label = "Enter text phrase without last word:", value = "It would mean the", width = "100%", rows = 6),
            
           
            hr(),
            
           
            
            radioButtons("rb", "Choose one:",
                         choiceNames = list(
                             "Fast mode (predict with 1% dataset, it takes less than 1 minute)",
                             "Complete mode (It takes about 5-15 minutes)"
                         ),
                         choiceValues = list(
                             "Fast mode", "Complete mode"
                         )),
            
            hr(),
            br(),
            

            submitButton("Submit"),
            
            hr(),
            br(),
            
            tags$b("Predicted next word:"),
            
            # Output box to display predicted word
            fluidRow(column(4, verbatimTextOutput("predict_input", placeholder = TRUE))),
            
            
            hr(),
            br(),
            
            tags$b("Predicted phrase:"),
            
            # Output box to display predicted word
            textOutput("txt")
                     
            
                     
                     
            
           )
        )
    )
)
