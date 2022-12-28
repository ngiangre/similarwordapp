#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load Libraries ----------------------------------------------------------

library(shiny)
library(rhymer)

# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(

    fluidRow(
        column(
            width=12,
            align="center",
            tags$h1("Give me something similar")
            )
    ),
    fluidRow(
        column(
            width=12,
            align="center",
            textInput(
                inputId = "my_word",
                label = "Enter Word or Phrase:"
                ),
            actionButton("goButton",label = "Get Similar Word or Phrase"),
            uiOutput("similar_word")
        )
    )
)


# Server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output) {

    my_phrase <- eventReactive(input$goButton,{

        paste0(strsplit(input$my_word," ")[[1]],collapse = "+")
    })

    api_content <- reactive({

        req(my_phrase())

        df <- rhymer::datamuse_api(
            paste0("/words?ml=",my_phrase()),
            limit = 500)[['content']]
        df[['score']] <-
            paste0(signif(
                (df[['score']] - min(df[['score']])) /
                    ( max(df[['score']]) - min(df[['score']]) )*100,
                digits = 4),"%")
        df[,c("word","score")]


    })

    output$similar_word <- renderUI({

        req(api_content())

        tags$div({
            DT::datatable( api_content(),
                           colnames = c("Word or Phrase",
                                        "Score"),
                           options = list(
                               regex = TRUE,
                               caseInsensitive = FALSE,
                               searchHighlight = TRUE
                           ))
        })
    })
}


# Run App -----------------------------------------------------------------


shinyApp(ui = ui, server = server)
