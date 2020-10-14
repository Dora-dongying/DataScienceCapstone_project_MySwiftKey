suppressWarnings(library(shiny))
shinyUI(navbarPage("Capstone: Course Project",
                   tabPanel("Predict the Next Word",
                            HTML("<strong>Author: Dongying Wang</strong>"),
                            br(),
                            HTML("<strong>Date: 13/10/2020</strong>"),
                            br(),
                            # Sidebar
                            sidebarLayout(
                                    sidebarPanel(
                                            helpText("Enter something to begin the next word prediction."),
                                            helpText("Make sure you have at least 2 words entered before submitting."),
                                            textInput("inputString", "Enter your sentence here",value = ""),
                                            submitButton('Submit'),
                                            br()
                                    ),
                                    mainPanel(
                                            h2("Predicted Next Word"),
                                            h5("Here are top 5 options for your typing"),
                                            verbatimTextOutput("prediction"),
                                            strong("Sentence Input:"),
                                            tags$style(type='text/css', '#text1 {background-color: rgba(255,255,0,0.40); color: blue;}'), 
                                            textOutput('text1')
                                    )
                            )
                            
                   )
)
)