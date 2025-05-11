library(shiny)
library(bslib)
library(luz)
library(dplyr)
library(shinythemes)
library(ggplot2)
library(stringr)
library(torch)

ui <- page_fluid(
  
  theme = bs_theme( # Controls the default grayscale palette
    bg = "#101010",
    fg = "#FFF",
    primary = "#E69F00",
    secondary = "#0072B2",
    success = "#009E73",
    "input-border-color" = "#E69F00",
    base_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono")),
  
  page_sidebar(
    title = "Text Sentiment Prediction Dashboard",
    
    sidebar = sidebar(
      title = "Text Input",
      
      textAreaInput(
        "text", NULL, height = "100%"
      ),
      
      h3("Test Text Inputs:"),
      p("I am not happy"),
      p("I am not sad, I am happy"),
      p("I am excited for the zoo but also nervous because the animals are scary."),
      
      style = "overflow: auto; border: 2px solid #E69F00; height: 100%; border-radius: 15px;"
    ),
    
    
      #card_header("Class Probabilities"),
      plotOutput("pred"),
    style = " margin-top: 0%; border-radius: 15px;"
  )
  

)
