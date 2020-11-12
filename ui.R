library(shiny)
library(chorddiag)
library(tidyverse)
library(circlize)
require(dplyr)
library(plyr)
library(plotly)
library(viridis)
library(doBy)
library(shinyjs)
library(rsconnect)

##### Marco Maissan
##### 0949830
##### Hogeschool Rotterdam

#generate GUI
ui <- navbarPage(
  "Mushrooms: edible or poisonous?",
  tabPanel(
    "Safe to eat, or deadly poisonous?",
    sidebarPanel(
      h2("Choose mushroom properties"),
      fluidRow(column(
        12,
        actionButton("resetAll", "Reset all"),
      )),
      fluidRow(#Programmatically generate all inputs
        useShinyjs(),
        id = "form",
        column(6,
               lapply(colnames(mushrooms)[-1][1:11], function(id) {
                 selectInput(
                   id,
                   gsub("\\.", " ", id),
                   choices = c("Subset dataset" = "", unique(mushrooms[, id])),
                   width = "80%"
                 )
               }),),
        column(6,
               lapply(colnames(mushrooms)[-1][12:22], function(id) {
                 selectInput(
                   id,
                   gsub("\\.", " ", id),
                   choices = c("Subset dataset" = "", unique(mushrooms[, id])),
                   width = "80%"
                 )
               }),)),
      width = 4
    ),
    mainPanel(
      fluidRow(
        h1("Chance of mushroom being poisonous or edible"),
        p(
          "Data Source: ",
          a(
            "Kaggle Mushroom Classification data set",
            href = "https://www.kaggle.com/uciml/mushroom-classification",
            target = "_blank"
          )
        ),
        p(
          "Github repo: ",
          a(
            "MarcoMaissan/r-mushrooms",
            href = "https://github.com/MarcoMaissan/r-mushrooms",
            target = "_blank"
          )
        ),
        hr(),
        htmlOutput("error"),
        htmlOutput("edibilityStatsMain"),
        br(),
        actionButton("setNumberOfMushrooms", "Show in numbers"),
        actionButton("setPercentageOfMushrooms", "Show in percentages"),
        plotlyOutput("ratio", height = "120px"),
      ),
      width = 8
    )
  ),
  tabPanel(
    "Properties of subset",
    sidebarPanel(h2("Selected properties"),
                 fluidRow(column(
                   p(
                     "This diagram is generated based on the input on the home page. Change the properties on the home page to change the diagram."
                   )
                   ,
                   width = 12
                   
                 ),),
                 width = 4),
    mainPanel(fluidRow(
      h1("Chance of mushroom being poisonous or edible"),
      numericInput(
        "amountOfProperties",
        "Max. amount of properties",
        10,
        min = 1,
        max = 100
      ),
      actionButton("ascenddescent", "Ascending / Descending"),
      
      plotlyOutput("properties", height = "600px")
    ),),
    width = 8
  )
  ,
  tabPanel(
    "Relationship between mushroom properties and edibility",
    mainPanel(
      fluidRow(
        h1("Chance of mushroom being poisonous or edible"),
        p(
          "Data Source: ",
          a(
            "Kaggle Mushroom Classification data set",
            href = "https://www.kaggle.com/uciml/mushroom-classification",
            target = "_blank"
          )
        ),
        p(
          "Github repo: ",
          a(
            "MarcoMaissan/r-mushrooms",
            href = "https://github.com/MarcoMaissan/r-mushrooms",
            target = "_blank"
          )
        ),
        hr(),
        h2("How to read this graph"),
        p(
          "This diagram is generated based on the input on the home page. Change the properties on the home page to change the diagram. Are you uncertain about the edibility of a mushroom? This graph shows the top 10 most distinguishing features of both edible and poisonous mushrooms. It easily shows which properties define edibility and which properties are overlapping in both edible and poisonous mushrooms."
        ),
        p("Transposing the diagram changes the orientation of the diagram."),
        hr(),
      ),
      fluidRow(
        htmlOutput("edibilityStatsSecond"),
        br(),
        actionButton("transpose", "Transpose diagram"),
        chorddiagOutput("distPlot", height = "1000px")
      ),
      width = 12
    )
  )
)
