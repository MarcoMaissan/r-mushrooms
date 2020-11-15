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


mushrooms <- read.csv("./mushrooms.csv")
original <- mushrooms

##################################
############Translation###########
##################################
ds_classes <- list("e" = "edible", "p" = "poisonous")
ds_cap_shape <-
  list(
    "b" = "bell",
    "c" = "conical",
    "x" = "convex",
    "f" = "flat",
    "k" = "knobbed",
    "s" = "sunken"
  )
ds_cap_surface <-
  list(
    "f" = "fibrous",
    "g" = "grooves",
    "y" = "scaly",
    "s" = "smooth"
  )
ds_cap_color <-
  list(
    "n" = "brown",
    "y" = "yellow",
    "b" = "buff",
    "c" = "cinnamon",
    "g" = "gray",
    "r" = "green",
    "p" = "pink",
    "u" = "purple",
    "e" = "red",
    "w" = "white",
    "u" = "yellow"
  )
ds_bruises <- list("t" = "bruises", "f" = "no")
ds_odor <-
  list(
    "a" = "almond",
    "l" = "anise",
    "c" = "creosote",
    "y" = "fishy",
    "f" = "foul",
    "m" = "musty",
    "n" = "none",
    "p" = "pungent",
    "s" = "spicy"
  )
ds_gill_attachment <-
  list (
    "a" = "attached",
    "d" = "descending",
    "f" = "free",
    "n" = "notched"
  )
ds_gill_spacing <-
  list("c" = "close", "w" = "crowded", "d" = "distant")
ds_gill_size <- list("b" = "broad", "n" = "narrow")
ds_gill_color <-
  list(
    "k" = "black",
    "n" = "brown",
    "b" = "buff",
    "h" = "chocolate",
    "g" = "gray",
    "r" = "green",
    "o" = "orange",
    "p" = "pink",
    "u" = "purple",
    "e" = "red",
    "w" = "white",
    "y" = "yellow"
  )
ds_stalk_shape <- list("e" = "enlarging", "t" = "tapering")
ds_stalk_root <-
  list(
    "b" = "bulbous",
    "c" = "club",
    "u" = "cup",
    "e" = "equal",
    "z" = "rhizomorphs",
    "r" = "rooted",
    "?" = "missing"
  )
ds_stalk_surface_above_ring <-
  list(
    "f" = "fibrous",
    "y" = "scaly",
    "k" = "silky",
    "s" = "smooth"
  )
ds_stalk_surface_below_ring <-
  list(
    "f" = "fibrous",
    "y" = "scaly",
    "k" = "silky",
    "s" = "smooth"
  )
ds_stalk_color_above_ring <-
  list(
    "n" = "brown",
    "b" = "buff",
    "c" = "cinnamon",
    "g" = "gray",
    "o" = "orange",
    "p" = "pink",
    "e" = "red",
    "w" = "white",
    "y" = "yellow"
  )
ds_stalk_color_below_ring <-
  list(
    "n" = "brown",
    "b" = "buff",
    "c" = "cinnamon",
    "g" = "gray",
    "o" = "orange",
    "p" = "pink",
    "e" = "red",
    "w" = "white",
    "y" = "yellow"
  )
ds_veil_type <- list("u" = "universal", "p" = "partial")
ds_veil_color <-
  list(
    "n" = "brown",
    "o" = "orange",
    "w" = "white",
    "y" = "yellow"
  )
ds_ring_number <- list("n" = "none", "o" = "one", "t" = "two")
ds_ring_type <-
  list(
    "c" = "cobwebby",
    "e" = "evanescent",
    "f" = "flaring",
    "l" = "large",
    "n" = "none",
    "p" = "pendant",
    "s" = "sheating",
    "z" = "zone"
  )
ds_spore_print_color <-
  list(
    "k" = "black",
    "n" = "brown",
    "b" = "buff",
    "h" = "chocolate",
    "r" = "green",
    "o" = "orange",
    "u" = "purple",
    "w" = "white",
    "y" = "yellow"
  )
ds_population <-
  list(
    "a" = "abundant",
    "c" = "clustered",
    "n" = "numerous",
    "s" = "scattered",
    "v" = "several",
    "y" = "solitary"
  )
ds_habitat <-
  list(
    "g" = "grasses",
    "l" = "leaves",
    "m" = "meadows",
    "p" = "paths",
    "u" = "urban",
    "w" = "waste",
    "d" = "woods"
  )

l <-
  list(
    "class" = ds_classes,
    "cap.shape" = ds_cap_shape,
    "cap.surface" = ds_cap_surface,
    "cap.color" = ds_cap_color,
    "bruises" = ds_bruises,
    "odor" = ds_odor,
    "gill.attachment" = ds_gill_attachment,
    "gill.spacing" = ds_gill_spacing,
    "gill.size" = ds_gill_size,
    "gill.color" = ds_gill_color,
    "stalk.shape" = ds_stalk_shape,
    "stalk.root" = ds_stalk_root,
    "stalk.surface.above.ring" = ds_stalk_surface_above_ring,
    "stalk.surface.below.ring" = ds_stalk_surface_below_ring,
    "stalk.color.above.ring" = ds_stalk_color_above_ring,
    "stalk.color.below.ring" = ds_stalk_color_below_ring,
    "veil.type" = ds_veil_type,
    "veil.color" = ds_veil_color,
    "ring.number" = ds_ring_number,
    "ring.type" = ds_ring_type,
    "spore.print.color" = ds_spore_print_color,
    "population" = ds_population,
    "habitat" = ds_habitat
  )

listnames <- names(l)

#loop through each column of the dataframe, replace all values within that column with the translated value
mushrooms <- as.data.frame(sapply(listnames, function(item) {
  mushrooms[, item] <-
    revalue(x = mushrooms[, item], replace = unlist(l[[item]]))
}))


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
        h1("Safe to eat, or deadly poisonous?"),
        p(a("Click here for the Shinyapps.io version", href="https://marcomaissan.shinyapps.io/assignment_datavisualisation/", target="_blank")),
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
        h2("How to use this website"),
        p("Select mushroom properties in the side panel. All the graphs will change based on the new subset of data."),
        hr(),
        htmlOutput("error"),
        htmlOutput("edibilityStatsMain"),
        br(),
        actionButton("setNumberOfMushrooms", "Show in numbers"),
        actionButton("setPercentageOfMushrooms", "Show in percentages"),
        hr(),
        plotlyOutput("ratio", height = "120px"),
      ),
      width = 8
    )
  ),
  tabPanel(
    "Properties of subset",
    sidebarPanel(h2("Description"),
                 fluidRow(column(
                   p(
                     "This diagram is generated based on the input from the home page. 
                     Change the properties on the home page to change the diagram.
                     This diagram shows the edible/poisonousness ratio of each property."
                   ),
                   hr(),
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
                   width = 12
                   
                 ),),
                 width = 4),
    mainPanel(fluidRow(
      h1("Properties of subset"),
      htmlOutput("error2"),
      numericInput(
        "numberOfProperties",
        "Max. number of properties",
        10,
        min = 1,
        max = 150
      ),
      actionButton("ascenddescent", "Ascending / Descending"),
      
      plotlyOutput("properties", height = "600px")
    ),),
    width = 8
  ),
  
  tabPanel(
    "Poisonous colors",
    sidebarPanel(h2("Description"),
                 fluidRow(column(
                   p(
                     "This diagram is generated based on the input on the home page. 
          Change the properties on the home page to change the diagram. "),
                   p(
                     "This diagrams hows which colors are the deadliest. Be careful around these mushrooms!"
                   ),
                   hr(),
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
                   width = 12
                   
                 ),),
                 width = 4),
    mainPanel(fluidRow(
      h1("Poisonous colors"),
      htmlOutput("error3"),
      
      plotlyOutput("colors", height = "600px")
    ),),
    width = 8
  ),
  
  tabPanel(
    "Relationship between most influencing mushroom properties and edibility",
    mainPanel(
      fluidRow(
        h1("Relationship between most influencing mushroom properties and edibility"),
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
        h2("Description"),
        p(
          "This diagram is generated based on the input on the home page. 
          Change the properties on the home page to change the diagram. "),
        p("Are you uncertain about the edibility of a mushroom? 
          This graph shows the top 10 most distinguishing features of both edible and poisonous mushrooms. 
          It easily shows which properties define edibility and which properties are overlapping in both edible and poisonous mushrooms."
        ),
        p("Transposing the diagram changes the orientation of the diagram."),
        hr(),
      ),
      fluidRow(
        htmlOutput("edibilityStatsSecond"),
        hr(),
        h3("Most influencing edible and poisonous mushroom properties of dataset"),
        actionButton("transpose", "Transpose diagram"),
        chorddiagOutput("distPlot", height = "1000px")
      ),
      width = 12
    )
  )
)
