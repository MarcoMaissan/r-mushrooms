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

##### Marco Maissan
##### 0949830
##### Hogeschool Rotterdam

mushrooms <- read.csv("C:/Users/marco/Documents/School/Minor/R/mushrooms.csv")
original <- mushrooms


##################################
############Translation###########
##################################
ds_classes <- list("e" = "edible", "p" = "poisonous") 
ds_cap_shape <- list( "b" = "bell","c" = "conical","x" = "convex","f" = "flat", "k" = "knobbed","s" = "sunken")
ds_cap_surface <- list("f" = "fibrous","g" = "grooves", "y" = "scaly","s" = "smooth") 
ds_cap_color <- list("n" = "brown","y" = "yellow", "b" = "buff","c" = "cinnamon","g" = "gray","r" = "green","p" = "pink","u" = "purple","e" = "red","w" = "white","u" = "yellow")
ds_bruises <- list("t" = "bruises", "f" = "no")
ds_odor <- list("a" = "almond", "l" = "anise", "c" = "creosote", "y" = "fishy", "f" = "foul", "m" = "musty", "n" = "none", "p" = "pungent", "s" = "spicy")
ds_gill_attachment <- list ("a" = "attached", "d" = "descending", "f" = "free", "n" = "notched")
ds_gill_spacing <- list("c" = "close", "w" = "crowded", "d" = "distant")
ds_gill_size <- list("b" = "broad", "n" = "narrow")
ds_gill_color <- list("k" = "black", "n" = "brown", "b" = "buff", "h" = "chocolate", "g" = "gray", "r" = "green", "o" = "orange", "p" = "pink", "u" = "purple", "e" = "red", "w" = "white", "y" = "yellow")
ds_stalk_shape <- list("e" = "enlarging", "t" = "tapering")
ds_stalk_root <- list("b" = "bulbous", "c" = "club", "u" = "cup", "e" = "equal", "z" = "rhizomorphs", "r" = "rooted", "?" = "missing")
ds_stalk_surface_above_ring <- list("f" = "fibrous", "y" = "scaly", "k" = "silky", "s" = "smooth")
ds_stalk_surface_below_ring <- list("f" = "fibrous", "y" = "scaly", "k" = "silky", "s" = "smooth")
ds_stalk_color_above_ring <- list("n" = "brown", "b" = "buff", "c" = "cinnamon", "g" = "gray", "o" = "orange", "p" = "pink", "e" = "red", "w" = "white", "y" = "yellow")
ds_stalk_color_below_ring <- list("n" = "brown", "b" = "buff", "c" = "cinnamon", "g" = "gray", "o" = "orange", "p" = "pink", "e" = "red", "w" = "white", "y" = "yellow")
ds_veil_type <- list("u" = "universal", "p" = "partial")
ds_veil_color <- list("n" = "brown", "o" = "orange", "w" = "white", "y" = "yellow")
ds_ring_number <- list("n" = "none", "o" = "one", "t" = "two")
ds_ring_type <- list("c" = "cobwebby", "e" = "evanescent", "f" = "flaring", "l" = "large", "n" = "none", "p" = "pendant", "s" = "sheating", "z" = "zone")
ds_spore_print_color <- list("k" = "black", "n" = "brown", "b" = "buff", "h" = "chocolate", "r" = "green", "o" = "orange", "u" = "purple", "w" = "white", "y" = "yellow")
ds_population <- list("a" = "abundant", "c" = "clustered", "n" = "numerous", "s" = "scattered", "v" = "several", "y" = "solitary")
ds_habitat <- list("g" = "grasses", "l" = "leaves", "m" = "meadows", "p" = "paths", "u" = "urban", "w" = "waste", "d" = "woods")

l <- list("class" = ds_classes, "cap.shape" = ds_cap_shape, "cap.surface" = ds_cap_surface,
          "cap.color" = ds_cap_color,
     "bruises" = ds_bruises, "odor" = ds_odor, "gill.attachment" = ds_gill_attachment, "gill.spacing" = ds_gill_spacing,
     "gill.size" = ds_gill_size, "gill.color" = ds_gill_color, "stalk.shape" = ds_stalk_shape, "stalk.root" = ds_stalk_root, 
     "stalk.surface.above.ring" = ds_stalk_surface_above_ring, "stalk.surface.below.ring" = ds_stalk_surface_below_ring, 
     "stalk.color.above.ring" = ds_stalk_color_above_ring, "stalk.color.below.ring" = ds_stalk_color_below_ring,
     "veil.type" = ds_veil_type, "veil.color" = ds_veil_color, "ring.number" = ds_ring_number, 
     "ring.type" = ds_ring_type, "spore.print.color" = ds_spore_print_color, "population" = ds_population, 
     "habitat" = ds_habitat)

listnames <- names(l)

#loop through each column of the dataframe, replace all values within that column with the translated value
mushrooms <- as.data.frame(sapply(listnames, function(item){
    mushrooms[, item] <- revalue(x = mushrooms[,item], replace = unlist(l[[item]]))
}))

#generate GUI
ui <- navbarPage("Mushrooms: edible or poisonous?",
                 tabPanel("Safe to eat, or deadly poisonous?",
                    sidebarPanel(
                        h2("Choose mushroom properties"),
                        fluidRow(
                            column(12,
                                   actionButton("resetAll", "Reset all"),
                                 )
                            
                        ),
                        fluidRow(
                            #Programmatically generate all inputs
                            useShinyjs(),
                            id="form",
                            column(6,
                                   lapply(colnames(mushrooms)[-1][1:11], function(id) {
                                       selectInput(id, gsub("\\.", " ", id), choices = c("Subset dataset" = "", unique(mushrooms[,id])), width="80%")
                                   }),
                                   ),
                            column(6,
                                   lapply(colnames(mushrooms)[-1][12:22], function(id) {
                                       selectInput(id, gsub("\\.", " ", id), choices = c("Subset dataset" = "", unique(mushrooms[,id])), width="80%")
                                   }),
                                   )
                            
                        ),
                        width = 4
                    ),
                    mainPanel(
                        fluidRow(
                            h1("Chance of mushroom being poisonous or edible"),
                            htmlOutput("error"),
                            p("Source: ", a("Kaggle Mushroom Classification data set", href="https://www.kaggle.com/uciml/mushroom-classification", target="_blank")),
                            htmlOutput("edibilityStatsMain"),
                            plotlyOutput("ratio", height="120px"),
                        ),
                        width=8
                    )
                ),
                tabPanel("Relationship between mushroom properties and edibility",
                         mainPanel(
                             fluidRow(
                                 
                             p("This graph shows the relation between mushroom properties and its chance of being edible or not.
                               The graph changes dynamically based on the selected in puts on the home page."),
                             ),
                             fluidRow(
                                 htmlOutput("edibilityStatsSecond"),
                                 br(),
                                 chorddiagOutput("distPlot", height="1000px")
                             ),
                         width=12
                         )
                )
                
                
                        
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    adjecencyList <- reactiveVal()
    
    generatedataset <- reactive({
        #Read csv
        mushrooms <- original
        
        mushrooms <- as.data.frame(sapply(listnames, function(item){
            mushrooms[, item] <- revalue(x = mushrooms[,item], replace = unlist(l[[item]]))
        }))
        
        colnames <- colnames(mushrooms)[-1]
        
        for (colname in colnames){
            if(input[[colname]] != ""){
                # print(paste(colname, input[[colname]], sep=": "))
                mushrooms <- mushrooms[ mushrooms[[colname]] == input[[colname]], ]
            }
        }
        return(mushrooms)
    })
    
    #Make this subset reactive to prevent excessive computation
    getEdible <- reactive({
        mushrooms <- generatedataset()
        mushrooms[mushrooms$class == 'edible',]
    })
    #Make this subset reactive to prevent excessive computation
    getPoisonous <- reactive({
        mushrooms <- generatedataset()
        mushrooms[mushrooms$class == 'poisonous',]
    })
    
    output$error <- renderText({
        if(nrow(getEdible()) == 0 && nrow(getPoisonous()) == 0){
            "<h2>No mushrooms found!</h2>"
        }else{
            ""
        }
    })

    output$distPlot <- renderChorddiag({
        mushrooms <- generatedataset()
        #all edible and all poisonous mushrooms
        edible <- getEdible()
        poisonous <- getPoisonous()
        if(nrow(edible) > 0 && nrow(poisonous) > 0){
            creatematrix <- function(colname, dataset, class){
                #Group by colname and count individual items
                groupby <- dataset %>% count(colname)
                
                #Get the individual values and make names for them for the chart
                rownames <- groupby[,colname]
                chartnames <- paste(colname, rownames, sep = ".")
                
                #make matrix with chartnames as rownames and count as column name
                #then bind the class as an extra column
                m <- matrix(groupby$freq, dimnames=list(as.list(chartnames), "value"))
                m <- cbind(m, from=class)
                return(m)
            }
            
            #Get all column names, except "class"
            column_names <- colnames(mushrooms)[-1]
            
            #For each column name in the dataset edible, create a matrix 
            #that contains the count of each individual value per column.
            #Bind this to one dataframe.
            if(nrow(edible) > 0){
                a <- sapply(column_names, creatematrix, edible, "edible")
                df_1 <- do.call(rbind, a)
            }else{
                df_1 <- NULL
            }
            
            if(nrow(poisonous) > 0){
                a <- sapply(column_names, creatematrix, poisonous, "poisonous")
                df_2 <- do.call(rbind, a)
            }else{
                df_2 <- NULL
            }
            
            df = rbind(df_1, df_2)
            df = cbind(df, to = rownames(df))
            
            #reset rowsnames to [1:n] and replace rows to ["from", "to", "value"]
            df <- df[, c(2,3,1)]
            rownames(df) <- 1:nrow(df)
            
            #Make dataframe and cast value from string to integer
            df <- as.data.frame(df)
            df$value <- as.integer(df$value)
            
            #clean values
            df$to <- gsub('\\.', " ", df$to)
    
            
            #Make adjecent matrix since chorddiag does not understand adjecent list
            x <- adjacencyList2Matrix(df, square=TRUE)
            adjecencyList(x)
            #convert to data matrix
            x <- data.matrix(x)
            
            #plot diagram
            chorddiag(x,  type="directional", showTicks = F, groupnameFontsize = 14, groupnamePadding = 5, margin=200)
        }
    })
    
    output$ratio <- renderPlotly(
        {
            edible <- nrow(getEdible())
            poisonous <-  nrow(getPoisonous())
            y <- c('')
            data <- data.frame(y, edible, poisonous)
            fig <- plot_ly(data, x = ~edible, y = ~y, type = 'bar', orientation = 'h', name = 'Edible',
                           marker = list(color = 'rgba(56, 188, 98, 0.8)',
                                         line = list(color = 'rgba(0, 255, 0, 1.0)',
                                                     width = 3)) 
                           , showlegend = TRUE)
            fig <- fig %>% add_trace(x = ~poisonous, name = 'Poisonous',
                                     marker = list(color = 'rgba(255, 0, 0, 0.8)',
                                                   line = list(color = 'rgba(255, 0, 0, 1.0)',
                                                               width = 3)))
            fig <- fig %>% layout(barmode = 'stack',
                                  xaxis = list(title = "Amount of mushrooms"),
                                  yaxis = list(title ="")) %>% config(displayModeBar = FALSE)
            
            fig
        }
    )
    
    output$edibilityStatsMain <- renderText({
        edible <- nrow(getEdible())
        poisonous <- nrow(getPoisonous())
        total <- edible + poisonous
        text1 <- paste("<b>Amount of edible mushrooms:</b> ", edible, ", <b>Percentage:</b> ", round((edible/total)*100, digits=2), "<br>")
        text2 <- paste("<b>Amount of poisonous mushrooms:</b> ", poisonous, ", <b>Percentage:</b> ", round((poisonous/total)*100, digits=2))
        paste(text1, text2)    
        })
    
    output$edibilityStatsSecond <- renderText({
        edible <- nrow(getEdible())
        poisonous <- nrow(getPoisonous())
        if(edible > 0 && poisonous > 0){
        total <- edible + poisonous
        text1 <- paste("<b>Amount of edible mushrooms:</b> ", edible, ", <b>Percentage:</b> ", round((edible/total)*100, digits=2), "<br>")
        text2 <- paste("<b>Amount of poisonous mushrooms:</b> ", poisonous, ", <b>Percentage:</b> ", round((poisonous/total)*100, digits=2), "<br>")
        
        # mushies <- getEdible()
        # print(getEdible() %>% count(cap.shape))
        indexes <- which.maxn(adjecencyList()["edible",], 5)
        items <- adjecencyList()["edible",][indexes]
        top_properties <- paste(names(items), collapse = ", ")
        text3 <- paste("<b>Top 5 distinguishing properties of edible mushrooms: </b>", top_properties, "<br>")
        
        indexes <- which.maxn(adjecencyList()["poisonous",], 5)
        items <- adjecencyList()["poisonous",][indexes]
        top_properties <- paste(names(items), collapse = ", ")
        text4 <- paste("<b>top 5 distinguishing properties of poisonous mushrooms: </b>", top_properties, "<br>")
        
        paste(text1, text2, text3, text4)    
        }
        else{
            "<h2>No mushrooms with given property!</h2>"
        }
        })
    
    #reset all inputs
    observeEvent(input$resetAll, {
        print("reset!")
        reset("form")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
