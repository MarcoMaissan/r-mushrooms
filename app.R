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
                                  p("Data Source: ", a("Kaggle Mushroom Classification data set", href="https://www.kaggle.com/uciml/mushroom-classification", target="_blank")),
                                  p("Github repo: ", a("MarcoMaissan/r-mushrooms", href="https://github.com/MarcoMaissan/r-mushrooms", target="_blank")),
                                  hr(),
                                  htmlOutput("error"),
                                  htmlOutput("edibilityStatsMain"),
                                  br(),
                                  actionButton("setNumberOfMushrooms", "Show in numbers"),
                                  actionButton("setPercentageOfMushrooms", "Show in percentages"),
                                  plotlyOutput("ratio", height="120px"),
                              ),
                              width=8
                          )
                 ),
                 tabPanel("Properties of subset",
                          sidebarPanel(
                              h2("Selected properties"),
                              fluidRow(
                                  column(
                                      p("This diagram is generated based on the input on the home page. Change the properties on the home page to change the diagram.")
                                      ,width=12
                                      
                                  ), 
                              ),
                              width = 4
                          ),
                          mainPanel(
                              fluidRow(
                                  h1("Chance of mushroom being poisonous or edible"),
                                  numericInput("amountOfProperties", "Max. amount of properties", 10, min = 1, max = 100),
                                  actionButton("ascenddescent", "Ascending / Descending"),
                                  
                                  plotlyOutput("properties", height="600px") 
                              ),
                          ),
                          width=8
                 )
                 ,
                 tabPanel("Relationship between mushroom properties and edibility",
                          mainPanel(
                              fluidRow(
                                  h1("Chance of mushroom being poisonous or edible"),
                                  p("Data Source: ", a("Kaggle Mushroom Classification data set", href="https://www.kaggle.com/uciml/mushroom-classification", target="_blank")),
                                  p("Github repo: ", a("MarcoMaissan/r-mushrooms", href="https://github.com/MarcoMaissan/r-mushrooms", target="_blank")),
                                  hr(),
                                  h2("How to read this graph"),
                                  p("This diagram is generated based on the input on the home page. Change the properties on the home page to change the diagram. Are you uncertain about the edibility of a mushroom? This graph shows the top 10 most distinguishing features of both edible and poisonous mushrooms. It easily shows which properties define edibility and which properties are overlapping in both edible and poisonous mushrooms."),
                                  p("Transposing the diagram changes the orientation of the diagram."),
                                  hr(),
                              ),
                              fluidRow(
                                  htmlOutput("edibilityStatsSecond"),
                                  br(),
                                  actionButton("transpose", "Transpose diagram"),
                                  chorddiagOutput("distPlot", height="1000px")
                              ),
                              width=12
                          )
                 )
                 
                 
                 
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    adjecencyList <- reactiveVal()
    ascending <- reactiveVal(F)
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
    
    transposed <- reactiveVal(FALSE)
    
    output$distPlot <- renderChorddiag({
        mushrooms <- generatedataset()
        #all edible and all poisonous mushrooms
        edible <- getEdible()
        poisonous <- getPoisonous()
        if(nrow(edible) > 0 || nrow(poisonous) > 0){
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
                
                
                df_1 <- head(df_1[order(-as.numeric(df_1[,"value"])),], n=10)
            }else{
                df_1 <- NULL
            }
            
            if(nrow(poisonous) > 0){
                a <- sapply(column_names, creatematrix, poisonous, "poisonous")
                df_2 <- do.call(rbind, a)
                df_2 <- head(df_2[order(-as.numeric(df_2[,"value"])),], n=10)
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
            if(transposed()){
                x <- data.matrix(x) 
            }else{
                x <- data.matrix(x) %>% t
            }
            #plot diagram
            chorddiag(x,  type="directional", showTicks = F, groupnameFontsize = 14, groupnamePadding = 5, margin=200)
        }
    })
    setPlot <- reactiveVal(F)
    
    output$ratio <- renderPlotly({
        type <- setPlot()
        edible <- nrow(getEdible())
        poisonous <-  nrow(getPoisonous())
        y <- c('')
        if(type == T){
            total <- edible+poisonous
            if(total > 0){
                edible <- (edible/total)*100
                poisonous <- (poisonous/total)*100
            }else{
                edible <- poisonous <- 0
            }
            title <- "Percentage" 
        }else{
            title <- "Number of mushrooms" 
        }
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
                              xaxis = list(title = title),
                              yaxis = list(title ="")) %>% config(displayModeBar = FALSE)
        
        fig
    })
    
    output$properties <- renderPlotly({
        #get all info
        edible <- getEdible()
        poisonous <- getPoisonous()
        
        columnnames <- colnames(edible)
        edibleproperties <- lapply(columnnames, function(colname){
            cnt <- edible[, colname] %>% count
            cnt$x <- paste(gsub("\\.", " ", colname), cnt$x)
            return(cnt)
        })[-1]
        
        columnnames <- colnames(poisonous)
        poisonousproperties <- lapply(columnnames, function(colname){
            cnt <- poisonous[, colname] %>% count
            cnt$x <- paste(gsub("\\.", " ", colname), cnt$x)
            return(cnt)
        })[-1]
        
        poisonousproperties <- do.call(rbind, poisonousproperties)
        edibleproperties <- do.call(rbind, edibleproperties)
        
        properties <- merge(x = poisonousproperties, y = edibleproperties, by = "x", all = TRUE)
        properties[is.na(properties)] <- 0
        
        properties$sum <- properties$freq.x + properties$freq.y
        
        print(ascending())
        if(ascending() == T){
            properties <- properties[order(properties$sum), ]
        }else{
            properties <- properties[order(-properties$sum), ]
        }
        
        properties <- properties[input$amountOfProperties:1,]
        
        x <- properties$x
        y1 <- properties$freq.x
        y2 <- properties$freq.y
        data <- data.frame(x, y1, y2)
        
        #The default order will be alphabetized unless specified as below:
        data$x <- factor(data$x, levels = data[["x"]])
        
        fig <- plot_ly(data, x = ~y1, y = ~x, type = 'bar', name = 'Poisonous', marker = list(color = 'red'), orientation='h')
        fig <- fig %>% add_trace(x = ~y2, name = 'Edible', marker = list(color = 'green'))
        fig <- fig %>% layout(xaxis = list(title = "Amount of mushrooms with property" ),
                              yaxis = list(title = "Mushroom property"),
                              margin = list(b = 100),
                              barmode = 'group')
        
        fig
    })
    
    generateDeath <- function(percentage){
        if(!is.nan(percentage)){
            if(percentage == 0){
                "No"
            }
            else if(percentage < 25){
                "Possibly"
            }else if(percentage < 50){
                "Likely"
            }else if(percentage < 75){
                "Very likely"
            }else if(percentage < 100){
                "Almost certainly"
            }else{
                "Yes"
            }
        }
    }
    
    output$edibilityStatsMain <- renderText({
        edible <- nrow(getEdible())
        poisonous <- nrow(getPoisonous())
        total <- edible + poisonous
        ediblePercentage <- round((edible/total)* 100, digits=2) 
        poisonousPercentage <- round((poisonous/total) *100, digits=2) 
        if(is.nan(ediblePercentage)){ediblePercentage <- 0}
        if(is.nan(poisonousPercentage)){poisonousPercentage <- 0}
        text1 <- paste("<h2><b>Will you die?</b> ", generateDeath(poisonousPercentage), "</h2><br>")
        text2 <- paste("<b>Number of edible mushrooms:</b> ", edible, ", <b>Percentage:</b> ", ediblePercentage, "<br>")
        text3 <- paste("<b>Number of poisonous mushrooms:</b> ", poisonous, ", <b>Percentage:</b> ", poisonousPercentage)
        paste(text1, text2, text3)    
    })
    
    output$edibilityStatsSecond <- renderText({
        edible <- nrow(getEdible())
        poisonous <- nrow(getPoisonous())
        if(edible > 0 || poisonous > 0){
            total <- edible + poisonous
            ediblePercentage <- round((edible/total)* 100, digits=2) 
            poisonousPercentage <- round((poisonous/total)*100, digits=2) 
            text1 <- paste("<h2><b>Will you die?</b> ", generateDeath(poisonousPercentage), "</h2><br>")
            text2 <- paste("<b>Number of edible mushrooms:</b> ", edible, ", <b>Percentage:</b> ", ediblePercentage, "<br>")
            text3 <- paste("<b>Number of poisonous mushrooms:</b> ", poisonous, ", <b>Percentage:</b> ", poisonousPercentage, "<br>")
            
            # mushies <- getEdible()
            # print(getEdible() %>% count(cap.shape))
            
            if(edible > 0){
                indexes <- which.maxn(adjecencyList()["edible",], 3)
                items <- adjecencyList()["edible",][indexes]
                top_properties <- paste(names(items), collapse = ", ")
                text4 <- paste("<b>Top 3 distinguishing properties of edible mushrooms in subset: </b>", top_properties, "<br>")
            }else{
                text4 <- ""
            }
            
            if(poisonous > 0){
                indexes <- which.maxn(adjecencyList()["poisonous",], 3)
                items <- adjecencyList()["poisonous",][indexes]
                top_properties <- paste(names(items), collapse = ", ")
                text5 <- paste("<b>Top 3 distinguishing properties of poisonous mushrooms in subset: </b>", top_properties, "<br>")
            }else{
                text5 <- ""
            }
            paste(text1, text2, text3, text4, text5)    
        }
        else{
            "<h2>No mushrooms found!</h2>Change the properties on the home page."
        }
    })
    
    #reset all inputs
    observeEvent(input$resetAll, {
        print("reset!")
        reset("form")
    })
    
    
    #Flip transposed value
    observeEvent(input$transpose, {
        if(transposed() == F){
            transposed(T)
        }else{
            transposed(F)
        }
    })
    
    #reset all inputs
    observeEvent(input$setNumberOfMushrooms, {
        setPlot(F)
    })
    
    #reset all inputs
    observeEvent(input$setPercentageOfMushrooms, {
        setPlot(T)
    })
    
    observeEvent(input$ascenddescent,{
        if(ascending() == T){
            ascending(F)
        }else{
            ascending(T)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
