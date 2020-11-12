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



# Define server logic required to draw a histogram
server <- function(input, output) {
    adjecencyList <- reactiveVal()
    ascending <- reactiveVal(F)
    generatedataset <- reactive({
        #Read csv
        mushrooms <- original
        
        mushrooms <- as.data.frame(sapply(listnames, function(item) {
            mushrooms[, item] <-
                revalue(x = mushrooms[, item], replace = unlist(l[[item]]))
        }))
        
        colnames <- colnames(mushrooms)[-1]
        
        for (colname in colnames) {
            if (input[[colname]] != "") {
                # print(paste(colname, input[[colname]], sep=": "))
                mushrooms <-
                    mushrooms[mushrooms[[colname]] == input[[colname]],]
            }
        }
        return(mushrooms)
    })
    
    #Make this subset reactive to prevent excessive computation
    getEdible <- reactive({
        mushrooms <- generatedataset()
        mushrooms[mushrooms$class == 'edible', ]
    })
    #Make this subset reactive to prevent excessive computation
    getPoisonous <- reactive({
        mushrooms <- generatedataset()
        mushrooms[mushrooms$class == 'poisonous', ]
    })
    
    output$error <- renderText({
        if (nrow(getEdible()) == 0 && nrow(getPoisonous()) == 0) {
            "<h2>No mushrooms found!</h2>"
        } else{
            ""
        }
    })
    
    transposed <- reactiveVal(FALSE)
    
    output$distPlot <- renderChorddiag({
        mushrooms <- generatedataset()
        #all edible and all poisonous mushrooms
        edible <- getEdible()
        poisonous <- getPoisonous()
        if (nrow(edible) > 0 || nrow(poisonous) > 0) {
            creatematrix <- function(colname, dataset, class) {
                #Group by colname and count individual items
                groupby <- dataset %>% count(colname)
                
                #Get the individual values and make names for them for the chart
                rownames <- groupby[, colname]
                chartnames <- paste(colname, rownames, sep = ".")
                
                #make matrix with chartnames as rownames and count as column name
                #then bind the class as an extra column
                m <-
                    matrix(groupby$freq, dimnames = list(as.list(chartnames), "value"))
                m <- cbind(m, from = class)
                return(m)
            }
            
            #Get all column names, except "class"
            column_names <- colnames(mushrooms)[-1]
            
            #For each column name in the dataset edible, create a matrix
            #that contains the count of each individual value per column.
            #Bind this to one dataframe.
            if (nrow(edible) > 0) {
                a <- sapply(column_names, creatematrix, edible, "edible")
                df_1 <- do.call(rbind, a)
                
                
                df_1 <-
                    head(df_1[order(-as.numeric(df_1[, "value"])), ], n = 10)
            } else{
                df_1 <- NULL
            }
            
            if (nrow(poisonous) > 0) {
                a <- sapply(column_names,
                            creatematrix,
                            poisonous,
                            "poisonous")
                df_2 <- do.call(rbind, a)
                df_2 <-
                    head(df_2[order(-as.numeric(df_2[, "value"])), ], n = 10)
            } else{
                df_2 <- NULL
            }
            
            df = rbind(df_1, df_2)
            df = cbind(df, to = rownames(df))
            
            #reset rowsnames to [1:n] and replace rows to ["from", "to", "value"]
            df <- df[, c(2, 3, 1)]
            rownames(df) <- 1:nrow(df)
            
            #Make dataframe and cast value from string to integer
            df <- as.data.frame(df)
            df$value <- as.integer(df$value)
            
            #clean values
            df$to <- gsub('\\.', " ", df$to)
            
            
            #Make adjecent matrix since chorddiag does not understand adjecent list
            x <- adjacencyList2Matrix(df, square = TRUE)
            adjecencyList(x)
            #convert to data matrix
            if (transposed()) {
                x <- data.matrix(x)
            } else{
                x <- data.matrix(x) %>% t
            }
            #plot diagram
            chorddiag(
                x,
                type = "directional",
                showTicks = F,
                groupnameFontsize = 14,
                groupnamePadding = 5,
                margin = 200
            )
        }
    })
    setPlot <- reactiveVal(F)
    
    output$ratio <- renderPlotly({
        type <- setPlot()
        edible <- nrow(getEdible())
        poisonous <-  nrow(getPoisonous())
        y <- c('')
        if (type == T) {
            total <- edible + poisonous
            if (total > 0) {
                edible <- (edible / total) * 100
                poisonous <- (poisonous / total) * 100
            } else{
                edible <- poisonous <- 0
            }
            title <- "Percentage"
        } else{
            title <- "Number of mushrooms"
        }
        data <- data.frame(y, edible, poisonous)
        
        
        
        fig <-
            plot_ly(
                data,
                x = ~ edible,
                y = ~ y,
                type = 'bar',
                orientation = 'h',
                name = 'Edible',
                marker = list(
                    color = 'rgba(56, 188, 98, 0.8)',
                    line = list(color = 'rgba(0, 255, 0, 1.0)',
                                width = 3)
                )
                ,
                showlegend = TRUE
            )
        fig <- fig %>% add_trace(
            x = ~ poisonous,
            name = 'Poisonous',
            marker = list(
                color = 'rgba(255, 0, 0, 0.8)',
                line = list(color = 'rgba(255, 0, 0, 1.0)',
                            width = 3)
            )
        )
        fig <- fig %>% layout(
            barmode = 'stack',
            xaxis = list(title = title),
            yaxis = list(title = "")
        ) %>% config(displayModeBar = FALSE)
        
        fig
    })
    
    output$properties <- renderPlotly({
        #get all info
        edible <- getEdible()
        poisonous <- getPoisonous()
        
        columnnames <- colnames(edible)
        edibleproperties <- lapply(columnnames, function(colname) {
            cnt <- edible[, colname] %>% count
            cnt$x <- paste(gsub("\\.", " ", colname), cnt$x)
            return(cnt)
        })[-1]
        
        columnnames <- colnames(poisonous)
        poisonousproperties <-
            lapply(columnnames, function(colname) {
                cnt <- poisonous[, colname] %>% count
                cnt$x <- paste(gsub("\\.", " ", colname), cnt$x)
                return(cnt)
            })[-1]
        
        poisonousproperties <- do.call(rbind, poisonousproperties)
        edibleproperties <- do.call(rbind, edibleproperties)
        
        properties <-
            merge(
                x = poisonousproperties,
                y = edibleproperties,
                by = "x",
                all = TRUE
            )
        properties[is.na(properties)] <- 0
        
        properties$sum <- properties$freq.x + properties$freq.y
        
        print(ascending())
        if (ascending() == T) {
            properties <- properties[order(properties$sum),]
        } else{
            properties <- properties[order(-properties$sum),]
        }
        
        properties <- properties[input$amountOfProperties:1, ]
        
        x <- properties$x
        y1 <- properties$freq.x
        y2 <- properties$freq.y
        data <- data.frame(x, y1, y2)
        
        #The default order will be alphabetized unless specified as below:
        data$x <- factor(data$x, levels = data[["x"]])
        
        fig <-
            plot_ly(
                data,
                x = ~ y1,
                y = ~ x,
                type = 'bar',
                name = 'Poisonous',
                marker = list(color = 'red'),
                orientation = 'h'
            )
        fig <-
            fig %>% add_trace(
                x = ~ y2,
                name = 'Edible',
                marker = list(color = 'green')
            )
        fig <-
            fig %>% layout(
                xaxis = list(title = "Amount of mushrooms with property"),
                yaxis = list(title = "Mushroom property"),
                margin = list(b = 100),
                barmode = 'group'
            )
        
        fig
    })
    
    generateDeath <- function(percentage) {
        if (!is.nan(percentage)) {
            if (percentage == 0) {
                "No"
            }
            else if (percentage < 25) {
                "Possibly"
            } else if (percentage < 50) {
                "Likely"
            } else if (percentage < 75) {
                "Very likely"
            } else if (percentage < 100) {
                "Almost certainly"
            } else{
                "Yes"
            }
        }
    }
    
    output$edibilityStatsMain <- renderText({
        edible <- nrow(getEdible())
        poisonous <- nrow(getPoisonous())
        total <- edible + poisonous
        ediblePercentage <- round((edible / total) * 100, digits = 2)
        poisonousPercentage <-
            round((poisonous / total) * 100, digits = 2)
        if (is.nan(ediblePercentage)) {
            ediblePercentage <- 0
        }
        if (is.nan(poisonousPercentage)) {
            poisonousPercentage <- 0
        }
        text1 <-
            paste(
                "<h2><b>Will you die?</b> ",
                generateDeath(poisonousPercentage),
                "</h2><br>"
            )
        text2 <-
            paste(
                "<b>Number of edible mushrooms:</b> ",
                edible,
                ", <b>Percentage:</b> ",
                ediblePercentage,
                "<br>"
            )
        text3 <-
            paste(
                "<b>Number of poisonous mushrooms:</b> ",
                poisonous,
                ", <b>Percentage:</b> ",
                poisonousPercentage
            )
        paste(text1, text2, text3)
    })
    
    output$edibilityStatsSecond <- renderText({
        edible <- nrow(getEdible())
        poisonous <- nrow(getPoisonous())
        if (edible > 0 || poisonous > 0) {
            total <- edible + poisonous
            ediblePercentage <-
                round((edible / total) * 100, digits = 2)
            poisonousPercentage <-
                round((poisonous / total) * 100, digits = 2)
            text1 <-
                paste(
                    "<h2><b>Will you die?</b> ",
                    generateDeath(poisonousPercentage),
                    "</h2><br>"
                )
            text2 <-
                paste(
                    "<b>Number of edible mushrooms:</b> ",
                    edible,
                    ", <b>Percentage:</b> ",
                    ediblePercentage,
                    "<br>"
                )
            text3 <-
                paste(
                    "<b>Number of poisonous mushrooms:</b> ",
                    poisonous,
                    ", <b>Percentage:</b> ",
                    poisonousPercentage,
                    "<br>"
                )
            
            # mushies <- getEdible()
            # print(getEdible() %>% count(cap.shape))
            
            if (edible > 0) {
                indexes <- which.maxn(adjecencyList()["edible", ], 3)
                items <- adjecencyList()["edible", ][indexes]
                top_properties <-
                    paste(names(items), collapse = ", ")
                text4 <-
                    paste(
                        "<b>Top 3 distinguishing properties of edible mushrooms in subset: </b>",
                        top_properties,
                        "<br>"
                    )
            } else{
                text4 <- ""
            }
            
            if (poisonous > 0) {
                indexes <- which.maxn(adjecencyList()["poisonous", ], 3)
                items <- adjecencyList()["poisonous", ][indexes]
                top_properties <-
                    paste(names(items), collapse = ", ")
                text5 <-
                    paste(
                        "<b>Top 3 distinguishing properties of poisonous mushrooms in subset: </b>",
                        top_properties,
                        "<br>"
                    )
            } else{
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
        if (transposed() == F) {
            transposed(T)
        } else{
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
    
    observeEvent(input$ascenddescent, {
        if (ascending() == T) {
            ascending(F)
        } else{
            ascending(T)
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
