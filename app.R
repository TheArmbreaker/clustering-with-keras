####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
library(shinythemes)
library(tidyverse)

# Get Data
df_clusters <- read.csv("2023_04_16_00_32_52_recipe_clusteredweapons.csv",header=TRUE)
# Get Clusters for DropDown Selection
cluster_vector <- df_clusters |> select(.cluster)

# df_img_files <- df_clusters |> filter(.cluster==cluster_vector[[1]][1]) |> select(myFiles)
# df_img_sample <- sample(df_img_files$myFiles,4)
# df_img <- data.frame(id = c(1:4), img_path = df_img_sample)

# Define UI
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Image Clustering",
                  tabPanel("Cluster New Image",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("txt1", "Given Name:", ""),
                             textInput("txt2", "Surname:", ""),
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Header 1"),
                             
                             h4("Output 1"),
                             #verbatimTextOutput("txtout"),
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Show Clusters",
                           mainPanel(
                             class = "myMainPanel",
                             h1("Cluster of Weapon Images"),
                             
                             h3("Amount of Image by Cluster"),
                             tableOutput("futureData"),
                             plotOutput(outputId = "ggplot_cluster"),
                             
                             h3("Display Images from Cluster"),
                             # fluidRow(
                            #   column(width=4,imageOutput("myImage")),
                            #   column(width=4,imageOutput("myImage2"))
                               
                            # ),
                            h4("Choose a cluster for sample images"),
                            selectInput("var_clus",
                                        "Choose a cluster:",
                                        cluster_vector,
                                        selected = 1),
                            h4("Filenames and Images"),
                            verbatimTextOutput("filenames"),
                            uiOutput("myImage5")
                           ) # mainPanel
                  ),
                  tabPanel("References", "This panel is intentionally left blank")
                  
                ), # navbarPage
                # Add a CSS block to adjust the margins of the main panel
                #tags$head(
                #  tags$style(
                #    HTML("body {
                #          margin: 0 auto;
                #          align-items: center;
                #          max-width: 800px; /* Adjust this value to your desired width */
                #    }")
                #  )
                # )
) # fluidPage


# Define server function  
server <- function(input, output, session) {
  
  img_data <- reactive({
    df_img_files <- df_clusters |>
      filter(.cluster==input$var_clus) |>
      select(myFiles)
    
    df_img_sample <- sample(df_img_files$myFiles,4)
    df_img <- data.frame(id = c(1:4), img_path = df_img_sample)
    df_img
  })
  
  output$filenames <- renderText({
    myString=""
    for (i in img_data()[[2]]){
      myString <- paste0(myString,"\n",i)
    }
    substr(myString,2,nchar(myString))
  })
  
  output$futureData <- renderTable(
    {
      df_clusters |> count(.cluster) |>
        filter(.cluster==input$var_clus)
    }
  )

  observe({
    for (i in 1:4)
    {
      print(i)
      local({
        my_i <- i
        imagename = paste0(my_i)
        print(imagename)
        output[[imagename]] <-
          renderImage({
            list(src = file.path("weapons",img_data()$img_path[my_i]), 
                 width = "240", height = "180",
                 alt = "Image failed to render")
          }, deleteFile = FALSE)
      })
    }
  })
  
  output$myImage5 <- renderUI({
    img_output_list <- 
      lapply(1:4,
             function(i){
               imagename= paste0(i)
               #imageOutput(i)
               div(style = "display:inline-block;margin-bottom:-12em", imageOutput(imagename))
             })
    do.call(tagList,img_output_list)
  })
  
  output$ggplot_cluster <- renderPlot(
    {
      df_clusters |>
        count(.cluster) |>
          ggplot(aes(x=reorder(.cluster,as.numeric(sub(".*_", "", .cluster))),y=n,fill=.cluster)) +
          geom_bar(stat="identity") + 
          theme_minimal()+
          scale_fill_viridis_d() +
          labs(x="Cluster",y="Count") +
          guides(fill=FALSE)
    }
  )
  
  output$myImage <- renderImage(
    {
      filename <- file.path("weapons/0a1cfb1bb8e135c2.jpg")
      list(src = filename,
           alt = "Here should be an image.",
           width = 240,
           height = 180)
    },deleteFile = FALSE
  )


} # server


# Create Shiny object
shinyApp(ui = ui, server = server)