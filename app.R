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

df_clusters <- read.csv("2023_04_16_00_32_52_recipe_clusteredweapons.csv",header=TRUE)
df_img <- data.frame(id = c(1:3), img_path = c("weapons/f7f68f95ed8ab879.jpg", "weapons/0a91369dacf3b60c.jpg", "weapons/0c9d371c13cd16f1.jpg"))

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
                             verbatimTextOutput("txtout"),
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Show Clusters",
                           mainPanel(
                             class = "myMainPanel",
                             h1("Cluster of Weapon Images"),
                             
                             h3("Amount of Image by Cluster"),
                             #tableOutput("futureData"),
                             plotOutput(outputId = "ggplot_cluster"),
                             
                             h3("Display Images from Cluster"),
                             # fluidRow(
                            #   column(width=4,imageOutput("myImage")),
                            #   column(width=4,imageOutput("myImage2"))
                               
                            # ),
                             h4("Multiple Images in DIV container"),
                             uiOutput("myImage5")
                           ) # mainPanel
                  ),
                  tabPanel("References", "This panel is intentionally left blank")
                  
                ), # navbarPage
                # Add a CSS block to adjust the margins of the main panel
                tags$head(
                  tags$style(
                    HTML("body {
                          margin: 0 auto;
                          align-items: center;
                          max-width: 800px; /* Adjust this value to your desired width */
                    }")
                  )
                )
) # fluidPage


# Define server function  
server <- function(input, output) {
  
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, sep = " " )
  })
  
  output$futureData <- renderTable(
    {
      df_clusters |> count(.cluster) |>
        arrange(as.numeric(sub(".*_", "", .cluster)))
    }
  )
  
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
  
  output$myImage2 <- renderImage(
    {
      filename <- file.path("weapons/0a91369dacf3b60c.jpg")
      list(src = filename,
           alt = "Here should be an image.",
           width = 240,
           height = 180)
    },deleteFile = FALSE
  )

  n <- nrow(df_img)
  
  observe({
    for (i in 1:n)
    {
      print(i)
      local({
        my_i <- i
        imagename = paste0(my_i)
        print(imagename)
        output[[imagename]] <-
          renderImage({
            list(src = file.path(df_img$img_path[my_i]), 
                 width = "240", height = "180",
                 alt = "Image failed to render")
          }, deleteFile = FALSE)
      })
    }
  })
  
  output$myImage5 <- renderUI({
    img_output_list <- 
      lapply(1:n,
             function(i){
               #imagename= paste0("img",i)
               #imageOutput(i)
               div(style = "display:inline-block;margin-bottom:-12em", imageOutput(i))
             })
    do.call(tagList,img_output_list)
  })
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)