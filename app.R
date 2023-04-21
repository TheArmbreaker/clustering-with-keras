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
#library(shiny)
#library(shinythemes)
#library(tidyverse)
#library(keras)
lapply(c("shiny", "shinythemes","shinycssloaders", "tidyverse", "keras", "recipes","tidyclust"), require, character.only = TRUE)

# Get Data
# df_clusters <- read.csv("2023_04_16_00_32_52_recipe_clusteredweapons.csv",header=TRUE)
# Get Clusters for DropDown Selection
# cluster_vector <- df_clusters |> select(.cluster)

myKerasModel <- application_vgg16(weights="imagenet",include_top=TRUE)
# Change Output to second last layer to access the feature map instead of classification result.
output <- myKerasModel$layers[[length(myKerasModel$layers)-1]]$output
myKerasModel <- keras_model(inputs=myKerasModel$input, outputs=output)


# Define UI
ui <- fluidPage(theme = shinytheme("united"),
                tags$head(
                  tags$style(
                    HTML(".image-container { height: 300px; overflow-y: auto; display:inline-block;}")
                  )
                ),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Image Clustering",
                  id = "navbar",
                  sidebarPanel(
                    selectInput("var_model","Load Images by Cluster",
                                choices = c("Flowers","Weapons","Flowers_predict","Weapons_predict"),
                                selected = 1),
                    #checkboxInput("var_clusRes", label="Prediction Cluster", value=FALSE),
                    actionButton("var_clusRes", label = "Load Model"),
                    htmlOutput("usedModel")  |> withSpinner(color="#0dc5c1"),
                  ),
                  tabPanel("Cluster New Image", value = "image_cluster",
                           mainPanel(
                             h1("Which cluster does your Image belong to?"),
                             h3("Upload an Image"),
                             fileInput("file1","",
                                       multiple=FALSE,
                                       accept=c(".jpg",".png")),
                             
                             h3("Your Image"),
                             imageOutput("myImage") |> withSpinner(color="#0dc5c1"),
                             h3("Images from the Cluster"),
                             verbatimTextOutput("img_array"),
                             verbatimTextOutput("img_cluster"),
                             uiOutput("format_images_cluster")
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Show Clusters", value = "show_cluster",
                           mainPanel(
                             class = "myMainPanel",
                             h1("Cluster of Weapon Images"),
                             
                             h3("Amount of Image by Cluster"),
                             plotOutput(outputId = "ggplot_cluster")  |> withSpinner(color="#0dc5c1"),
                             
                             h3("Display Images from Cluster"),
                             # fluidRow(
                            #   column(width=4,imageOutput("myImage")),
                            #   column(width=4,imageOutput("myImage2"))
                               
                            # ),
                            h4("Choose a cluster for sample images"),
                            selectInput("var_clus",
                                        "Choose a cluster:",
                                        choices = NULL,
                                        selected = NULL),
                            tableOutput("futureData"),
                            h4("Filenames and Images"),
                            verbatimTextOutput("filenames"),
                            uiOutput("format_images") |> withSpinner(color="#0dc5c1"),
                           ) # mainPanel
                  ),
                  tabPanel("References", value ="ref", "This panel is intentionally left blank")
                  
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
  
  reactValues <- reactiveValues(trigger_cluster=NULL,
                                myRender=NULL,
                                #activeShowCluster=NULL,
                                #activeImageCluster=NULL,
                                #dtuseModel=TRUE,
                                ldModel="None")
  
  df_clusters <- reactive({
    if (input$var_model == "Flowers"){
      df_clusters <- read.csv("2023_04_20_16_18_27_recipe_clusteredflowers.csv",header=TRUE)
    } else if (input$var_model == "Weapons"){
      df_clusters <- read.csv("2023_04_16_00_32_52_recipe_clusteredweapons.csv",header=TRUE)
    } else if (input$var_model == "Flowers_predict"){
      df_clusters <- read.csv("2023_04_20_16_18_27_recipe_clusteredflowers.csv",header=TRUE)
    } else if (input$var_model == "Weapons_predict") {
      df_clusters <- read.csv("2023_04_16_00_32_52_recipe_clusteredweapons.csv",header=TRUE)
    }
  })
  
  observe({
    cluster_vector <- df_clusters() |> select(.cluster)
    updateSelectInput(session,"var_clus",choices=cluster_vector,selected=NULL)
  })

  img_data <- function(myFilter) {
    df_img_files <- df_clusters() |>
      filter(.cluster==myFilter) |>
      select(myFiles)
    
    df_img_sample <- sample(df_img_files$myFiles,4)
    df_img <- data.frame(id = c(1:4), img_path = df_img_sample)
    df_img
  }

  observeEvent(input$var_clusRes,{
    model <- str_split(input$var_model,"_")[[1]][1]
    #print(paste("test",model[[1]][1]))
    if (model == "Flowers"){
      print("Flowers")
      reactValues$ldModel <- "Flowers"
      #reactValues$dtuseModel <- FALSE
    } else if (model == "Weapons") {
      print("Weapons")
      reactValues$ldModel <- "Weapons"
      #reactValues$dtuseModel <- FALSE
    }
    else {
      print("No Model loaded.")
      reactValues$ldModel <- "None"
      #reactValues$dtuseModel <- TRUE
    }
  })
  
  predModel <- function(myString){
    if (myString == "Flowers"){
      myClusterModel <- readRDS("flowers_cluster.rds")
    } else if (myString == "Weapons") {
      myClusterModel <- readRDS("weapon_cluster.rds")      
    }
    else {
      myClusterModel <- NULL
    }
  }
  
  output$usedModel <- renderUI({
    #req(input$var_clusRes)
    HTML(paste("<br/>","Active Model for prediction:",reactValues$ldModel,sep="<br/>"))
  })

  
  output$filenames <- renderText({
    req(input$var_clus)
    myString=""
    for (i in img_data(input$var_clus)[[2]]){
      myString <- paste0(myString,"\n",i)
    }
    substr(myString,2,nchar(myString))
  })
  
  output$futureData <- renderTable(
    {
      df_clusters() |> count(.cluster) |>
        filter(.cluster==input$var_clus)
    }
  )
  
  observeEvent(input$var_clus,{
    #req(reactValues$activeShowCluster)
    print("yeahaaa")
    for (i in 5:8){
      local({
        loc_i <- i
        imagename <- paste("img_",i,sep="")
        output[[imagename]] <-
        renderImage({
          list(
            src = file.path(str_to_lower(str_split(input$var_model,"_")[[1]][1]),img_data(input$var_clus)[loc_i-4,"img_path"]),
            width = "240", height = "180",
            alt = "Image failed to render"
          )}, deleteFile=FALSE)
        })
      }
    }, ignoreInit = TRUE)
  
  myImage2 <- renderImage(
    {
      req(input$var_clus)
      list(src = paste0("weapons/",img_data(input$var_clus)[1,"img_path"]),
           alt = "Here should be an image.",
           width = 240,
           height = 180)
    },deleteFile = FALSE
  )

  myImage3 <- renderImage(
    {
      req(input$var_clus)
      list(src = paste0("weapons/",img_data(input$var_clus)[2,"img_path"]),
           alt = "Here should be an image.",
           width = 240,
           height = 180)
    },deleteFile = FALSE
  )
  
  output$format_images <- renderUI({
    #req(reactValues$activeShowCluster)
    print("yeahaaa")
    myImages <- 
      lapply(5:8,
             function(i){
               imagename <- paste("img_",i,sep="")
               div(style="display:inline-block", imageOutput(imagename))
               })
    do.call(tagList,myImages)
  })
  
  output$ggplot_cluster <- renderPlot(
    {
      req(input$var_model)
      df_clusters() |>
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
      req(input$file1)
      list(src = input$file1$datapath,
           alt = "Here should be an image.",
           width = 240,
           height = 180)
    },deleteFile = FALSE
  )

  output$img_array <- renderText({
    req(input$file1)
    if (reactValues$ldModel != "None") {
    img <- image_load(input$file1$datapath, grayscale=FALSE,target_size = c(224,224))
    img_array <- image_to_array(img)
    reshaped_image_array <- array_reshape(img_array,c(1,dim(img_array)))
    prepro_img <- imagenet_preprocess_input(reshaped_image_array)
    features <- myKerasModel |> predict(prepro_img)
    res <- predict(predModel(reactValues$ldModel),new_data=as.data.frame(features))
    res_char <- as.character(res$".pred_cluster")
    reactValues$trigger <- res_char }
    else {
      "No Model loaded."
    }
  })

  output$img_cluster <- renderText({
    req(reactValues$trigger)
    reactValues$trigger
  })
  
  observeEvent(reactValues$trigger,{
    #req(reactValues$activeImageCluster)
    print("nooooo")
    for (i in 1:4){
      local({
        loc_i <- i
        imagename <- paste("img_",i,sep="")
        output[[imagename]] <-
          renderImage({
            list(
              src = file.path(str_to_lower(str_split(input$var_model,"_")[[1]][1]),img_data(reactValues$trigger)[loc_i,"img_path"]),
              width = "240", height = "180",
              alt = "Image failed to render"
            )}, deleteFile=FALSE)
      })
    }
  }, ignoreInit = TRUE)

  output$format_images_cluster <- renderUI({
    #req(reactValues$activeImageCluster)
    print("nooooo")
    myImages <- 
      lapply(1:4,
             function(i){
               imagename <- paste("img_",i,sep="")
               div(style="display:inline-block", imageOutput(imagename))
             })
    do.call(tagList,myImages)
  })
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)