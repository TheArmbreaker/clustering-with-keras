####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load required R packages
lapply(c("shiny", "shinythemes","shinycssloaders", "tidyverse", "keras", "recipes","tidyclust"), require, character.only = TRUE)

# Load model from keras
myKerasModel <- application_vgg16(weights="imagenet",include_top=TRUE)
# Change Output to second last layer to access the feature map instead of classification result.
output <- myKerasModel$layers[[length(myKerasModel$layers)-1]]$output
myKerasModel <- keras_model(inputs=myKerasModel$input, outputs=output)

secs <- 1

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
                    conditionalPanel(
                      condition = "input.navbar != 'userguide'",
                      selectInput("var_model","Load Images by Cluster",
                                  choices = c("Flowers","Weapons","Flowers_predict","Weapons_predict"),
                                  selected = 1),
                      #checkboxInput("var_clusRes", label="Prediction Cluster", value=FALSE),
                      actionButton("var_clusRes", label = "Load Model"),
                      htmlOutput("usedModel")  |> withSpinner(color="#0dc5c1"),
                    )
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
                            fluidRow(
                              column(width=6,selectInput("var_clus",
                                                         "Choose a cluster:",
                                                         choices = NULL,
                                                         selected = NULL)),
                              column(width=6, tableOutput("futureData"))
                            ),
                            fluidRow(
                              column(width=6,textInput("label_cluster", "Label the Cluster")),
                              column(width=4,actionButton("store_label", label = "Write to DB"))
                            ),
                            verbatimTextOutput("savedLabel"),
                            h4("Filenames and Images"),
                            verbatimTextOutput("filenames"),
                            uiOutput("format_images") |> withSpinner(color="#0dc5c1"),
                           ) # mainPanel
                  ),
                  tabPanel("User Guide", value ="userguide",
                           br(),br(),
                           uiOutput("ref"),
                           )
                  
                ), # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output, session) {

# Load Data
  
  # react values
  reactValues <- reactiveValues(trigger_cluster=NULL,
                                myRender=NULL,
                                ldModel="None",
                                label_cluster_name="none")
  
  # load data from model results
  df_clusters <- reactive({
    if (input$var_model == "Flowers"){
      df_clusters <- read.csv("noRecipe_flowers.csv",header=TRUE)
    } else if (input$var_model == "Weapons"){
      df_clusters <- read.csv("noRecipe_weapons.csv",header=TRUE)
    } else if (input$var_model == "Flowers_predict"){
      df_clusters <- read.csv("recipe_flowers.csv",header=TRUE)
    } else if (input$var_model == "Weapons_predict") {
      df_clusters <- read.csv("recipe_weapons.csv",header=TRUE)
    }
  })
  
  # extract clusters for selectionInput.
  observe({
    cluster_vector <- df_clusters() |> select(.cluster) |> arrange(as.numeric(sub(".*_", "", .cluster)))
    updateSelectInput(session,"var_clus",choices=cluster_vector,selected=NULL)
  })

# Load Images
  
  # extract filenames by cluster
  img_data <- function(myFilter) {
    df_img_files <- df_clusters() |>
      filter(.cluster==myFilter) |>
      select(myFiles)

    df_img_sample <- sample(df_img_files$myFiles,4)
    df_img <- data.frame(id = c(1:4), img_path = df_img_sample)
    df_img
  }

  # observe Event for Show Clusters
  # this renders each image in a separate environment
  observeEvent(input$var_clus,{
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
  
  # observe Event for Cluster New Images
  # this renders each image in a separate environment
  observeEvent(reactValues$trigger,{
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
  
# Load Model
  
  # Set string with model path for predict() function
  observeEvent(input$var_clusRes,{
    model <- str_split(input$var_model,"_")[[1]][1]
    #print(paste("test",model[[1]][1]))
    if (model == "Flowers"){
      print("Flowers")
      reactValues$ldModel <- "Flowers"
    } else if (model == "Weapons") {
      print("Weapons")
      reactValues$ldModel <- "Weapons"
    }
    else {
      print("No Model loaded.")
      reactValues$ldModel <- "None"
    }
  })
  
  # load modal based on path argument
  predModel <- function(myString){
    if (myString == "Flowers"){
      myClusterModel <- readRDS("flowers_cluster.rds")
    } else if (myString == "Weapons") {
      myClusterModel <- readRDS("weapons_cluster.rds")      
    }
    else {
      myClusterModel <- NULL
    }
  }
  
# Page Show Clusters
  
  # display the filenames of the rendered images
  output$filenames <- renderText({
    req(input$var_clus)
    myString=""
    for (i in img_data(input$var_clus)[[2]]){
      myString <- paste0(myString,"\n",i)
    }
    substr(myString,2,nchar(myString))
  })
  
  # display count of images in cluster
  output$futureData <- renderTable(
    {
      df_clusters() |> count(.cluster) |>
        filter(.cluster==input$var_clus)
    }
  )
  
  # Output the loaded and rendered images
  output$format_images <- renderUI({
    myImages <- 
      lapply(5:8,
             function(i){
               imagename <- paste("img_",i,sep="")
               div(style="display:inline-block", imageOutput(imagename))
               })
    do.call(tagList,myImages)
  })
  
  # display plot with count for cluster results
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
  
# Page Cluster New Image
  
  # display the currently loaded prediction model
  output$usedModel <- renderUI({
    HTML(paste("<br/>","Active Model for prediction:",reactValues$ldModel,sep="<br/>"))
  })
  
  # render and output the image that is provided from the user
  output$myImage <- renderImage(
    {
      req(input$file1)
      list(src = input$file1$datapath,
           alt = "Here should be an image.",
           width = 240,
           height = 180)
    },deleteFile = FALSE
  )

  # feature extraction with keras and prediction with recipe-workflow
  output$img_array <- renderText({
    req(input$file1)
    if (reactValues$ldModel != "None") {
    # loading and preparing image provided by the user
    img <- image_load(input$file1$datapath, grayscale=FALSE,target_size = c(224,224))
    img_array <- image_to_array(img)
    reshaped_image_array <- array_reshape(img_array,c(1,dim(img_array)))
    prepro_img <- imagenet_preprocess_input(reshaped_image_array)
    # extracting features with keras
    features <- myKerasModel |> predict(prepro_img)
    # prediction with workflow
    res <- predict(predModel(reactValues$ldModel),new_data=as.data.frame(features))
    res_char <- as.character(res$".pred_cluster")
    reactValues$trigger <- res_char }
    else {
      "No Model loaded."
    }
  })

  #output$img_cluster <- renderText({
  #  req(reactValues$trigger)
  #  reactValues$trigger
  #})

  # Output the loaded and rendered images
  output$format_images_cluster <- renderUI({
    myImages <- 
      lapply(1:4,
             function(i){
               imagename <- paste("img_",i,sep="")
               div(style="display:inline-block", imageOutput(imagename))
             })
    do.call(tagList,myImages)
  })

  # name the cluster
  observeEvent(input$store_label,{
    reactValues$label_cluster_name <- input$label_cluster
    output$savedLabel <- renderText({
      paste("stored in database:",reactValues$label_cluster_name)
    })
  })

# Guide Page  
  getPage <- function(){
    return(includeHTML("TecDoc_Shiny.html"))
  }
  
  output$ref <- renderUI({
    getPage()
  })
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)