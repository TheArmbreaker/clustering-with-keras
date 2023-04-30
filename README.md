# Image Clustering with Keras

## Introduction

The requirement of self-labeled image data became obvious, when I was researching a project for my masterthesis.

I was looking for a way to automatically sort images by their content and found a project from Gabo Flomo on [towardsdatascience.com (2020)](https://towardsdatascience.com/how-to-cluster-images-based-on-visual-similarity-cd6e7209fe34), in which flower-images are clustered by similarity.

In this R learning project the python code from Gabo Flomo is used as blueprint. It is translated to R and incorporated to a Data Science Life Cycle.

## Datasets

The images can be found here:

-   Weapons in Images on [kaggle.com](https://www.kaggle.com/datasets/jubaerad/weapons-in-images-segmented-videos)
-   Flower Color Images on [kaggle.com](https://www.kaggle.com/datasets/olgabelitskaya/flower-color-images)

To execute the code on a locally run shiny web app the data shall be copied in folders "flowers" and "weapons" of the Rproject folder. From the dataset-downloads the sub folders *flower_images* and *Weapons-in-Images* are required.

## Deployment

The final clusters where deployed on shinyapps.io.  
[Link to my App on Shinyapps.io](https://thearmbreaker.shinyapps.io/clustering-with-keras/)