library(shiny)
library(shinyFiles)
library(tidyverse)
shinyUI(fluidPage(sidebarLayout(
  
  sidebarPanel(
    p("Select the data folder for an IonAnalytics project"),
    shinyDirButton("directory", "Folder select", "Please select a folder"),
    # verbatimTextOutput("directorypath"),
    
    p("Choose which samples to include"),
    checkboxGroupInput("inCheckboxGroup1", "Choose Samples",
                       c("Item A", "Item B", "Item C")),
    
    p("Choose which report to analyze"),
    checkboxGroupInput("inCheckboxGroup2", "Choose a report",
                       c("Item A", "Item B", "Item C"))
  ),

  mainPanel(

    
  )
  
))) 