library(shiny)
library(shinyFiles)
library(tidyverse)
shinyUI(fluidPage(sidebarLayout(
  
  sidebarPanel(
    # First, choose a directory
    h4("1. Select the data folder for an IonAnalytics project"),
    shinyDirButton("directory", "Folder select", "Please select a folder"),
    # verbatimTextOutput("directorypath"), #outputs chosen paths for debugging
    
    # Second, exclude any samples (folders).  By default they are all checked
    h4("2. Choose which samples to include"),
    checkboxGroupInput("sample_choice", "Choose Samples"), 
    # verbatimTextOutput("chosensamples"), 
    
    
    # Third, choose which (if there is more than one) .csv file name to import
    h4("3. Choose which report to analyze"),
    checkboxGroupInput("report_choice", "Choose a report"),
    
    # Fourth, submit and import those files!
    actionButton("go", "Import Reports")
  ),

  mainPanel(
    # dataTableOutput("test"),
    # plotOutput("diagnostic_plot"),
    h2("Compounds sorted by standard deviation of retention time"),
    plotlyOutput("diagnostic_plot"),
    numericInput("page", "Page", min = 1, max = 10, value = 1, step = 1, width = "75px"),
    dataTableOutput("brush")
  )
  
))) 