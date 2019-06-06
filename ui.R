library(shiny)
library(shinyFiles)
library(tidyverse)
library(plotly)
shinyUI(fluidPage(sidebarLayout(
  
  sidebarPanel(
    h4("1. Save Reports in IonAnalytics"),
    p('In IonAnalytics, after searching a file with a method, click the "Export" button on the Integration Report tab and save the .csv file in the default location (inside the .D folder) with the default filename.  Do this for all samples.'),
    
    # First, choose a directory
    h4("2. Select the data folder for an IonAnalytics project"),
    shinyDirButton("directory", "Folder select", "Please select a folder"),
    # verbatimTextOutput("directorypath"), #outputs chosen paths for debugging
    
    # Second, exclude any samples (folders).  By default they are all checked
    h4("3. Choose which samples to include"),
    checkboxGroupInput("sample_choice", "Choose Samples"), 
    # verbatimTextOutput("chosensamples"), 
    
    # Third, choose which (if there is more than one) .csv file name to import
    h4("4. Choose which report to analyze"),
    p("You may have used multiple methods on a file, each producing its own .csv file. Currently, you must choose a .csv file present in all of the samples you selected above or it will crash."),
    checkboxGroupInput("report_choice", "Choose a report"),
    
    # Fourth, submit and import those files!
    actionButton("go", "Import Reports")
  ),

  mainPanel(
    # dataTableOutput("test"),
    # plotOutput("diagnostic_plot"),
    
    
    tabsetPanel(type = "tabs", id = "tabs",
                  #this tab would have the RT deviation plotly plot and interactive table
                tabPanel("RT",
                         h2("Compounds sorted by standard deviation of retention time"),
                         p("Hover over points to view details and select points to generate a table."),
                         plotlyOutput("diagnostic_plot"),
                         numericInput("page", "Page", min = 1, max = 10, value = 1, step = 1, width = "75px"),
                         # sliderInput("page", "Page", min = 1, max = 10, value = 1, step = 1),
                         dataTableOutput("brush")))
                  #this tab would have a simple table of compounds sorted by mean Q-value
    #             tabPanel("Q-Value", diagnostics %>% arrange(q_value)),
                  #this tab would have some diagnostics about peak width?
    #             tabPanel("Width", diagnostics %>% arrange(desc(sd_width))),
                  #this tab would have some preliminary outlier diagnostics just to catch errors?
    #             tabPanel("Outliers", pcaoutlierplot, HDoutliersoutput)
                # )
    
    # h2("Compounds sorted by standard deviation of retention time"),
    # p("Hover over points to view details and select points to generate a table."),
    # plotlyOutput("diagnostic_plot"),
    # numericInput("page", "Page", min = 1, max = 10, value = 1, step = 1, width = "75px"),
    # dataTableOutput("brush")
  )
  
))) 