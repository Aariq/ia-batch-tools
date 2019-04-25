library(shiny)
library(shinyFiles)
library(tidyverse)
library(glue)
library(chemhelper)
home = "~/Documents/ia-batch-tools" #for testing.  Change to "~" for production
# home = "~"
shinyServer(function(input, output, session) {
  
  shinyDirChoose(input, "directory", roots = c(home = home))
  
  #sends to UI for debugging
  output$directorypath <- renderPrint({
    full_paths <- dir(parseDirPath(c(home = home), input$directory), full.names = TRUE)
    full_paths
  })
  # Save directory choice three ways:
  data_dir <- reactive({
    parseDirPath(c(home = home), input$directory)
  })
  
  folders <- reactive({
    list.files(parseDirPath(c(home = home), input$directory))
  })
  
  full_paths <- reactive({
    dir(parseDirPath(c(home = home), input$directory), full.names = TRUE)
  })
  
  
  # display sample folders as a checkbox selction with all selected by default
  observe({
    #get folder names
    
    # makes it have no choices until you select a directory
    if (is.null(folders()))
      folders() <- character(0)
    
    # Updates checkbox with report options
    updateCheckboxGroupInput(session, "sample_choice",
                             label = "Choose Samples",
                             choices = folders(),
                             selected = folders())
  })
  
  output$chosensamples <- renderPrint({ #for debugging only
    input$sample_choice
  })
  
  # Save choices of folders
  chosen_folders <- reactive({
    full_paths()[list.files(data_dir()) %in% input$sample_choice]
  })
  
  #get .csv files in chosen folders
  reports <- reactive({
    list.files(chosen_folders(), pattern = "*\\.csv")
  })
  
  #Updating checkbox for .csv file name
  observe({
    # create options that are the file names plus the number of samples that have that file name
    # df <- as.data.frame(table(reports))
    # report_options <- glue("{df$reports} ({df$Freq})")
    report_options <- unique(reports())
    
    #  makes it have no choices until you select a directory
    if (is.null(report_options))
      files <- character(0)
    
    # Updates checkbox with report options
    updateCheckboxGroupInput(session, "report_choice",
                             label = "Choose a report",
                             choices = report_options,
                             selected = NULL
    )
  })
  
  # Import button
  data <- eventReactive(input$go, {
    final_paths <- paste0(chosen_folders(), "/", input$report_choice)
    
    map(final_paths, read_IA) %>%
      set_names(basename(chosen_folders())) %>%
      bind_rows(.id = "sample")
  })
  
  
  output$test <- renderDataTable({
    data()
  })
  
})