library(shiny)
library(shinyFiles)
library(tidyverse)
library(glue)
library(chemhelper)
home = "~/Documents/ia-batch-tools" #for testing.  Change to "~" for production
# home = "~"
shinyServer(function(input, output, session) {
  
  shinyDirChoose(input, "directory", roots = c(home = home))
  # #prints to console for debugging purposes
  # observe({
  #   cat("\ninput$directory value:\n\n")
  #   print(input$directory)
  # })
  
  #sends to UI for debugging
  output$directorypath <- renderPrint({
    full_paths <- dir(parseDirPath(c(home = home), input$directory), full.names = TRUE)
    full_paths
  })
  
  # display directories as a checkbox selction with all selected by default
  observe({
    #get folder names
    data_dir <- parseDirPath(c(home = home), input$directory)
    folders <- list.files(parseDirPath(c(home = home), input$directory))
    full_paths <- dir(parseDirPath(c(home = home), input$directory), full.names = TRUE)
    
    # print(full_paths)
    # print(data_dir)
    
    #  makes it have no choices until you select a directory
    if (is.null(folders))
      folders <- character(0)
    
    # Updates checkbox with report options
    
    updateCheckboxGroupInput(session, "sample_choice",
                             label = "Choose Samples",
                             choices = folders,
                             selected = folders)
    
  })
  
output$chosensamples <- renderPrint({
  input$sample_choice
})
  # gets names of .csv files at tips of directory and updates a checkbox selection
  observe({
    #get the .csv file names
    
    data_dir <- parseDirPath(c(home = home), input$directory) #root directory with all samples
    folders <- list.files(data_dir) #list the folders (samples)
    chosen_folders <- list.files(data_dir, full.names = TRUE)[list.files(data_dir) %in% input$sample_choice] #filter by choices from updating checkbox and get full paths
    reports <- list.files(chosen_folders, pattern = "*\\.csv") #what  .csv files are in those folders?
    
    
    # create options that are the file names plus the number of samples that have that file name
    # df <- as.data.frame(table(reports))
    # report_options <- glue("{df$reports} ({df$Freq})")
    report_options <- unique(reports)
    
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
  
  observeEvent(input$go, {
    data_dir <- parseDirPath(c(home = home), input$directory) #root directory with all samples
    chosen_folders <- list.files(data_dir, full.names = TRUE)[list.files(data_dir) %in% input$sample_choice] #filter by choices from updating checkbox and get full paths
    full_paths <- paste0(chosen_folders, "/", input$report_choice)
    # print(full_paths)
    print(map(full_paths, read_IA) %>% set_names(basename(chosen_folders)) %>% bind_rows(.id = "sample"))
  })
}) 