library(shiny)
library(shinyFiles)
library(tidyverse)
home = "~/Documents/ia-batch-tools" #for testing.  Change to "~" for production
shinyServer(function(input, output, session) {
  
  shinyDirChoose(input, "directory", roots = c(home = home))
  #prints to console
  observe({
    cat("\ninput$directory value:\n\n")
    print(input$directory)
  })
  #sends to UI
  output$directorypath <- renderPrint({
    list.files(parseDirPath(c(home = home), input$directory))
  })
  
  # display directories as a checkbox selction with all selected by default
  observe({
    #get folder names
    folders <- list.files(parseDirPath(c(home = home), input$directory))
    # print(folders)
    
    #  makes it have no choices until you select a directory
    if (is.null(folders))
      folders <- character(0)
    
    # Updates checkbox with report options
    updateCheckboxGroupInput(session, "inCheckboxGroup1",
                             label = "Choose Samples",
                             choices = folders,
                             selected = folders)
  })
  
  # gets names of .csv files at tips of directory and updates a checkbox selection
  observe({
    #get the .csv file names
    ##** MAKE THIS UPDATE BASED ON EXCLUDED SAMPLES **##
    x <- list.files(parseDirPath(c(home = home), input$directory),
                    recursive = TRUE,
                    pattern = "*\\.csv") %>% 
      basename()
    
    # create options that are the file names plus the number of samples that have that file name
    df <- as.data.frame(table(x))
    
    options <- glue("{df$x} ({df$Freq})")
    
    #  makes it have no choices until you select a directory
    if (is.null(x))
      x <- character(0)
    
    # Updates checkbox with report options
    updateCheckboxGroupInput(session, "inCheckboxGroup2",
                             label = "Choose a report",
                             choices = options,
                             selected = NULL
    )
  })
}) 