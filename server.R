# TODO:
# - What happens when some of the folders don't have a given .csv?  How to warn user of that or only show reports that are in all samples?  Two options: 1) only display reports if they are in all the selected samples (or alternatively have them greyed out if that's possible), 2) when you click 'import' just skip over folders where the report doesn't exist
# - Add text explaining what shit does
# - put on web (eg shinyapps.io)
# - share with Nicole and ask her to try it
# - would be nice if Q-value colorbar was the same across all pages of the plot
# - add an input for how many compounds to show per page of plot
# - put things in tabs


library(shiny)
library(shinyFiles)
library(tidyverse)
library(glue)
library(chemhelper)
library(plotly)
home = "~/Documents/ia-batch-tools" #for testing.  Change to "~" for production
# home = "~"
shinyServer(function(input, output, session) {
  
  shinyDirChoose(input, "directory", roots = c(home = home))
  
  #sends to UI for debugging
  # output$directorypath <- renderPrint({
  #   full_paths <- dir(parseDirPath(c(home = home), input$directory), full.names = TRUE)
  #   full_paths
  # })
  
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
    df <- as.data.frame(table(reports()))
    report_options <- unique(reports()) %>% set_names(glue("{df[[1]]} ({df$Freq})"))
    
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
  
  diagnostic_df <- reactive({
    df <- data() %>%
      janitor::clean_names() %>% 
      #set zeroes to NAs
      mutate_if(is.double, ~ifelse(. == 0, NA, .)) %>% 
      #calculate deviations from expected RT
      mutate(rt_dev = r_time_min - expect_min) %>% 
      group_by(compound) %>% 
      #remove compounds that don't appear in any files
      filter(!all(is.na(r_time_min))) %>%
      #calculate standard deviation within compound
      mutate(rt_sd = sd(r_time_min, na.rm = TRUE)) %>% 
      ungroup() %>% 
      # reorder compounds based on their standard deviation so most problematic ones show up in first page
      mutate(no = as.factor(no) %>% fct_reorder(rt_sd, .desc = TRUE)) %>% 
      mutate(compound_trunc = glue("({no}) {str_trunc(compound, 15)}"))
    
    nperpage <- 20 #make input for this later?
    #figure out pages for plots
    lvls <-
      #get unique levels of compound number in order
      tibble(no = fct_unique(fct_drop(df$no))) %>%
      #add a column that puts them in groups of nperpage (currently 20)
      mutate(page = rep(1:ceiling(n()/nperpage), each = nperpage, length.out = n()))
    #join that to the original data so there is a grouping variable called "page"
    full_join(df, lvls) %>%
      arrange(desc(rt_sd)) %>% 
      mutate(rownum = row_number())
  })
  
  # output$test <- renderDataTable({
  #   head(test())
  # })
  
  output$diagnostic_plot <- renderPlotly({
    p <- ggplot(diagnostic_df() %>%
             #only plot one page at a time
             filter(page == input$page),
           aes(x = rt_dev,
               # y = str_trunc(compound, width = 15),
               y = compound_trunc,
               label = sample,
               key = rownum,
               color = q_val)) +
      geom_jitter(alpha = 0.5, width = 0, height = 0.2) +
      scale_color_viridis_c(option = "C") +
      coord_cartesian(xlim = c(-0.5, 0.5)) +
      labs(x = "deviation from expected RT", y = "(No.) Compound",
           # title = "Compounds sorted by standard deviation of retention time",
           color = "Q")
      ggplotly(p)
  })
  
  observeEvent(diagnostic_df(), {
    updateNumericInput(session, "page", max = max(diagnostic_df()$page))
  })
  
  # display data for selected points in table format
  output$brush <- renderDataTable({
    d <- event_data("plotly_selected")
    diagnostic_df() %>%
      filter(rownum %in% d$key) %>% 
      select(sample, no. = no, compound, RT = r_time_min, `Expected RT` = expect_min, `RT sd` = rt_sd, q_val)
    # d
  })
  
})