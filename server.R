# TODO:
# - What happens when some of the folders don't have a given .csv?  How to warn user of that or only show reports that are in all samples?  Two options: 1) only display reports if they are in all the selected samples (or alternatively have them greyed out if that's possible), 2) when you click 'import' just skip over folders where the report doesn't exist
# - put on web (eg shinyapps.io)
# - share with Nicole and ask her to try it
# - would be nice if Q-value colorbar was the same across all pages of the plot
# - add an input for how many compounds to show per page of plot


library(shiny)
library(shinyFiles)
library(tidyverse)
library(glue)
library(chemhelper)
library(plotly)
home = "~/Documents/ia-batch-tools" #for testing.  Change to "~" for production
# home = "~"
shinyServer(function(input, output, session) {
  
  hideTab("tabs", "RT")
  hideTab("tabs", "Q Value")
  hideTab("tabs", "Width")
  hideTab("tabs", "Isomer")
  
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
  
  # show tabs after hitting go
  observeEvent(input$go, {
    showTab(inputId = "tabs", target = "RT")
    showTab(inputId = "tabs", target = "Q Value")
    showTab("tabs", "Width")
    showTab("tabs", "Isomer")
    
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
  
  output$qtable <- renderDataTable({
    diagnostic_df() %>% 
      select(sample, no, compound, q_val) %>% 
      group_by(compound) %>% 
      mutate(`Mean Q Value` = mean(q_val, na.rm = TRUE)) %>% 
      filter(!is.na(q_val)) #%>% 
      # spread(key = compound, value = q_val) %>% 
      # arrange(`Mean Q Value`)
  })
  
  # Idea for visualization for Q-values:
  # a heat map with sample on the x-axis, sorted by average q-value (over all compounds) for that sample.  Compounds (with the lowest Q-values) on the y-axis, arranged by mean q-value for the compound.  That way shitty compounds and shitty samples would group together, but a unusually low q-value would stand out. Worth a try?
  
  # a hueristic diagram of how to read the plot.
  output$qplot_hueristic <- renderPlot({
    p <- ggplot() +
      geom_blank() + 
      annotate("segment", x = -1, xend = 1, y = 0, yend = 0, arrow = arrow(ends = "both")) +
      annotate("segment", x = 0, xend = 0, y = -1, yend = 1, arrow = arrow(ends = "both")) +
      annotate("text", x = 0.90, y = -0.08, label = "High Sample Q-Value") +
      annotate("text", x = -0.9, y = -0.08, label = "Low Sample Q-Value") +
      
      annotate("text", x = 0, y = 1.05, label = "High Compound Q-Value") +
      annotate("text", x = 0, y = -1.05, label = "Low Compound Q-Value") +
      labs(x = "Sample (arranged by mean Q-value across all compounds)",
           y = "Compound (arranged by mean Q-value across all samples)") +
      theme_void() + 
      theme(axis.title = element_text(size = 14),
            axis.title.y = element_text(angle = 90),
            # axis.line = element_line(),
            panel.background = element_rect(fill = "white", color = "black", size = 1))
    return(p)
  })
  
  output$qplot <- renderPlot({
    qtable %>%
      ungroup() %>% 
      mutate(sample = fct_reorder(sample, q_val),
             compound_trunc = fct_reorder(compound_trunc, q_val)) %>% 
      arrange(`Mean Q Value`) %>% 
      # add a filter so fewer compounds are shown.  Not sure how to do this exactly without dropping partial samples.
      ggplot(aes(x = sample, y = compound_trunc, fill = q_val)) +
      geom_tile() +
      scale_fill_viridis_c(option = "C") +
      labs(x = "low <-- sample mean Q-value --> high",
           y = "low <-- compound mean Q-value --> high")
  })
  
  output$widthtable <- renderDataTable({
    data() %>%
      janitor::clean_names() %>% 
      mutate_if(is.double, ~ifelse(. == 0, NA, .)) %>% 
      filter(!is.na(end_time_min)) %>% 
      mutate(width = end_time_min - st_time_min) %>% 
      select(sample, no, compound, width) %>% 
      group_by(compound) %>% 
      mutate(sd_width = sd(width, na.rm = TRUE)) %>% 
      # spread(key = sample, value = width) %>% 
      arrange(desc(sd_width), compound)
  })

  output$isomertable <- renderDataTable({
    #Find potential duplicate RTs
    
    diagnostic_df() %>% 
      arrange(sample, r_time_min) %>% 
      mutate(rt_diff = r_time_min - lag(r_time_min), rt_diff2 = r_time_min - lead(r_time_min)) %>% 
      mutate(possible_isomer = case_when(rt_diff < 0.005 ~ lag(compound),
                                         rt_diff2 > -0.005 ~ lead(compound))) %>% 
      dplyr::filter(rt_diff < 0.005 | rt_diff2 > -0.005) %>%
      select(sample, no, compound, "main ion (m/z)" = main_ion_m_z, "RT" = r_time_min, "expected RT" = expect_min, "also integrated as...?" = possible_isomer, "Q" = q_val, "start time" = st_time_min, "end time" = end_time_min) %>%
      arrange(RT)
  })
  
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
    # updateSliderInput(session, "page", max = max(diagnostic_df()$page))
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