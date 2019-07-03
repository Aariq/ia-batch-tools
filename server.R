# TODO:
# - What happens when some of the folders don't have a given .csv?  How to warn user of that or only show reports that are in all samples?  Two options: 1) only display reports if they are in all the selected samples (or alternatively have them greyed out if that's possible), 2) when you click 'import' just skip over folders where the report doesn't exist
# - share with Josh and ask him to try it
# - would be nice if Q-value colorbar was the same across all pages of the plot
# - add an input for how many compounds to show per page of plot
# - add tests?
# - split peak detector? Check if RT is same as one of the two ends of the peak.


library(shiny)
library(shinyFiles)
library(tidyverse)
library(glue)
library(plotly)

### Functions for reading in .csv's exported by Ion Analytics
parse_IA <- function(file){
  parsed_IA <- file %>%
    str_replace_all("^.+\r\n", "") %>% #remove first line
    str_replace_all("(?<!\r)\n", "") %>% #remove line breaks within headers (\n but now \r\n)
    str_replace_all("\r\n", "\n") #convert weird windows linebreaks (\r\n) to regular \n
  return(parsed_IA)
}

read_IA <- function(file){
  output <- readr::read_file(file) %>%
    parse_IA() %>%
    readr::read_csv()
  return(output)
}
###

shinyServer(function(input, output, session) {
  
  hideTab("tabs", "RT-Plot")
  hideTab("tabs", "Table")
  hideTab("tabs", "Q Value")
  hideTab("tabs", "Width")
  hideTab("tabs", "Isomer")
  
  roots = getVolumes(c("Preboot", "Recovery", "com.apple.TimeMachine.localsnapshots"))
  
  shinyDirChoose(input, "directory", roots = roots)
  
  # Save directory choice three ways:
  data_dir <- reactive({
    parseDirPath(roots, input$directory)
  })
  
  folders <- reactive({
    list.files(parseDirPath(roots, input$directory))
  })
  
  full_paths <- reactive({
    dir(parseDirPath(roots, input$directory), full.names = TRUE)
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
    showTab(inputId = "tabs", target = "RT-Plot")
    showTab("tabs", "Table")
    showTab(inputId = "tabs", target = "Q Value")
    showTab("tabs", "Width")
    showTab("tabs", "Isomer")
    
  })

    diagnostic_df <- reactive({
    df <- data() %>%
      janitor::clean_names() %>% 
      rename(rt = r_time_min, rt_exp = expect_min, rt_window_st = st_time_min,
             rt_window_end = end_time_min) %>% 
      #set zeroes to NAs
      mutate_if(is.double, ~ifelse(. == 0, NA, .)) %>% 
      #calculate deviations from expected RT
      mutate(rt_dev = rt - rt_exp,
             rt_dev_start = rt_window_st - rt_exp,
             rt_dev_end = rt_window_end - rt_exp) %>% 
      group_by(compound) %>% 
      #remove compounds that don't appear in any files
      filter(!all(is.na(rt))) %>%
      #calculate standard deviation within compound
      mutate(rt_sd = sd(rt, na.rm = TRUE)) %>% 
      ungroup() %>% 
      # reorder compounds based on their standard deviation so most problematic ones show up in first page
      mutate(no = as.factor(no) %>% fct_reorder(rt_sd, .desc = TRUE)) %>% 
      mutate(compound_trunc = glue("({no}) {str_trunc(compound, 15)}")) %>% 
      select(-type) #i don't know what the type column is even
    
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
    output$diagnostic_table <- renderDataTable({
      diagnostic_df()
    })
    
    output$diagnostic_plot <- renderPlotly({
      j <- position_jitter(height = 0.2, width = 0)
      p <- ggplot(diagnostic_df() %>%
                    #only plot one page at a time
                    filter(page == input$page),
                  aes(x = rt_dev,
                      y = compound_trunc,
                      color = q_val,
                      text = glue("File: {sample}
                            Compound: {compound_trunc}
                            RT: {round(rt, 3)}
                            RT expected: {round(rt_exp, 3)}
                            Q Value: {round(q_val, 3)}"))) +
        geom_point(alpha = 0.5, position = j) +
        geom_errorbarh(aes(xmin = rt_dev_start, xmax = rt_dev_end, y = compound_trunc),
                       alpha = 0.4, height = 0, width = 0, position = j) +
        scale_color_viridis_c(option = "C") +
        coord_cartesian(xlim = c(-0.5, 0.5)) +
        labs(x = "deviation from expected RT", y = "(No.) Compound",
             color = "Q")
      ggplotly(p, tooltip = c("text")) %>% 
        layout(xaxis = list(fixedrange = TRUE)) #only allow zooming and panning along y-axis
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
        select(sample, no. = no, compound, RT = rt, `Expected RT` = rt_exp, `RT sd` = rt_sd, q_val)
      # d
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
  
  
  output$qplot <- renderPlotly({
    p <- diagnostic_df() %>% 
      # select(sample, no, compound, q_val) %>% 
      group_by(compound) %>% 
      mutate(`Mean Q Value` = mean(q_val, na.rm = TRUE)) %>% 
      filter(!is.na(q_val)) %>% 
      ungroup() %>% 
      mutate(sample = fct_reorder(sample, q_val),
             compound_trunc = fct_reorder(compound_trunc, q_val)) %>% 
      arrange(`Mean Q Value`) %>%  
      # filter(`Mean Q Value` < 93) %>% #filter just to make there be fewer rows
      ggplot(aes(x = sample, y = compound_trunc, fill = q_val)) +
      geom_tile() +
      scale_fill_viridis_c(option = "C") +
      labs(x = "low <-- sample mean Q-value --> high",
           y = "low <-- compound mean Q-value --> high") #+
      # theme(axis.text.x = element_text(angle = 90))
      ggplotly(p) %>% 
        layout(xaxis = list(tickangle = -90,
                            fixedrange = TRUE)) #only allow zooming and panning along y-axis
  })
  
  output$widthtable <- renderDataTable({
    diagnostic_df() %>% 
      mutate_if(is.double, ~ifelse(. == 0, NA, .)) %>% 
      filter(!is.na(rt_window_end)) %>% 
      mutate(width = rt_window_end - rt_window_st) %>% 
      select(sample, no, compound, width) %>% 
      group_by(compound) %>% 
      mutate(mean_width = mean(width, na.rm = TRUE),
             sd_width = sd(width, na.rm = TRUE)) %>% 
      spread(key = sample, value = width) %>%
      arrange(desc(sd_width), compound)
  })

  output$isomertable <- renderDataTable({
    #Find potential duplicate RTs
    
    diagnostic_df() %>% 
      arrange(sample, rt) %>% 
      mutate(rt_diff = rt - lag(rt), rt_diff2 = rt - lead(rt)) %>% 
      mutate(possible_isomer = case_when(rt_diff < 0.005 ~ lag(compound),
                                         rt_diff2 > -0.005 ~ lead(compound))) %>% 
      dplyr::filter(rt_diff < 0.005 | rt_diff2 > -0.005) %>%
      select(sample, no, compound, "main ion (m/z)" = main_ion_m_z, "RT" = rt, "expected RT" = rt_exp, "also integrated as...?" = possible_isomer, "Q" = q_val, "start time" = rt_window_st, "end time" = rt_window_end) %>%
      arrange(RT)
  })
  
  
})