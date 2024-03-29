# TODO:
# - What happens when some of the folders don't have a given .csv?  How to warn user of that or only show reports that are in all samples?  Two options: 1) only display reports if they are in all the selected samples (or alternatively have them greyed out if that's possible), 2) when you click 'import' just skip over folders where the report doesn't exist
# - would be nice if Q-value colorbar was the same across all pages of the plot
# - add an input for how many compounds to show per page of plot
# - add tests?
# - split peak detector? Check if RT is same as one of the two ends of the peak.


library(shiny)
library(shinyFiles)
library(tidyverse)
library(glue)
library(plotly)
library(DT)
library(naniar)

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
    output$diagnostic_table <- 
      DT::renderDataTable({
        diagnostic_df() %>% 
          mutate_if(is.numeric, ~round(., 4)) %>% 
          select(sample, no, compound, main_ion_m_z, rt, rt_exp, rt_window_st, rt_window_end, height, area = area_a_u_s, q_val, plot_page = page)
        },
                          # server=FALSE,
                          extensions = c("Buttons"), filter = "top",
                          options = list(
                            dom = "lBfrtip",
                            buttons = c("copy", "csv", "excel")
                          )
      )
    
    output$diagnostic_plot <- renderPlotly({
      j <- position_jitter(height = 0, width = 0.2)
      rt_df <- diagnostic_df() #only do this once. Maybe speeds it up?
      p <- ggplot(rt_df %>%
                    #only plot one page at a time
                    filter(page == input$page),
                  aes(y = rt_dev,
                      x = fct_inorder(compound_trunc),
                      color = q_val,
                      key = rownum, #necessary for brush selection to work
                      text = glue("File: {sample}
                            Compound: {compound_trunc}
                            RT: {round(rt, 3)}
                            RT expected: {round(rt_exp, 3)}
                            Q Value: {round(q_val, 3)}"))) +
        geom_miss_point(alpha = 0.5, position = j, prop_below = 0.05) +
        geom_linerange(aes(ymin = rt_dev_start, ymax = rt_dev_end, x = compound_trunc),
                       alpha = 0.4, position = j) +
        scale_color_viridis_c(option = "C", end = 0.95, limits = c(min(rt_df$q_val, na.rm = TRUE),
                                                       max(rt_df$q_val, na.rm = TRUE))) +
        coord_flip(ylim = c(-0.55, 0.5)) +
        labs(x = "deviation from expected RT", y = "(No.) Compound",
             color = "Q")
      ggplotly(p, tooltip = c("text")) %>% 
        layout(xaxis = list(fixedrange = TRUE))  #only allow zooming and panning along y-axis
    })
    
    observeEvent(diagnostic_df(), {
      updateNumericInput(session, "page", max = max(diagnostic_df()$page))
      # updateSliderInput(session, "page", max = max(diagnostic_df()$page))
    })
    
    # display data for selected points in table format
    output$brush <- DT::renderDataTable({
      d <- event_data("plotly_selected")
      diagnostic_df() %>%
        filter(rownum %in% d$key) %>% 
        select(sample, no. = no, compound, RT = rt, `Expected RT` = rt_exp, `RT sd` = rt_sd, q_val) %>% 
        mutate_if(is.numeric, ~round(., 3))
    },
    # server = FALSE, #Allows download of entire dataframe, not just what's visible in the browser.  However, not good for large data
    extensions = c("Buttons"),
    options = list(
      dom = "Brtip",
      buttons = c("copy", "csv", "excel"))
    )
    
    
    output$qtable <- DT::renderDataTable({
      diagnostic_df() %>% 
        select(sample, no, compound, q_val) %>%
        group_by(compound) %>% 
        mutate(`Mean Q Value` = mean(q_val, na.rm = TRUE)) %>% 
        filter(!is.na(q_val)) %>% 
        spread(key = sample, value = q_val) %>%
        arrange(`Mean Q Value`)
    },
    extensions = c("Buttons"), filter = "top",
    options = list(
      dom = "lBfrtip",
      buttons = c("copy", "csv", "excel")
    )
    )
    
    
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
                            fixedrange = TRUE))  #only allow zooming and panning along y-axis
      
    })
    
    output$widthtable <- DT::renderDataTable({
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
    },    
    # server = FALSE, #not good for large data
    extensions = c("Buttons"),
    options = list(
      dom = "lBrtip",
      buttons = c("copy", "csv", "excel")
    )
    )
    
    #TODO: Add import of ions from ion tab in method.  Then, look for match of any two(?) ions to further filter possible isomers.
    output$isomertable <- renderDataTable({
      #Find potential duplicate RTs
      tol = 0.001 #tolerance in RT. Could make this a reactive input in the future.
      diagnostic_df() %>% 
        arrange(sample, rt) %>% 
        mutate(rt_diff = rt - lag(rt), rt_diff2 = rt - lead(rt)) %>% 
        mutate(compound2 = case_when(rt_diff < tol ~ lag(compound),
                                     rt_diff2 > -tol ~ lead(compound)),
               no2 = case_when(rt_diff < tol ~ lag(no),
                               rt_diff2 > -tol ~ lead(no)),
               main_ion2 = case_when(rt_diff < tol ~ lag(main_ion_m_z),
                                     rt_diff2 > -tol ~ lead(main_ion_m_z)),
               rt2 = case_when(rt_diff < tol ~ lag(rt),
                               rt_diff2 > -tol ~ lead(rt)),
               rt_exp2 = case_when(rt_diff < tol ~lag(rt_exp),
                                   rt_diff2 > -tol ~lead(rt_exp))) %>% 
        dplyr::filter(rt_diff < tol | rt_diff2 > -tol) %>%
        select(sample, no, compound,"main ion (m/z)" = main_ion_m_z, "expected RT" = rt_exp, RT = rt, 
               "no. 2" = no2, "compound 2" = compound2, "main ion 2" = main_ion2, "RT 2" = rt2,
               "expected RT 2" = rt_exp2) %>%
        arrange(RT) %>%
        mutate_if(is.numeric, ~round(., 4))
    },
    # server = FALSE, #not good for large data
    extensions = c("Buttons"), filter = "top",
    options = list(
      dom = "lBfrtip",
      buttons = c("copy", "csv", "excel")
    )
    )
    
    
})