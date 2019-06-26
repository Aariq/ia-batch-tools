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
                         h2("RT Consistency"),
                         p("Compounds sorted by standard deviation of retention time.  If you are inconsistent about choosing the correct peak, or if a compound has a naturally variable retention time, it will show up higher up and on earlier pages in these plots. Use the 'Page' input to look through all compounds. Hover over points to view details and select points using the selection tools on the plot to generate a table."),
                         plotlyOutput("diagnostic_plot"),
                         numericInput("page", "Page", min = 1, max = 10, value = 1, step = 1, width = "75px"),
                         # sliderInput("page", "Page", min = 1, max = 10, value = 1, step = 1),
                         p("Select datapoints above with one of the selection tools to populate this table"),
                         dataTableOutput("brush")),
                
                #this tab would have a simple table of compounds sorted by mean Q-value
                tabPanel("Q Value",
                         # h2("Q Values"),
                         # p("This table shows mean Q value for each compound across samples along with the Q value for each compound detected in each sample. You can also sort by other columns by clicking on them.  For example, you may spot individual sample/compound combinations with very low Q-values when sorting by q_val"),
                         # dataTableOutput("qtable"),
                         h2("Idea for visualization for Q-values:"),
                         plotOutput("qplot", height = "600px"),
                         p("A heat map with sample on the x-axis, sorted by average q-value [over all compounds] for that sample.  Compounds [with the lowest Q-values] on the y-axis, arranged by mean q-value for the compound.  That way shitty compounds and shitty samples would group together, but a unusually low q-value would stand out. Worth a try?")),
                
                #this tab would have some diagnostics about peak width?
                tabPanel("Width",
                         h2("Peak Width Consistency"),
                         p("Compound peak widths sorted by standard deviation of peak width.  This is helpful for spotting inconsistency in manual integration of broad peaks or peaks that are sometimes cut off by overlap with another compound"),
                         dataTableOutput("widthtable")),
                
                #This tab aims to detect problems with isomers
                tabPanel("Isomer",
                         h2("Same peak, multiple compounds?"),
                         p("This table looks for compounds that have a very small difference in retention time from other compounds within the same sample.  That could indicate that IonAnalytics is integrating the same peak as two different compounds.  This often happens when isomers have similar MS signatures and IonAnalytics picks the same peak for all isomers.  After a problematic peak is identified, the IonAnalytics method can be edited to narrow the retention time window based on the start and end times of the correct hit."),
                         dataTableOutput("isomertable")))
                  
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