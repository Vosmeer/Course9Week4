## Script setup

library(ggrepel)
library(grid)
library(openxlsx)
library(shiny)
library(shinythemes)
library(clipr)
library(dplyr)
library(stringr)


# Define UI for data upload app ----
ui <- fluidPage(
  
  tags$style("
              body {
    -moz-transform: scale(0.9, 0.9); /* Moz-browsers */
    zoom: 0.9; /* Other non-webkit browsers */
    zoom: 90%; /* Webkit browsers */
  }"),
  
  # App title ----
  titlePanel("Jackknife generator"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a file --
      fileInput("file1", "Upload .CSV-file with data",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Button
      downloadButton("Download_Uploadsheet_Jackknife.csv", "Download Uploadsheet"),

      
      # A select input for selecting subset
      downloadButton("Jackknife.png", "Download Jackknife plot"),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox wel of geen titel ----
      checkboxInput("header", "Including title", TRUE),
      
      # Input: Decimaal  ----
      radioButtons("sep", "Column separator",
                   choices = c(Semicolon = ";",
                               Comma = ",",
                               Tab = "\t"),
                   selected = ";"),
     
       # Horizontal line ----
      tags$hr(),  
      
      # Input: Datums interval ----
      dateInput('Begindatum',
                label = 'Start date',
                value = Sys.Date()-3*365,
                format = "dd-mm-yyyy"
      ),
      dateInput('Einddatum',
                label = 'End date',
                value = Sys.Date(),
                format = "dd-mm-yyyy"
      ),   
      # Horizontal line ----
      tags$hr(),
      
      # Input: Trendinterval ----
      radioButtons("JK_interval", "Trendinterval",
                   choices = c(Week = "Week",
                               Month = 'Maand',
                               Quarter = "Kwartaal",
                               Year = "Jaar",
                               Complete = "Compleet"),
                   selected = 'Compleet')


    ),
    
    # Main panel for displaying outputs ----
    mainPanel(


      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Data", tableOutput("JK_data_upload")),
                  tabPanel("Summary", tableOutput("samenvatting")),
                  tabPanel("Jackknife", plotOutput("plot_jk_tr")),
                  tabPanel("Manual", htmlOutput("manual"))
                 )
   )
  )
)
