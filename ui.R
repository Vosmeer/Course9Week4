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
      
      #shinythemes::themeSelector(),
      
      img(src='Mainformatics_1.png', height = 79, width = 187, align = "right"),
      
      # Input: Select a file --
      fileInput("file1", "Upload .CSV-bestand met data",
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
      checkboxInput("header", "Inclusief titel", TRUE),
      
      # Input: Decimaal  ----
      radioButtons("sep", "Kolom scheidingsteken",
                   choices = c(Puntkomma = ";",
                               Komma = ",",
                               Tab = "\t"),
                   selected = ";"),
     
       # Horizontal line ----
      tags$hr(),  
      
      # Input: Datums interval ----
      dateInput('Begindatum',
                label = 'Begindatum',
                value = Sys.Date()-3*365,
                format = "dd-mm-yyyy"
      ),
      dateInput('Einddatum',
                label = 'Einddatum',
                value = Sys.Date(),
                format = "dd-mm-yyyy"
      ),   
      # Horizontal line ----
      tags$hr(),
      
      # Input: Trendinterval ----
      radioButtons("JK_interval", "Trendinterval",
                   choices = c(Week = "Week",
                               Maand = 'Maand',
                               Kwartaal = "Kwartaal",
                               Jaar = "Jaar",
                               Compleet = "Compleet"),
                   selected = 'Compleet')


    ),
    
    # Main panel for displaying outputs ----
    mainPanel(


      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Gegevens", tableOutput("JK_data_upload")),
                  tabPanel("Samenvatting", tableOutput("samenvatting")),
                  tabPanel("Jackknife", plotOutput("plot_jk_tr"))
                 )
   )
  )
)
