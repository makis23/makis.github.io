
# Shiny app created in the R language that displays AWFE set-comparison diagrams
#   for user-input data or for 2+ of 6 provided drug-target datasets
#
# This is the User Interface component of the app; this file returns the UI object.
#
# Known limitations of this app:
#   (a) The AWFE diagrams can take time to render if the input datasets are large.
#   (b) For diagrams comparing 5 or 6 datasets, the numerical labels on the plots can appear crowded.
#   (c) Currently, the 6 drug-target datasets provided with this app must be specified in the order indicated by their prefixes;
#           this is because references to the columns of these files are currently hard-coded in this script.
#   (d) Related to (c), this script currently cannot handle user-provided files, as references to individual files are hard-coded in this script.


# Load R libraries
library(shiny)       # Create interactive Shiny visualizations
library(Vennerable)  # Package to plot set comparison diagrams of various styles
library(tidyverse)   # Collection of packages for processing data
library(rowr)        # Row-based functions for R objects
library(shinydashboard)

#1.dashboardPage()
ui <- dashboardPage(
                #2.dashboardHeader()
  dashboardHeader(
                title="AWFE Diagrams Shiny App",
                #title width
                titleWidth = 270),  
                  #3.dashboardSidebar()
                  dashboardSidebar(
                    width = 270,
                    h5("Select variable & datasets to compare:"),
                    selectInput("comparisonSelector", "Select variable", choices = c("None",
                                                                                     "Drug Name",
                                                                                     "ChEMBL ID",
                                                                                     "DrugBank ID",
                                                                                     "Drug SMILES",
                                                                                     "Target"),selected = "Drug Name"),
                    #Horizontal line
                    tags$hr(),
                    
                    checkboxGroupInput("datasetSelector","Select the datasets to compare", choices = c("DB",
                                                                                                       "TC",
                                                                                                       "RH",
                                                                                                       "DGI",
                                                                                                       "DCH",
                                                                                                       "LDP"),selected = c("DB",
                                                                                                                           
                                                                                                                           "RH",
                                                                                                                           "DGI",
                                                                                                                           "DCH",
                                                                                                                           "LDP")),
                    #Horizontal line
                    tags$hr(),
                    
                    actionButton("plot","Apply Selections")
                  ),
                  #4.dashboardBody()
                  dashboardBody(
                    
                    h4(textOutput("titleplot")),
                    fluidRow(column(8,plotOutput("Plot"))),
                    br(),
                    h4(textOutput("Legend")),
                    htmlOutput("legend1"),
                    htmlOutput("legend2"),
                    htmlOutput("legend3"),
                    htmlOutput("legend4"),
                    htmlOutput("legend5"),
                    htmlOutput("legend6")
                    
                  )
                  
                
)
