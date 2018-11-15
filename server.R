
# Shiny app created in the R language that displays AWFE set-comparison diagrams
#   for user-input data or for 2+ of 6 provided drug-target datasets
#
# This is the Server component of the app; this file returns the server function.
#
# Known limitations of this app:
#   (a) The AWFE diagrams can take time to render if the input datasets are large.
#   (b) For diagrams comparing 5 or 6 datasets, the numerical labels on the plots can appear crowded.
#   (c) Currently, the 6 drug-target datasets provided with this app must be specified in the order indicated by their prefixes;
#           this is because references to the columns of these files are currently hard-coded in this script.
#   (d) Related to (c), this script currently cannot handle user-provided files, as references to individual files are hard-coded in this script.



# Load R libraries
library(shiny)        # Create interactive Shiny visualizations
library(Vennerable)   # Package to plot set comparison diagrams of various styles
library(tidyverse)    # Collection of packages for processing data
library(rowr)         # Row-based functions for R objects
library(shinydashboard)

server <- function(input, output, session){
  
  # Process the uploaded files
  
  F1Final <- reactive({
    # Instead of reading the dataframe from the fileInput() we instantly load it with its name from our working directory
    rawData = read.csv("1_DB_Subset_v1_ConsistentHeaders.csv", stringsAsFactors = FALSE)
    if(!is.null(rawData)) {
      rawDF1 <- (read.csv("1_DB_Subset_v1_ConsistentHeaders.csv", stringsAsFactors = FALSE))
      rawDF1 <- rawDF1[,c(1,2,3,4,6)]
      F1DrugName <- unique(rawDF1[,1])
      F1ChEMBL <- unique(rawDF1[,2])
      F1DrugBank <- unique(rawDF1[,3])
      F1DrugSMILES <- unique(rawDF1[,4])
      F1Target <- unique(rawDF1[,5])
      F1Final <- as.data.frame(cbind.fill(F1DrugName,F1ChEMBL,F1DrugBank,F1DrugSMILES,F1Target ,fill = NA),col.names = c("F1DrugName", 
                                                                                                                         "F1ChEMBL",
                                                                                                                         "F1DrugBank",
                                                                                                                         "F1DrugSMILES",
                                                                                                                         "F1Target"))
    } else {
      return(NULL);
    }
    
  })
  F2Final <- reactive({
    # Instead of reading the dataframe from the fileInput() we instantly load it with its name from our working directory
    
    rawData = read.csv("2_TC_Subset_v1_ConsistentHeaders.csv", stringsAsFactors = FALSE)
    if(!is.null(rawData)) {
      rawDF2 <- (read.csv("2_TC_Subset_v1_ConsistentHeaders.csv", stringsAsFactors = FALSE))
      rawDF2 <- rawDF2[,c(1,2,3,4,6)]
      F2DrugName <- unique(rawDF2[,1])
      F2ChEMBL <- unique(rawDF2[,2])
      F2DrugBank <- unique(rawDF2[,3])
      F2DrugSMILES <- unique(rawDF2[,4])
      F2Target <- unique(rawDF2[,5])
      F2Final <- as.data.frame(cbind.fill(F2DrugName,F2ChEMBL,F2DrugBank,F2DrugSMILES,F2Target ,fill = NA),col.names = c("F2DrugName", 
                                                                                                                         "F2ChEMBL",
                                                                                                                         "F2DrugBank",
                                                                                                                         "F2DrugSMILES",
                                                                                                                         "F2Target"))
    } else {
      return(NULL);
    }
    
  })  
  F3Final <- reactive({
    # Instead of reading the dataframe from the fileInput() we instantly load it with its name from our working directory
    
    rawData = read.csv("3_RH_Subset_v1_ConsistentHeaders.csv", stringsAsFactors = FALSE)
    if(!is.null(rawData)) {
      rawDF3 <- (read.csv("3_RH_Subset_v1_ConsistentHeaders.csv", stringsAsFactors = FALSE))
      rawDF3 <- rawDF3[,c(1,3,4,5,6)]
      F3DrugName <- unique(rawDF3[,1])
      F3ChEMBL <- unique(rawDF3[,2])
      F3DrugBank <- unique(rawDF3[,3])
      F3DrugSMILES <- unique(rawDF3[,4])
      F3Target <- unique(rawDF3[,5])
      F3Final <- as.data.frame(cbind.fill(F3DrugName,F3ChEMBL,F3DrugBank,F3DrugSMILES,F3Target ,fill = NA),col.names = c("F3DrugName", 
                                                                                                                         "F3ChEMBL",
                                                                                                                         "F3DrugBank",
                                                                                                                         "F3DrugSMILES",
                                                                                                                         "F3Target"))
    } else {
      return(NULL);
    }
    
  })
  F4Final <- reactive({
    # Instead of reading the dataframe from the fileInput() we instantly load it with its name from our working directory
    
    rawData = read.csv("4_DGI_Subset_v1_ConsistentHeaders.csv", stringsAsFactors = FALSE)
    if(!is.null(rawData)) {
      rawDF4 <- read.csv("4_DGI_Subset_v1_ConsistentHeaders.csv", stringsAsFactors = FALSE)
      rawDF4 <- rawDF4[,c(1,2,3,4,5)]
      F4DrugName <- unique(rawDF4[,1])
      F4ChEMBL <- unique(rawDF4[,2])
      F4DrugBank <- unique(rawDF4[,3])
      F4DrugSMILES<- unique(rawDF4[,4])
      F4Target <- unique(rawDF4[,5])
      F4Final <- as.data.frame(cbind.fill(F4DrugName,F4ChEMBL,F4DrugBank,F4DrugSMILES,F4Target ,fill = NA),col.names = c("F4DrugName", 
                                                                                                                         "F4ChEMBL",
                                                                                                                         "F4DrugBank",
                                                                                                                         "F4DrugSMILES",
                                                                                                                         "F4Target"))
    } else {
      return(NULL);
    }
    
  })
  F5Final <- reactive({
    # Instead of reading the dataframe from the fileInput() we instantly load it with its name from our working directory
    
    rawData = read.csv("5_DCH_Subset_v1_ConsistentHeaders.csv", stringsAsFactors = FALSE)
    if(!is.null(rawData)) {
      rawDF5 <- read.csv("5_DCH_Subset_v1_ConsistentHeaders.csv", stringsAsFactors = FALSE)
      rawDF5 <- rawDF5[,c(1,2,3,4,7)]
      F5DrugName <- unique(rawDF5[,1])
      F5ChEMBL <- unique(rawDF5[,2])
      F5DrugBank <- unique(rawDF5[,3])
      F5DrugSMILES<- unique(rawDF5[,4])
      F5Target <- unique(rawDF5[,5])
      F5Final <- as.data.frame(cbind.fill(F5DrugName,F5ChEMBL,F5DrugBank,F5DrugSMILES,F5Target ,fill = NA),col.names = c("F5DrugName", 
                                                                                                                         "F5ChEMBL",
                                                                                                                         "F5DrugBank",
                                                                                                                         "F5DrugSMILES",
                                                                                                                         "F5Target"))
    } else {
      return(NULL);
    }
    
  })
  F6Final <- reactive({
    # Instead of reading the dataframe from the fileInput() we instantly load it with its name from our working directory
    
    rawData = read.csv("6_LDP_Subset_v1_ConsistentHeaders.csv", stringsAsFactors = FALSE)
    if(!is.null(rawData)) {
      rawDF6 <-read.csv("6_LDP_Subset_v1_ConsistentHeaders.csv", stringsAsFactors = FALSE) 
      rawDF6 <- rawDF6[,c(1,3,4,6,7)]
      F6DrugName <- unique(rawDF6[,1])
      F6ChEMBL <- unique(rawDF6[,2])
      F6DrugBank <- unique(rawDF6[,3])
      F6DrugSMILES<- unique(rawDF6[,4])
      F6Target <- unique(rawDF6[,5])
      F6Final <- as.data.frame(cbind.fill(F6DrugName,F6ChEMBL,F6DrugBank,F6DrugSMILES,F6Target ,fill = NA),col.names = c("F6DrugName", 
                                                                                                                         "F6ChEMBL",
                                                                                                                         "F6DrugBank",
                                                                                                                         "F6DrugSMILES",
                                                                                                                         "F6Target"))
    } else {
      return(NULL);
    }
    
  })
  
  
  # Switches to indicate which datasets have been selected
  
  DF1 <- reactive({ 
    if("DB" %in% input$datasetSelector){
      F1Final()
    }
    else{
      NULL
    }
  })
  DF2 <- reactive({ 
    if("TC" %in% input$datasetSelector){
      F2Final()
    }
    else{
      NULL
    }
  })
  DF3 <- reactive({ 
    if("RH" %in% input$datasetSelector){
      F3Final()   
    }
    else{
      NULL
    }
  })
  DF4 <- reactive({ 
    if("DGI" %in% input$datasetSelector){
      F4Final()   
    }
    else{
      NULL
    }
  })
  DF5 <- reactive({ 
    if("DCH" %in% input$datasetSelector){
      F5Final()
    }
    else{
      NULL
    }
  })
  DF6 <- reactive({ 
    if("LDP" %in% input$datasetSelector){
      F6Final()  
    }
    else{
      NULL
    }
  })
  
  
  # Create lists that are updated when checkboxes are selected/unselected

  DrugsList <- reactive({
    
    DrugsList <-list(DF1()[,1],DF2()[,1],DF3()[,1],DF4()[,1],DF5()[,1],DF6()[,1])
    
    names(DrugsList) <- c(paste(input$datasetSelector[1]),
                          paste(input$datasetSelector[2]),
                          paste(input$datasetSelector[3]),
                          paste(input$datasetSelector[4]),
                          paste(input$datasetSelector[5]),
                          paste(input$datasetSelector[6]))
    
    Filter(function(x) ! is.null(x), DrugsList)
  })
  
  ChEMBLList <- reactive({
    
    ChEMBLList <-list(DF1()[,2],DF2()[,2],DF3()[,2],DF4()[,2],DF5()[,2],DF6()[,2])
    
    names(ChEMBLList) <- c(paste(input$datasetSelector[1]),
                          paste(input$datasetSelector[2]),
                          paste(input$datasetSelector[3]),
                          paste(input$datasetSelector[4]),
                          paste(input$datasetSelector[5]),
                          paste(input$datasetSelector[6]))
    
    Filter(function(x) ! is.null(x), ChEMBLList)
  })
  
  DrugBankList <- reactive({
    
    DrugBankList <-list(DF1()[,3],DF2()[,3],DF3()[,3],DF4()[,3],DF5()[,3],DF6()[,3])
    
    names(DrugBankList) <- c(paste(input$datasetSelector[1]),
                          paste(input$datasetSelector[2]),
                          paste(input$datasetSelector[3]),
                          paste(input$datasetSelector[4]),
                          paste(input$datasetSelector[5]),
                          paste(input$datasetSelector[6]))
    
    Filter(function(x) ! is.null(x), DrugBankList)
  })
  
  DrugSMILESList <- reactive({
    
    DrugSMILESList <-list(DF1()[,4],DF2()[,4],DF3()[,4],DF4()[,4],DF5()[,4],DF6()[,4])
    
    names(DrugSMILESList) <- c(paste(input$datasetSelector[1]),
                          paste(input$datasetSelector[2]),
                          paste(input$datasetSelector[3]),
                          paste(input$datasetSelector[4]),
                          paste(input$datasetSelector[5]),
                          paste(input$datasetSelector[6]))
    
    Filter(function(x) ! is.null(x), DrugSMILESList)
  })
  
  TargetList <- reactive({
    
    TargetList <- list(DF1()[,5],DF2()[,5],DF3()[,5],DF4()[,5],DF5()[,5],DF6()[,5])
    
    names(TargetList) <- c(paste(input$datasetSelector[1]),
                           paste(input$datasetSelector[2]),
                           paste(input$datasetSelector[3]),
                           paste(input$datasetSelector[4]),
                           paste(input$datasetSelector[5]),
                           paste(input$datasetSelector[6]))
    
    Filter(function(x) ! is.null(x), TargetList)
  })
  
  
  # Handle button action
  
  DrugCR <- reactive({
    Venn(DrugsList())
  }) 
  ChEMBLCR <- reactive({
    Venn(ChEMBLList())
  }) 
  DrugBankListCR <- reactive({
    Venn(DrugBankList())
  }) 
  DrugSmilesCR <- reactive({
    Venn(DrugSMILESList())
  }) 
  TargetListCR <- reactive({
    Venn(TargetList())
  }) 
  
  
  # Create the AWFE plot according to the user's specifications
  
  output$Plot <- renderPlot({
    input$plot
    if(isolate(input$comparisonSelector == "Drug Name")){
      plot(isolate(DrugCR()), type= "AWFE", show = list(SetLabels = FALSE, Faces = FALSE))
    }
    
    if(isolate(input$comparisonSelector == "ChEMBL ID")){
      plot(isolate(ChEMBLCR()),type= "AWFE", show = list(SetLabels = FALSE, Faces = FALSE))
    }
    
    if(isolate(input$comparisonSelector == "DrugBank ID")){
      plot(isolate(DrugBankListCR()),type= "AWFE", show = list(SetLabels = FALSE, Faces = FALSE))
    }
    
    if(isolate(input$comparisonSelector == "Drug SMILES")){
     plot(isolate(DrugSmilesCR()),type= "AWFE", show = list(SetLabels = FALSE, Faces = FALSE))
    }
    
    if(isolate(input$comparisonSelector == "Target")){
      plot(isolate(TargetListCR()),type= "AWFE", show = list(SetLabels = FALSE, Faces = FALSE))
    }
  })
  
  
  # Specify the plot title
  
  output$titleplot <- renderText({
    input$plot
    if(isolate(len(DrugsList()) >= 2) && isolate(input$comparisonSelector == "Drug Name")){
      paste0("Compare sets of Drug Names using an AWFE diagram")
    }
    else if(isolate(len(DrugsList()) >= 2 )&& isolate(input$comparisonSelector == "Target")){
      paste0("Compare sets of Targets using an AWFE diagram")
    }
    else if(isolate(len(DrugsList()) >= 2 )&& isolate(input$comparisonSelector == "DrugBank ID" )){
      paste0("Compare sets of DrugBank IDs using an AWFE diagram")
    }
    else if(isolate(len(DrugsList()) >= 2 )&& isolate(input$comparisonSelector == "ChEMBL ID" )){
      paste0("Compare sets of ChEMBL IDs using an AWFE diagram")
    }
    else if(isolate(len(DrugsList()) >= 2 )&& isolate(input$comparisonSelector == "Drug SMILES" )){
      paste0("Compare sets of Drug SMILES strings using an AWFE diagram")
    }
  })
  
  
  # Specify the legend
  
  output$Legend<- renderText({
    input$plot
    if(isolate(len(DrugsList()) >= 2 )&& isolate(input$comparisonSelector != "None" )&& isolate(len(input$datasetSelector ) >= 2)){
      "Legend" 
    }
  })
  output$legend1 <- renderText({
    input$plot
    if(isolate(len(DrugsList()) >= 2 )&& isolate(input$comparisonSelector != "None" )&& isolate(len(input$datasetSelector ) >= 2)){
      paste0("<font color=\"#e41a1c\"><b>",isolate(input$datasetSelector[1]))
    }
  })
  output$legend2 <- renderText({
    input$plot
    if(isolate(len(DrugsList()) >= 2 )&&isolate( input$comparisonSelector != "None" )&& isolate(len(input$datasetSelector ) >= 2)){
      paste0("<font color=\"#377eb8\"><b>",isolate(input$datasetSelector[2]))
    }
  })
  output$legend3 <- renderText({
    input$plot
    if(isolate(len(DrugsList()) >= 3 )&& isolate(input$comparisonSelector != "None" )&& isolate(len(input$datasetSelector ) >= 3)){
      paste0("<font color=\"#4daf4a\"><b>",isolate(input$datasetSelector[3]))
    }
  })
  output$legend4 <- renderText({
    input$plot
    if(isolate(len(DrugsList()) >= 4 )&&isolate( input$comparisonSelector != "None") &&isolate( len(input$datasetSelector ) >= 4)){
      paste0("<font color=\"#984ea3\"><b>",isolate(input$datasetSelector[4]))
    }
  })
  output$legend5 <- renderText({
    input$plot
    if(isolate(len(DrugsList()) >= 5) && isolate(input$comparisonSelector != "None" )&& isolate( len(input$datasetSelector ) >= 5)){
      paste0("<font color=\"#ff7f00\"><b>",isolate(input$datasetSelector[5]))
    }
  })
  output$legend6 <- renderText({
    input$plot
    if(isolate(len(DrugsList()) >= 6 )&&isolate( input$comparisonSelector != "None") && isolate(len(input$datasetSelector ) >= 6)){
      paste0("<font color=\"#ffff00\"><b>",isolate(input$datasetSelector[6]))
    }
  })
  
}
