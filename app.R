library(shiny)
library(bulletr)
library(xml2)
library(shinyjs)
library(lubridate)
library(utils)

# Set the app directory as the current working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
appdir<-getwd()

# Read in the default template from the app folder
a1<- xml2::read_xml("./defaultTemplateXML.xml")

# Convert the XML file into a list
input.info<- as_list(a1)

# Initialization: Generate the *.info lists from the template
feature.info = input.info$Record1
general.info = input.info$Record2
matrix.info = input.info$Record3

# This variable is called while using the reset button to reset the page
# uses package::shinjs 
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

######################################################################################
#####################################################################################
# This function saves the information provided in the default template, which will be loaded the next time
setdefault<- function(a = FALSE,b = FALSE, c){
  if (a == TRUE){
    general.info<- c$general.info
    record2.assign(a1, general.info)
  }
  if (b == TRUE){
    feature.info<- c$feature.info
    record1.assign(a1, feature.info)
  }
}

# Reading X3P and dat files
loadData <- function(name, file1, profiley) {
  
  if(substrRight(name,3)[1] == "dat"){
    data1 <- read_dat(path = file1, profiley = profiley)
    return(data1)
  }
  
  if(substrRight(name,3)[1] == "x3p"){
    read_x3p(path = file1, profiley = profiley)
  }
}

# Finding the extension of the file read in
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Function for assigning General Information to the XML file (used while setting defaults)
record2.assign<- function(a1, record2.data){
  
  xml_set_text(xml_child(a1, search = "Record2/Date"), as.character(record2.data$Date) )
  xml_set_text(xml_child(a1, search = "Record2/Creator"), as.character(record2.data$Creator) )
  xml_set_text(xml_child(a1, search = "Record2/Instrument/Manufacturer"), as.character(record2.data$Instrument$Manufacturer) )
  xml_set_text(xml_child(a1, search = "Record2/Instrument/Model"), as.character(record2.data$Instrument$Model))
  xml_set_text(xml_child(a1, search = "Record2/Instrument/Serial"), as.character(record2.data$Instrument$Serial) )
  xml_set_text(xml_child(a1, search = "Record2/Instrument/Version"), as.character(record2.data$Instrument$Version) )
  xml_set_text(xml_child(a1, search = "Record2/CalibrationDate"), as.character(record2.data$CalibrationDate) )
  xml_set_text(xml_child(a1, search = "Record2/ProbingSystem/Type"), as.character(record2.data$ProbingSystem$Type))
  xml_set_text(xml_child(a1, search = "Record2/ProbingSystem/Identification"), as.character(record2.data$ProbingSystem$Identification))
  xml_set_text(xml_child(a1, search = "Record2/Comment"), as.character(record2.data$Comment))
  
}

# Function for assigning Feature Information to the XML file (used while setting defaults)
record1.assign<- function(a1, record1.data){
  
  xml_set_text(xml_child(a1, search = "Record1/Revision"), as.character(record1.data$Revision) )
  xml_set_text(xml_child(a1, search = "Record1/FeatureType"), as.character(record1.data$FeatureType) )
  xml_set_text(xml_child(a1, search = "Record1/Axes/CX/AxisType"), as.character(record1.data$Axes$CX$AxisType) )
  xml_set_text(xml_child(a1, search = "Record1/Axes/CX/DataType"), as.character(record1.data$Axes$CX$DataType) )
  xml_set_text(xml_child(a1, search = "Record1/Axes/CX/Increment"), as.character(record1.data$Axes$CX$Increment) )
  xml_set_text(xml_child(a1, search = "Record1/Axes/CX/Offset"), as.character(record1.data$Axes$CX$Offset) )
  xml_set_text(xml_child(a1, search = "Record1/Axes/CY/AxisType"), as.character(record1.data$Axes$CY$AxisType) )
  xml_set_text(xml_child(a1, search = "Record1/Axes/CY/DataType"), as.character(record1.data$Axes$CY$DataType) )
  xml_set_text(xml_child(a1, search = "Record1/Axes/CY/Increment"), as.character(record1.data$Axes$CY$Increment) )
  xml_set_text(xml_child(a1, search = "Record1/Axes/CY/Offset"), as.character(record1.data$Axes$CY$Offset) )
  xml_set_text(xml_child(a1, search = "Record1/Axes/CZ/AxisType"), as.character(record1.data$Axes$CY$AxisType) )
  xml_set_text(xml_child(a1, search = "Record1/Axes/CZ/DataType"), as.character(record1.data$Axes$CZ$DataType) )
  xml_set_text(xml_child(a1, search = "Record1/Axes/CZ/Increment"), as.character(record1.data$Axes$CZ$Increment) )
  xml_set_text(xml_child(a1, search = "Record1/Axes/CZ/Offset"), as.character(record1.data$Axes$CZ$Offset) )
}

#################################################################################
#################################################################################

# Shiny app for the user can read and write data for
ui <- fluidPage(
  titlePanel("Read and Write x3p objects"),
  sidebarLayout(
    # Setting the layout of the side panel
    sidebarPanel(
      radioButtons("mode", "Select Write mode", c("Single file" = "singfile",
                                                  "Batch" = "batch")),
      checkboxInput("profiley", "Do you need Profile Adjustment? Check for Yes"),
      selectInput("changeinfo", "Do you wish to Change any information?", c("No, Keep the defaults", "Yes, I want to make changes")),
      helpText("Note: Please note the Matrix Dimensions",
               "are automatically detected and changing these might alter",
               "the surface matrix"),
      radioButtons("ftype", "Select File type", c("dat files" = "datf",
                                                  "x3p files" = "x3pf")),
      #actionButton("folderget", "Browse"),
      textOutput("text1"),
      conditionalPanel( 
        condition = "input.changeinfo == 'Yes, I want to make changes'",
        selectInput("information", "Information",
                    c("Header Info (Automatic)","General Information", "Feature Information", "Matrix Information"))
      ),
      
      conditionalPanel( 
        condition = "input.mode == 'singfile'",
        
        #fileInput('file1', 'Choose dat or x3p file',
        #          accept=c('dat', 'x3p')),
        actionButton("fileget", "Browse"),
        helpText("Please wait for the Loading complete sign to apear before prooceeding"),
        helpText("Note: Houston data sets have Profile = FALSE",
                 "while Phoenix data sets have Profile = TRUE"),
        actionButton("write", "Write File"),
        actionButton("save_template", "Save Template")
        # actionButton("folderget", "Get it")
      ),# bracket for conditional section
      conditionalPanel( 
        condition = "input.mode == 'batch'",
        # radioButtons("ftype", "Select File type", c("dat files" = "datf",
        #                                             "x3p files" = "x3pf")),
        actionButton("folderget", "Browse"),
        actionButton("writebatch", "Batch Write")
      ),
      # This includes shinyjs in the UI
      useShinyjs(),
      # This adds the js code to the page
      extendShinyjs(text = jsResetCode),
      actionButton("reset_button", "Reset Page")
      
    ), # bracket for sidepanel
    mainPanel(
      # Setting up the text boxes in the main panel for the form layout
      conditionalPanel( 
        condition = "input.information == 'Header Info (Automatic)'",
        #textOutput("text3")
        htmlOutput("text3")
      ),
      conditionalPanel( 
        condition = "input.information == 'General Information'",
        textInput("date", "Date", general.info$Date),
        textOutput("text2"), # Shows system date for covenience of updating
        textInput("general.info$Creator", "Creator", general.info$Creator),
        textInput("general.info$Instrument$Manufacturer", "Manufacturer", general.info$Instrument$Manufacturer),
        textInput("general.info$Instrument$Model", "Model", general.info$Instrument$Model),
        textInput("general.info$Instrument$Serial", "Serial", general.info$Instrument$Serial),
        textInput("general.info$Instrument$Version", "Version", general.info$Instrument$Version),
        textInput("general.info$CalibrationDate", "Calibration Date", general.info$CalibrationDate),
        textInput("general.info$ProbingSystem$Type", "ProbingSystem Type", general.info$ProbingSystem$Type),
        textInput("general.info$ProbingSystem$Identification", "ProbingSystem Identification", general.info$ProbingSystem$Identification),
        textInput("general.info$Comment", "Comment", general.info$Comment),
        checkboxInput("setdefaultgeneral", "Set the General Info provided as default?")
      ),
      
      conditionalPanel( 
        condition = "input.information == 'Feature Information'",
        textInput("feature.info$Revision", "Revision", feature.info$Revision),
        textInput("feature.info$FeatureType", "Feature Type", feature.info$FeatureType),
        textInput("feature.info$Axes$CX$AxisType", "Axes CX", feature.info$Axes$CX$AxisType),
        textInput("feature.info$Axes$CX$DataType", "Axes CX", feature.info$Axes$CX$DataType),
        textInput("feature.info$Axes$CX$Increment", "Axes CX", feature.info$Axes$CX$Increment),
        textInput("feature.info$Axes$CX$Offset", "Axes CX", feature.info$Axes$CX$Offset),
        textInput("feature.info$Axes$CY$AxisType", "Axes CY", feature.info$Axes$CY$AxisType),
        textInput("feature.info$Axes$CY$DataType", "Axes CY", feature.info$Axes$CY$DataType),
        textInput("feature.info$Axes$CY$Increment", "Axes CY", feature.info$Axes$CY$Increment),
        textInput("feature.info$Axes$CY$Offset", "Axes CY", feature.info$Axes$CY$Offset),
        textInput("feature.info$Axes$CZ$AxisType", "Axes CZ", feature.info$Axes$CZ$AxisType),
        textInput("feature.info$Axes$CZ$DataType", "Axes CZ", feature.info$Axes$CZ$DataType),
        textInput("feature.info$Axes$CZ$Increment", "Axes CZ", feature.info$Axes$CZ$Increment),
        textInput("feature.info$Axes$CZ$Offset", "Axes CZ", feature.info$Axes$CZ$Offset),
        checkboxInput("setdefaultfeature", "Set the feature Info provided as default?")
      ),
      conditionalPanel( 
        condition = "input.information == 'Matrix Information'",
        textInput("matrix.info$MatrixDimension$SizeX", "Matrix Size X", matrix.info$MatrixDimension$SizeX),
        textInput("matrix.info$MatrixDimension$SizeY", "Matrix Size Y", matrix.info$MatrixDimension$SizeY),
        textInput("matrix.info$MatrixDimension$SizeZ", "Matrix Size Z", matrix.info$MatrixDimension$SizeZ)
      ),
      textOutput("text4"),
      textOutput("text5")
      
      
    )
  )
)   

###########################################################################

server <- function(input, output, session) {
  options(shiny.maxRequestSize=200*1024^2)
  # Initializing 
  file1name<- reactiveValues(a = NULL)
  cdat<- reactiveValues(a=NULL)
  cx3p<- reactiveValues(a=NULL)
  ft<- reactiveValues(a = NULL)
  data2<- reactiveValues(a = NULL)
  
  onefile.write<- function(data2,file1name){    
    # Setting Values of matrix dimensions by automatically using header.info
    feature.info$Axes$CX$Increment <- data2$header.info$obs_inc
    feature.info$Axes$CY$Increment <- data2$header.info$profile_inc
    
    matrix.info$MatrixDimension$SizeX <- data2$header.info$num_obs_per_profile
    matrix.info$MatrixDimension$SizeY <- data2$header.info$num_profiles
    
    output$text2 <- renderText({
      paste0("Current Sytem Time = ",as.character(Sys.time()))
    })
    output$text4 <- renderText({
      as.character(substrRight(file1name,3))
    })
    
    # Show the Header Info, Profile etc which is Automatically detected on reading a file
    output$text3 <- renderUI({
      str1 <- paste0("Number of profiles or Y dimension = ",as.character(data2$header.info$num_profiles))
      str2 <- paste0("Number of Observations per profile or X dimension = ",as.character(data2$header.info$num_obs_per_profile))
      str3 <- paste0("Profile Increments = ", as.character(data2$header.info$profile_inc))
      str4 <- paste0("Observation Increments = ",as.character(data2$header.info$obs_inc))
      str5 <- paste0("Profiley = ", as.character(input$profiley))
      HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>'))
    })
  }


  singlebrowse<- function(){
  #  if (input$mode == 'singfile'){
      pt<- choose.files(default= "", "Select Files")
      
      if (substrRight(pt,3)[1] == "dat"){
        if (!is.null(pt)) {
          cdat$a<- pt
        }
        
        output$text5 <- renderText({
          as.character(cdat$a)
          paste0(length(cdat$a), " dat files being processed")
        })
       # ft$a<- length(cdat$a)
        
        fpth<- cdat$a
        file1name$a<- basename(cdat$a)
        data2$a<- loadData( fpth,  fpth, input$profiley)
        
        output$text1 <- renderText({
          paste0(1, " file Loaded")
        })
      }
      
      
      if (substrRight(pt,3)[1] == "x3p"){
        if (!is.null(pt)) {
          #cdat<- Sys.glob(paste0(pt,"\\*.dat"))
          cx3p$a<- pt
        }
        output$text5 <- renderText({
          as.character(cx3p$a)
          paste0(length(cx3p$a), " x3p files being processed")
        })
        #ft$a<- length(cx3p$a)
        fpth<- cx3p$a
        file1name$a<- basename(cx3p$a)
        data2$a<- loadData( "x3p",  fpth, input$profiley)
        
        output$text1 <- renderText({
          paste0(1, " file Loaded")
        })
        
        # check if file is an x3p file, retrieve *.info from the file
        # if(substrRight(fpth,3)[1] == "x3p"){
        general.info<- data2$a$general.info
        feature.info<- data2$a$feature.info
        matrix.info<- data2$a$matrix.info
        # }
      }
      
      onefile.write(data2$a, file1name$a)
      
    #}
  }
  
  
  single.write<- function(){

    formData <- reactive({
      
      general.info$Creator<- input$'general.info$Creator'
      general.info$Instrument$Manufacturer<- input$'general.info$Instrument$Manufacturer'
      general.info$Instrument$Model<- input$'general.info$Instrument$Model'
      general.info$Instrument$Serial<- input$'general.info$Instrument$Serial'
      general.info$Instrument$Version<- input$'general.info$Instrument$Version'
      general.info$CalibrationDate<- input$'general.info$CalibrationDate'
      general.info$ProbingSystem$Type<- input$'general.info$ProbingSystem$Type'
      general.info$ProbingSystem$Identification<- input$'general.info$ProbingSystem$Identification'
      general.info$Comment<- input$'general.info$Comment'
      
      feature.info$Revision<- input$'feature.info$Revision'
      feature.info$FeatureType<- input$'feature.info$FeatureType'
      feature.info$Axes$CX$AxisType<- input$'feature.info$Axes$CX$AxisType'
      feature.info$Axes$CX$DataType<- input$'feature.info$Axes$CX$DataType'
      feature.info$Axes$CX$Increment<- input$'feature.info$Axes$CX$Increment'
      feature.info$Axes$CX$Offset<- input$'feature.info$Axes$CX$Offset'
      feature.info$Axes$CY$AxisType  <- input$'feature.info$Axes$CY$AxisType'
      feature.info$Axes$CY$DataType  <- input$'feature.info$Axes$CY$DataType'
      feature.info$Axes$CY$Increment  <- input$'feature.info$Axes$CY$Increment'
      feature.info$Axes$CY$Offset  <- input$'feature.info$Axes$CY$Offset'
      feature.info$Axes$CZ$AxisType  <- input$'feature.info$Axes$CZ$AxisType'
      feature.info$Axes$CZ$DataType  <- input$'feature.info$Axes$CZ$DataType'
      feature.info$Axes$CZ$Increment  <- input$'feature.info$Axes$CZ$Increment'
      feature.info$Axes$CZ$Offset  <- input$'feature.info$Axes$CZ$Offset'
      
      matrix.info$MatrixDimension$SizeX <- input$'matrix.info$MatrixDimension$SizeX'
      matrix.info$MatrixDimension$SizeY <- input$'matrix.info$MatrixDimension$SizeY'
      matrix.info$MatrixDimension$SizeZ <- input$'matrix.info$MatrixDimension$SizeZ'
      
      # Assign retrieved data from forms to the original file which will be written out later
      data2$a$general.info<- general.info
      data2$a$feature.info<- feature.info
      data2$a$matrix.info<- matrix.info
      data2$a
    })
    
    fileName <- sprintf("%s_%s-%s-%s_%s.x3p", date(Sys.time()), paste0(hour(Sys.time())), paste0(minute(Sys.time())), paste0(ceiling(second(Sys.time()))), as.character(strsplit(file1name$a, '.', fixed = TRUE)[[1]][1]))
    setwd(dirname(fpth))
    write_x3p(formData(), fileName)
    setwd(appdir)
  }
  
  batchbrowse<- function(){
    if (input$mode == 'batch'){
      pt<- choose.dir(default= "", "Select Folder")
      
      if (input$ftype == 'datf'){
        if (!is.null(pt)) {
          cdat$a<- Sys.glob(paste0(pt,"\\*.dat"))
        }
        
        output$text5 <- renderText({
          as.character(cdat$a)
          paste0(length(cdat$a), " dat files being processed")
        })
       # observe({ft$a<- isolate(length(cdat$a))})
        ft$a<- isolate(length(cdat$a))
      }
      
      
      if (input$ftype == 'x3pf'){
        if (!is.null(pt)) {
          #cdat<- Sys.glob(paste0(pt,"\\*.dat"))
          cx3p$a<- Sys.glob(paste0(pt,"\\*.x3p"))
        }
        output$text5 <- renderText({
          as.character(cx3p$a)
          paste0(length(cx3p$a), " x3p files being processed")
        })
      # observe({ ft$a<- isolate(length(cx3p$a))})
        ft$a<- isolate(length(cx3p$a))
      }
    }
    
  }
  
  batch.write<-function(i){
    if(input$ftype == "datf"){
      fpth<- cdat$a[i]
      file1name$a<- basename(cdat$a[i])
      data2$a<- loadData( fpth,  fpth, input$profiley)
    }
    
    if(input$ftype == "x3pf"){
      fpth<- cx3p$a[i]
      file1name$a<- basename(cx3p$a[i])
      data2$a<- loadData( "x3p",  fpth, input$profiley)
    }
    
    output$text1 <- renderText({
      paste0(i, " file Loaded")
    })
    # check if file is an x3p file, retrieve *.info from the file
    if(substrRight(fpth,3)[1] == "x3p"){
      general.info<- data2$a$general.info
      feature.info<- data2$a$feature.info
      matrix.info<- data2$a$matrix.info
    }
    
    onefile.write(data2$a, file1name$a)
    
    formData <- reactive({
      
      general.info$Creator<- input$'general.info$Creator'
      general.info$Instrument$Manufacturer<- input$'general.info$Instrument$Manufacturer'
      general.info$Instrument$Model<- input$'general.info$Instrument$Model'
      general.info$Instrument$Serial<- input$'general.info$Instrument$Serial'
      general.info$Instrument$Version<- input$'general.info$Instrument$Version'
      general.info$CalibrationDate<- input$'general.info$CalibrationDate'
      general.info$ProbingSystem$Type<- input$'general.info$ProbingSystem$Type'
      general.info$ProbingSystem$Identification<- input$'general.info$ProbingSystem$Identification'
      general.info$Comment<- input$'general.info$Comment'
      
      feature.info$Revision<- input$'feature.info$Revision'
      feature.info$FeatureType<- input$'feature.info$FeatureType'
      feature.info$Axes$CX$AxisType<- input$'feature.info$Axes$CX$AxisType'
      feature.info$Axes$CX$DataType<- input$'feature.info$Axes$CX$DataType'
      feature.info$Axes$CX$Increment<- input$'feature.info$Axes$CX$Increment'
      feature.info$Axes$CX$Offset<- input$'feature.info$Axes$CX$Offset'
      feature.info$Axes$CY$AxisType  <- input$'feature.info$Axes$CY$AxisType'
      feature.info$Axes$CY$DataType  <- input$'feature.info$Axes$CY$DataType'
      feature.info$Axes$CY$Increment  <- input$'feature.info$Axes$CY$Increment'
      feature.info$Axes$CY$Offset  <- input$'feature.info$Axes$CY$Offset'
      feature.info$Axes$CZ$AxisType  <- input$'feature.info$Axes$CZ$AxisType'
      feature.info$Axes$CZ$DataType  <- input$'feature.info$Axes$CZ$DataType'
      feature.info$Axes$CZ$Increment  <- input$'feature.info$Axes$CZ$Increment'
      feature.info$Axes$CZ$Offset  <- input$'feature.info$Axes$CZ$Offset'
      
      matrix.info$MatrixDimension$SizeX <- input$'matrix.info$MatrixDimension$SizeX'
      matrix.info$MatrixDimension$SizeY <- input$'matrix.info$MatrixDimension$SizeY'
      matrix.info$MatrixDimension$SizeZ <- input$'matrix.info$MatrixDimension$SizeZ'
      
      # Assign retrieved data from forms to the original file which will be written out later
      data2$a$general.info<- general.info
      data2$a$feature.info<- feature.info
      data2$a$matrix.info<- matrix.info
      data2$a
    })
    
    fileName <- sprintf("%s_%s-%s-%s_%s.x3p", date(Sys.time()), paste0(hour(Sys.time())), paste0(minute(Sys.time())), paste0(ceiling(second(Sys.time()))), as.character(strsplit(file1name$a, '.', fixed = TRUE)[[1]][1]))
    setwd(dirname(fpth))
    write_x3p(formData(), fileName)
    setwd(appdir)
    # })
    
  }
  

observeEvent(input$writebatch, {
  count<- isolate(ft$a)
  #breact<-reactive({
    lapply(X = 1:count, FUN = function(i){batch.write(i)})
  #})
    output$text5 <- renderText({
      "All files converted"
    })
})

  observeEvent(input$write, {
    single.write()
    output$text5 <- renderText({
      "All files converted"
    })
    }) 
  
  # Reset button to refresh page to blank new session like look
  observeEvent(input$reset_button, {js$reset()})
  
  observeEvent(input$folderget, {
        batchbrowse()
  })
  
  observeEvent(input$fileget, {
        singlebrowse()
  })
  
  
}

shinyApp(ui, server)