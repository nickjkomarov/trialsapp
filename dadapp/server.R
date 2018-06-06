#########################################################################
#Purpose: Define all server logics needed to create results             #
#Created: 4/20/2017                                                     # 
#Naming:  S1-Single Line Plot                                           # 
#         S2-AUDPC                                                      # 
#         M1-Multi Trial Analysis                                       # 
#         M2-Relative to Specified Treatment                            # 
#         M3-Two-Response Plot                                          # 
#         M4-Heat Map                                                   # 
#         M5-Piano Chart                                                # 
#         M6-Box Plot                                                   # 
#         M7-Spider Plot                                                # 
#         M8-Bar Chart                                                  # 
#Setup:   All R programs should be in the same application home         #
#         directory on the server.                                      # 
#Note:    There are 8 tasks, referred to as trials, and coded as S1,    #
#         M1-M8. The are inside reactive values so that we can use loops#
#         to include logic once in the program to make program shorter. #  
#         Most variable names in the program match the numbers and      #
#         positions shown in UI for easy idenfication.                  #
#         All plots are saved in the common directory OutputDir before  #
#         showing on the screen. Download button actually copies the    #
#         saved pdf file to the user desired location.                  #
#########################################################################
#setwd("\\\\rtpvmntctx02d.ddnet1.net/Trials App/Dev")
setwd("C:/Users/Nick/Desktop/Dads app files")
#setwd("E:/REVEAL20180126/RScript")
#setwd('C:/Users/xjin/E/RScript/RScript')
#Include all R programs needed to support the application
#They should be in the same application home directory as ui.R and server.R on the server.  
source('Packages.R')
source('Handlers.R')
source('multiplot.R')
source('relativeplot.R')
source('tworesponseplot.R')
source('heatmap.R')
source('boxplotbarchart.R')
source('radarplot.R')
source('pianochart.R')
source('singlelineplot.R')
source('AUDPC.R')

#This is a common directory used for all plots
#HomeDir="E:/REVEAL20180126/RScript"
#HomeDir='C:/Users/xjin/E/RScript/RScript'
HomeDir='C:/Users/Nick/Desktop/Dads app files'
OutputDir=paste0(HomeDir,"/output")
ImageDir=paste0(HomeDir,"/image")
file.remove(file.path(OutputDir, list.files(OutputDir)))

#Function to sort columns is desired order, returns vector sorted in desired order
#selected - values already selected, should already be on top
#data - data source where the values are from 
#input - input vector to be sorted
#defaultSort - specified by users, usually a hard-coded vector with desired sort order
get_var_choices<-function(selected, data, input, defaultSort) {
  try({
  varselected <- unlist(strsplit(selected,split="/"))
  VarSorted <- c(intersect(names(data),defaultSort), setdiff(names(data),defaultSort))
  VarChoices<-VarSorted[grepl(toupper(trimws(input)),toupper(VarSorted))]
  VarChoices<-unique(c(varselected,VarChoices))
  })
}

server <- function(input, output, session) {
  #options to handle large import file size
  options(shiny.maxRequestSize=10000*1024^2) 
  options(java.parameters = "-Xmx8000m")
  
  start.time <- Sys.time()

  #Define repetitive values so that they can be used in loops to shorted code
  data <- reactiveValues(S1 = NULL,S2 = NULL, calculated = NULL, M1 = NULL,M2 = NULL,M3 = NULL,M4 = NULL,M5 = NULL,M6 = NULL,M7 = NULL,M8 = NULL)
  adafa <- reactiveValues(S1 = NULL)
  data_renamed <- reactiveValues(S1 = NULL,S2 = NULL, M1 = NULL,M2 = NULL,M3 = NULL,M4 = NULL,M5 = NULL,M6 = NULL,M7 = NULL,M8 = NULL)
  Plot_Sort_Order <- reactiveValues(S1 = NULL,S2 = NULL, M1 = NULL,M2 = NULL,M3 = NULL,M4 = NULL,M5 = NULL,M6 = NULL,M7 = NULL,M8 = NULL)
  selected <- reactiveValues(S1 = NULL,S2 = NULL,M1 = NULL,M2 = NULL,M3 = NULL,M4 = NULL,M5 = NULL,M6 = NULL,M7 = NULL,M8 = NULL)
  filtered <- reactiveValues(S1 = NULL,S2 = NULL,M1 = NULL,M2 = NULL,M3 = NULL,M4 = NULL,M5 = NULL,M6 = NULL,M7 = NULL,M8 = NULL)
  URL <- reactiveValues(S1 = NULL,S2 = NULL,M1 = NULL,M2 = NULL,M3 = NULL,M4 = NULL,M5 = NULL,M6 = NULL,M7 = NULL,M8 = NULL)
  pdfname <- reactiveValues(S1 = NULL,S2 = NULL,M1 = NULL,M2 = NULL,M3 = NULL,M4 = NULL,M5 = NULL,M6 = NULL,M7 = NULL,M8 = NULL)
  trials <- c("S1","S2","M1","M2","M3","M4","M5","M6","M7","M8")
  trials_trt <- c(
                  "M1", #Multi Trial
                  "M6",	#Box Plot
                  "M8"	#Bar Chart
                  )
  trt_selected <- reactiveValues(S1 = NULL,S2 = NULL,M1 = NULL,M2 = NULL,M3 = NULL,M4 = NULL,M5 = NULL,M6 = NULL,M7 = NULL,M8 = NULL)
  trt_renamed <- reactiveValues(S1 = NULL,S2 = NULL,M1 = NULL,M2 = NULL,M3 = NULL,M4 = NULL,M5 = NULL,M6 = NULL,M7 = NULL,M8 = NULL)
  datahot = reactiveValues(S1 = NULL,S2 = NULL,M1 = NULL,M2 = NULL,M3 = NULL,M4 = NULL,M5 = NULL,M6 = NULL,M7 = NULL,M8 = NULL)

  filterdefault <- list(
  S1 = c("Symptom","Crop","PestID"),
  S2 = c("Crop","Symptom","Pest"),
  M1 = c("Symptom","Crop","PestID"),
  M2 = c("Symptom","Crop","PestID"),
  M3 = c("Symptom","Crop","PestID"),
  M4 = c("Symptom","Crop","PestID"),
  M5 = c("Symptom","Crop","PestID"),
  M6 = c("Symptom","Crop","PestID"),
  M7 = c("Crop_Pest","Pest_Subclass","Symptom"),
  M8 = c("Symptom","Crop","PestID"))
  filtervaldefault <- list(
  S1 = c("all","all","all"),
  S2 = c("all","all","all"),
  M1 = c("all","all","all"),
  M2 = c("all","all","all"),
  M3 = c("all","all","all"),
  M4 = c("all","all","all"),
  M5 = c("all","all","all"),
  M6 = c("all","all","all"),
  M7 = c("Pest","Monocotyledones","CONTRO"),
  M8 = c("all","all","all"))
  examples <- c("box.gif","bar.gif","rat.gif","rel.gif","two.gif","heat.gif","piano.gif","boxplot.gif","radar.gif","barchart.gif")

  #select 3 categorical subset variables and values
  #Loop through all 7 tasks and 3 subset columns since they contain same logic
  for (i in 1:length(trials)) { 
    local({ 
      my_i <- i 
      for (j in 1:3) { 
        local({ 
          my_j <- j
          dataSheetName <- paste0(trials[my_i], "datasheet") 
          outputVarUIName <- paste0(trials[my_i], "subsetvar",my_j,"UI")
          outputValUIName <- paste0(trials[my_i], "subsetval",my_j,"UI")
          outputOprUIName <- paste0(trials[my_i], "subsetopr",my_j,"UI")
          subsetVarIDName <- paste0(trials[my_i], "subsetvar",my_j)
          subsetValIDName <- paste0(trials[my_i], "subsetval",my_j)
          subsetOprIDName <- paste0(trials[my_i], "subsetopr",my_j)
          filtercols <- filterdefault[[trials[my_i]]]
          filtervals <- filtervaldefault[[trials[my_i]]]
          if (trials[my_i]=="M1") plot_analysis <- "analysis"
          else plot_analysis <- "plot"
          output[[outputVarUIName]] <- renderUI({
            if(is.null(input[[dataSheetName]])) {return()}   
            selectInput(subsetVarIDName, label = h5(paste0("Select column ",my_j," to filter by")), width='90%',
                        choices = colnames(data[[trials[my_i]]])[sapply(data[[trials[my_i]]],class) %in%c("factor","character")], 
                        selected=filtercols[my_j])
          })
          output[[outputValUIName]] <- renderUI({
            if(is.null(input[[dataSheetName]])) {return()}
            if(is.null(input[[subsetVarIDName]])) {return()} 
                #selectInput(subsetValIDName, label = h5(paste0("Select value of column ",my_j," for ", plot_analysis)),multiple = F, width='90%',
                #        choices = c("all",levels(factor(data[[trials[my_i]]][,input[[subsetVarIDName]]]))), 
                #        #selected=input[[subsetValIDName]])
                #        selected=filtervals[my_j])
            div(style='height:150px; width:91%; overflow: scroll',
                checkboxGroupInput(subsetValIDName,h5(" "), 
                choices =c("all",levels(factor(data[[trials[my_i]]][,input[[subsetVarIDName]]]))), 
                selected="all",inline=F)
            )    
          })            
          output[[outputOprUIName]] <- renderUI({ 
            if(is.null(input[[dataSheetName]])) {return()}
            if(is.null(input[[subsetVarIDName]])) {return()} 
            selectInput(subsetOprIDName, label = h5("Select Operator"),multiple = F, width='90%',
                        choices = c('Include','Exclude'), selected='Include')    #input[[subsetOprIDName]])
          })
	  #When a specific selection is made, the All box should NOT stay selected.
	  observeEvent(input[[subsetValIDName]],{
            if (length(input[[subsetValIDName]])>1 & "all" %in% input[[subsetValIDName]]) {
	      updateSelectInput(session, subsetValIDName, selected =  input[[subsetValIDName]][input[[subsetValIDName]] !='all'])  
	      
	    }
	    if (!is.null(filtered[[trials[my_i]]])) {
              if (!"all" %in% filtered[[trials[my_i]]]  & "all" %in% input[[subsetValIDName]]) {
	        updateSelectInput(session, subsetValIDName, selected = 'all')  
	      }     
	    }
	    filtered[[trials[my_i]]] <- input[[subsetValIDName]]
	  })
        })
      } 
    }) 
  }
  
  #Build filtered data using subset variables and values
  #"all" is treated as not filting the data
  dataS1 <- reactive({
    if(is.null(data$S1)) {return()}
    #m<-subset(data$S1,(get(input$S1subsetvar1)==input$S1subsetval1 | input$S1subsetval1 == "all") &
    #                  (get(input$S1subsetvar2)==input$S1subsetval2 | input$S1subsetval2 == "all") &
    #                  (get(input$S1subsetvar3)==input$S1subsetval3 | input$S1subsetval3 == "all") &
    #                  (get("Trial_No_")==input$S1select1aval | input$S1select1aval == "all")) 
    #####filer vars with operators######
    m<-data$S1
    try({
    if (input$S1subsetopr1=="Include") m <- subset(m,(get(input$S1subsetvar1) %in% input$S1subsetval1 | "all" %in% input$S1subsetval1))
    if (input$S1subsetopr1=="Exclude") m <- subset(m,(!get(input$S1subsetvar1) %in% input$S1subsetval1))
    if (input$S1subsetopr2=="Include") m <- subset(m,(get(input$S1subsetvar2) %in% input$S1subsetval2 | "all" %in% input$S1subsetval2))
    if (input$S1subsetopr2=="Exclude") m <- subset(m,(!get(input$S1subsetvar2) %in% input$S1subsetval2))
    if (input$S1subsetopr3=="Include") m <- subset(m,(get(input$S1subsetvar3) %in% input$S1subsetval3 | "all" %in% input$S1subsetval3))
    if (input$S1subsetopr3=="Exclude") m <- subset(m,(!get(input$S1subsetvar3) %in% input$S1subsetval3))  
    m <- subset(m,(get("Trial_No_")==input$S1select1aval | input$S1select1aval == "all"))
    })
   (m)
  })
  
  #Build filtered data ADAFA for Single Line Plot only.
  adafaS1 <- reactive({
    if(is.null(adafa$S1)) {return()}
    if (input$S1subsetvar1 %in% colnames(adafa$S1))
      m<-subset(adafa$S1,(get(input$S1subsetvar1)==input$S1subsetval1 | input$S1subsetval1 == "all"))
    else 
      m<-adafa$S1
    if (input$S1subsetvar2 %in% colnames(adafa$S1))
      m<-subset(m,(get(input$S1subsetvar2)==input$S1subsetval2 | input$S1subsetval2 == "all"))
    if (input$S1subsetvar1 %in% colnames(adafa$S1))
      m<-subset(m,(get(input$S1subsetvar3)==input$S1subsetval3 | input$S1subsetval31 == "all"))
   (m)
  })
  
  dataS2 <- reactive({
    if(is.null(data$S2)) {return()}
    m<-data$S2
    if (input$S2subsetopr1=="Include") m <- subset(m,(get(input$S2subsetvar1) %in% input$S2subsetval1 | "all" %in% input$S2subsetval1))
    if (input$S2subsetopr1=="Exclude") m <- subset(m,(!get(input$S2subsetvar1) %in% input$S2subsetval1))
    if (input$S2subsetopr2=="Include") m <- subset(m,(get(input$S2subsetvar2) %in% input$S2subsetval2 | "all" %in% input$S2subsetval2))
    if (input$S2subsetopr2=="Exclude") m <- subset(m,(!get(input$S2subsetvar2) %in% input$S2subsetval2))
    if (input$S2subsetopr3=="Include") m <- subset(m,(get(input$S2subsetvar3) %in% input$S2subsetval3 | "all" %in% input$S2subsetval3))
    if (input$S2subsetopr3=="Exclude") m <- subset(m,(!get(input$S2subsetvar3) %in% input$S2subsetval3))  
   (m)
  })
  
  dataM1 <- reactive({
    if(is.null(data$M1)) {return()}
    #####filer vars with operators######
    m<-data$M1
    if (input$M1subsetopr1=="Include") m <- subset(m,(get(input$M1subsetvar1) %in% input$M1subsetval1 | "all" %in% input$M1subsetval1))
    if (input$M1subsetopr1=="Exclude") m <- subset(m,(!get(input$M1subsetvar1) %in% input$M1subsetval1))
    if (input$M1subsetopr2=="Include") m <- subset(m,(get(input$M1subsetvar2) %in% input$M1subsetval2 | "all" %in% input$M1subsetval2))
    if (input$M1subsetopr2=="Exclude") m <- subset(m,(!get(input$M1subsetvar2) %in% input$M1subsetval2))
    if (input$M1subsetopr3=="Include") m <- subset(m,(get(input$M1subsetvar3) %in% input$M1subsetval3 | "all" %in% input$M1subsetval3))
    if (input$M1subsetopr3=="Exclude") m <- subset(m,(!get(input$M1subsetvar3) %in% input$M1subsetval3))  

    if(input$M1select2gval=="Herbicide Visual Response (0-100)" | input$M1select2gval=="Phyto") {                  
      m$Rating <- ""
      y <- as.numeric(m[,input$M1select2bval])
      members <- as.integer(input$M1select2nval)
      for(i in 1:members){    
        if (input[[paste0("O1",i)]]==">" & input[[paste0("O2",i)]]=="=")  m[y>as.numeric(input[[paste0("S",i)]]) & y==as.numeric(input[[paste0("E",i)]]),"Rating"] <- input[[paste0("L",i)]]
        if (input[[paste0("O1",i)]]==">" & input[[paste0("O2",i)]]=="<=") m[y>as.numeric(input[[paste0("S",i)]]) & y<=as.numeric(input[[paste0("E",i)]]),"Rating"] <- input[[paste0("L",i)]]
        if (input[[paste0("O1",i)]]==">" & input[[paste0("O2",i)]]=="<") m[y>as.numeric(input[[paste0("S",i)]]) & y<as.numeric(input[[paste0("E",i)]]),"Rating"] <- input[[paste0("L",i)]]
        
        if (input[[paste0("O1",i)]]==">=" & input[[paste0("O2",i)]]=="=")  m[y>=as.numeric(input[[paste0("S",i)]]) & y==as.numeric(input[[paste0("E",i)]]),"Rating"] <- input[[paste0("L",i)]]
        if (input[[paste0("O1",i)]]==">=" & input[[paste0("O2",i)]]=="<=") m[y>=as.numeric(input[[paste0("S",i)]]) & y<=as.numeric(input[[paste0("E",i)]]),"Rating"] <- input[[paste0("L",i)]]
        if (input[[paste0("O1",i)]]==">=" & input[[paste0("O2",i)]]=="<") m[y>=as.numeric(input[[paste0("S",i)]]) & y<as.numeric(input[[paste0("E",i)]]),"Rating"] <- input[[paste0("L",i)]]
        
        if (input[[paste0("O1",i)]]=="=" & input[[paste0("O2",i)]]=="=")  m[y==as.numeric(input[[paste0("S",i)]]) & y==as.numeric(input[[paste0("E",i)]]),"Rating"] <- input[[paste0("L",i)]]
        if (input[[paste0("O1",i)]]=="=" & input[[paste0("O2",i)]]=="<=") m[y==as.numeric(input[[paste0("S",i)]]) & y<=as.numeric(input[[paste0("E",i)]]),"Rating"] <- input[[paste0("L",i)]]
        if (input[[paste0("O1",i)]]=="=" & input[[paste0("O2",i)]]=="<") m[y==as.numeric(input[[paste0("S",i)]]) & y<as.numeric(input[[paste0("E",i)]]),"Rating"] <- input[[paste0("L",i)]]
      }
    }   
   (m)
  })
  
  dataM2 <- reactive({
    if(is.null(data$M2)) {return()}
    #####filer vars with operators######
    m<-data$M2
    if (input$M2subsetopr1=="Include") m <- subset(m,(get(input$M2subsetvar1) %in% input$M2subsetval1 | "all" %in% input$M2subsetval1))
    if (input$M2subsetopr1=="Exclude") m <- subset(m,(!get(input$M2subsetvar1) %in% input$M2subsetval1))
    if (input$M2subsetopr2=="Include") m <- subset(m,(get(input$M2subsetvar2) %in% input$M2subsetval2 | "all" %in% input$M2subsetval2))
    if (input$M2subsetopr2=="Exclude") m <- subset(m,(!get(input$M2subsetvar2) %in% input$M2subsetval2))
    if (input$M2subsetopr3=="Include") m <- subset(m,(get(input$M2subsetvar3) %in% input$M2subsetval3 | "all" %in% input$M2subsetval3))
    if (input$M2subsetopr3=="Exclude") m <- subset(m,(!get(input$M2subsetvar3) %in% input$M2subsetval3))  
   (m)
  })

  dataM3 <- reactive({
    if(is.null(data$M3)) {return()}
    #####filer vars with operators######
    m<-data$M3
    if (input$M3subsetopr1=="Include") m <- subset(m,(get(input$M3subsetvar1) %in% input$M3subsetval1 | "all" %in% input$M3subsetval1))
    if (input$M3subsetopr1=="Exclude") m <- subset(m,(!get(input$M3subsetvar1) %in% input$M3subsetval1))
    if (input$M3subsetopr2=="Include") m <- subset(m,(get(input$M3subsetvar2) %in% input$M3subsetval2 | "all" %in% input$M3subsetval2))
    if (input$M3subsetopr2=="Exclude") m <- subset(m,(!get(input$M3subsetvar2) %in% input$M3subsetval2))
    if (input$M3subsetopr3=="Include") m <- subset(m,(get(input$M3subsetvar3) %in% input$M3subsetval3 | "all" %in% input$M3subsetval3))
    if (input$M3subsetopr3=="Exclude") m <- subset(m,(!get(input$M3subsetvar3) %in% input$M3subsetval3))  
   (m)
  })

  dataM4 <- reactive({
    if(is.null(data$M4)) {return()}
    #####filer vars with operators######
    m<-data$M4
    if (input$M4subsetopr1=="Include") m <- subset(m,(get(input$M4subsetvar1) %in% input$M4subsetval1 | "all" %in% input$M4subsetval1))
    if (input$M4subsetopr1=="Exclude") m <- subset(m,(!get(input$M4subsetvar1) %in% input$M4subsetval1))
    if (input$M4subsetopr2=="Include") m <- subset(m,(get(input$M4subsetvar2) %in% input$M4subsetval2 | "all" %in% input$M4subsetval2))
    if (input$M4subsetopr2=="Exclude") m <- subset(m,(!get(input$M4subsetvar2) %in% input$M4subsetval2))
    if (input$M4subsetopr3=="Include") m <- subset(m,(get(input$M4subsetvar3) %in% input$M4subsetval3 | "all" %in% input$M4subsetval3))
    if (input$M4subsetopr3=="Exclude") m <- subset(m,(!get(input$M4subsetvar3) %in% input$M4subsetval3))  
   (m)
  })
  
  dataM5 <- reactive({
    if(is.null(data$M5)) {return()}
    #####filer vars with operators######
    m<-data$M5
    if (input$M5subsetopr1=="Include") m <- subset(m,(get(input$M5subsetvar1) %in% input$M5subsetval1 | "all" %in% input$M5subsetval1))
    if (input$M5subsetopr1=="Exclude") m <- subset(m,(!get(input$M5subsetvar1) %in% input$M5subsetval1))
    if (input$M5subsetopr2=="Include") m <- subset(m,(get(input$M5subsetvar2) %in% input$M5subsetval2 | "all" %in% input$M5subsetval2))
    if (input$M5subsetopr2=="Exclude") m <- subset(m,(!get(input$M5subsetvar2) %in% input$M5subsetval2))
    if (input$M5subsetopr3=="Include") m <- subset(m,(get(input$M5subsetvar3) %in% input$M5subsetval3 | "all" %in% input$M5subsetval3))
    if (input$M5subsetopr3=="Exclude") m <- subset(m,(!get(input$M5subsetvar3) %in% input$M5subsetval3))  
   (m)
  })

  dataM6 <- reactive({
    if(is.null(data$M6)) {return()}
    #####filer vars with operators######
    m<-data$M6
    if (input$M6subsetopr1=="Include") m <- subset(m,(get(input$M6subsetvar1) %in% input$M6subsetval1 | "all" %in% input$M6subsetval1))
    if (input$M6subsetopr1=="Exclude") m <- subset(m,(!get(input$M6subsetvar1) %in% input$M6subsetval1))
    if (input$M6subsetopr2=="Include") m <- subset(m,(get(input$M6subsetvar2) %in% input$M6subsetval2 | "all" %in% input$M6subsetval2))
    if (input$M6subsetopr2=="Exclude") m <- subset(m,(!get(input$M6subsetvar2) %in% input$M6subsetval2))
    if (input$M6subsetopr3=="Include") m <- subset(m,(get(input$M6subsetvar3) %in% input$M6subsetval3 | "all" %in% input$M6subsetval3))
    if (input$M6subsetopr3=="Exclude") m <- subset(m,(!get(input$M6subsetvar3) %in% input$M6subsetval3))  
   (m)
  })

  dataM7 <- reactive({
    if(is.null(data$M7)) {return()}
    m<-data$M7
    if (input$M7subsetopr1=="Include") m <- subset(m,(get(input$M7subsetvar1) %in% input$M7subsetval1 | "all" %in% input$M7subsetval1))
    if (input$M7subsetopr1=="Exclude") m <- subset(m,(!get(input$M7subsetvar1) %in% input$M7subsetval1))
    if (input$M7subsetopr2=="Include") m <- subset(m,(get(input$M7subsetvar2) %in% input$M7subsetval2 | "all" %in% input$M7subsetval2))
    if (input$M7subsetopr2=="Exclude") m <- subset(m,(!get(input$M7subsetvar2) %in% input$M7subsetval2))
    if (input$M7subsetopr3=="Include") m <- subset(m,(get(input$M7subsetvar3) %in% input$M7subsetval3 | "all" %in% input$M7subsetval3))
    if (input$M7subsetopr3=="Exclude") m <- subset(m,(!get(input$M7subsetvar3) %in% input$M7subsetval3))   
   (m)
  })
  
  dataM8 <- reactive({
    if(is.null(data$M8)) {return()}
    #####filer vars with operators######
    m<-data$M8
    if (input$M8subsetopr1=="Include") m <- subset(m,(get(input$M8subsetvar1) %in% input$M8subsetval1 | "all" %in% input$M8subsetval1))
    if (input$M8subsetopr1=="Exclude") m <- subset(m,(!get(input$M8subsetvar1) %in% input$M8subsetval1))
    if (input$M8subsetopr2=="Include") m <- subset(m,(get(input$M8subsetvar2) %in% input$M8subsetval2 | "all" %in% input$M8subsetval2))
    if (input$M8subsetopr2=="Exclude") m <- subset(m,(!get(input$M8subsetvar2) %in% input$M8subsetval2))
    if (input$M8subsetopr3=="Include") m <- subset(m,(get(input$M8subsetvar3) %in% input$M8subsetval3 | "all" %in% input$M8subsetval3))
    if (input$M8subsetopr3=="Exclude") m <- subset(m,(!get(input$M8subsetvar3) %in% input$M8subsetval3))     
   (m)
  })

  #######################################################################################################
  ##Renamed data, used in data tab. If rename is not done, then renamed data is equals the original data. 
  ##Renamed data is used as the data source for calling each interface/plot
  ##Vector trials_trt contains the list of interfaces where renaming treatment is possible.
  ##Original treatment name is saved in trt1.
  ##Additional field Plot_Sort_Order is added to the renamed dataset. It is initially set to be 1:N
  ##Each time the rename button is clicked, user selected sort order is saved in the value Plot_Sort_Order
  ##Only M1, M6, M8 use plot order, others only rename, not order 
  #######################################################################################################
  data_renamedM1 <- reactive({
    if(is.null(dataM1())) {return()}
	m <- dataM1()
    if(is.null(data_renamed$M1)) {
      trt <- input$M1select2aval
      m[,paste0(trt,"1")] <- m[,trt]
      v <- unique(m[,trt])
      m$Plot_Sort_Order <- 1 #1:length(v)
    } else {
	  df <- unique(data_renamed$M1[,c(1,2,3)])
	  trt1 <- colnames(df)[1]
	  trt <- colnames(df)[2]
	  m[,trt1] <- m[,trt]
	  m <- m[,colnames(m) !=trt]
	  m <- merge(m,df, by=trt1)
	}
    assign("data_renamed_M1", data_renamed$M1 ,envir = .GlobalEnv)
    assign("data_renamedM1", m ,envir = .GlobalEnv)
    assign("data_M1", dataM1() ,envir = .GlobalEnv) 
    assign("dataM1", data$M1 ,envir = .GlobalEnv)  #Need the statement to force reactivity
    (m)
  })

  data_renamedM6 <- reactive({
    if(is.null(dataM6())) {return()}
	m <- dataM6()
    if(is.null(data_renamed$M6)) {
      trt <- input$M6select2aval
      m[,paste0(trt,"1")] <- m[,trt]
      v <- unique(m[,trt])
      m$Plot_Sort_Order <- 1 #1:length(v)
    } else {
	  df <- unique(data_renamed$M6[,c(1,2,3)])
	  trt1 <- colnames(df)[1]
	  trt <- colnames(df)[2]
	  m[,trt1] <- m[,trt]
	  m <- m[,colnames(m) !=trt]
	  m <- merge(m,df, by=trt1)
	}
    assign("data_renamed_M6", data_renamed$M6 ,envir = .GlobalEnv)
    assign("data_renamedM6", m ,envir = .GlobalEnv)
    assign("data_M6", dataM6() ,envir = .GlobalEnv) 
    assign("dataM6", data$M6 ,envir = .GlobalEnv)  #Need the statement to force reactivity
    (m)
  })


  #######################################################################################################
  # Display filtered data tab.
  #######################################################################################################
  for (i in 1:length(trials)) { 
      local({ 
        my_i <- i 
        outputName <- paste0(trials[my_i], "source_data") 
        dataName <- paste0("data",trials[my_i]) 
        data_renamedName <- paste0("data_renamed",trials[my_i]) 
        output[[outputName]] <- renderDataTable({
        if(is.null(get(dataName)())) return() 
        if (trials[my_i] %in% trials_trt) {
		  if (!is.null(data_renamed[[trials[my_i]]])) get(data_renamedName)() ##Renamed
          else get(dataName)()
        } else get(dataName)()
        })
      }) 
  } 
  
  #Build Progress alerts.
  #This alerts is active when import is process, closed when import is completed.
  for (i in 1:length(trials)) { 
    local({ 
      my_i <- i 
      FileName <- paste0("file", trials[my_i]) 
      AlertName <- paste0(trials[my_i], "ImportAlert") 
      AlertNameID <- paste0(AlertName, "ID") 
      observeEvent(input[[FileName]], {
        createAlert(session, AlertNameID, AlertName, title = "Importing Data, Please Wait...",
                content = "The rest of the parameters will show after import process is completed.", append = FALSE)
      }) 
    }) 
  } 

  #This alerts is active when plotting is process, closed when plotting is completed.
  for (i in 1:length(trials)) { 
    local({ 
      my_i <- i 
      PlotIdName <- paste0(trials[my_i], "CreatePlot") 
      AlertName <- paste0(trials[my_i], "PlotAlert") 
      AlertNameID <- paste0(AlertName, "ID") 
      GroupingColumns <- paste0(trials[my_i],"select1val")
      observeEvent(input[[PlotIdName]], {
        if(!is.null(input[[GroupingColumns]])) {
          createAlert(session, AlertNameID, AlertName, title = "Plotting Data, Please Wait...",
                content = "The system will navigate to the Plot tab after the plot is created.", append = FALSE)
      }
      }) 
    }) 
  } 
  
  # Display search Boxes.
  for (i in 1:length(trials)) { 
    local({ 
      my_i <- i 
      outputName <- paste0(trials[my_i], "search1") 
      IdName <- paste0(trials[my_i], "search1val") 
      output[[outputName]] <- renderUI({
        if(is.null(data[[trials[my_i]]])) return()
        else {
          textInput(inputId=IdName, h5("Search"), "")
        }
      })
    }) 
  } 

  #Selected History
  #selected is a reactive value for the program to store selected items, 
  #not for user viewing because we need to remember which items have been 
  #selected and display them on top of the list 

  for (i in 1:length(trials)) { 
    local({ 
      my_i <- i 
      SearchID <- paste0(trials[my_i], "search1val") 
      SelectID <- paste0(trials[my_i], "select1val") 
      observeEvent(input[[SearchID]],{
        selected[[trials[my_i]]] <- paste(input[[SelectID]],collapse="/")
      })
    }) 
  } 

  #Select sheet names from Excel files.
  #The loop includes all trials because they have the same function
  #They all need to select sheet name first, and ADAFA for Single only
  for (i in 1:length(trials)) { 
    local({ 
      my_i <- i 
      FileName <- paste0("file", trials[my_i]) 
      observeEvent(input[[FileName]], {  
        # Get Excel sheetnames.
        outputName <- paste0(trials[my_i], "datasheetUI") 
        outputName1 <- paste0(trials[my_i], "datasheet1UI") 
        dataSheetName <- paste0(trials[my_i], "datasheet") 
        dataSheetName1 <- paste0(trials[my_i], "datasheet1")
        xlsxname <- paste0(input[[FileName]]$datapath,'.xlsx')
        file.copy(input[[FileName]]$datapath,xlsxname)
        SheetNames=getSheetNames(xlsxname)
        SheetNames1 <- paste0(input[[FileName]],SheetNames)
        output[[outputName]] <- renderUI({
          selectInput(dataSheetName, label = h5("Select a data sheet name"),multiple = F,
                      width='80%', choices = SheetNames, selected="Data")
        })
        output[[outputName1]] <- renderUI({
          hidden(
          selectInput(dataSheetName1, label = h5("Full data sheet name"),multiple = F,
                      width='80%', choices = paste(input[[FileName]], SheetNames1), selected=input[[dataSheetName1]])
          )
        })
        if(trials[my_i]=="S1"){
          output$S1adafasheetUI <- renderUI({
            selectInput("S1adafasheet", label = h5("Select an ADAFA sheet name"),multiple = F,
                        width='80%', choices = SheetNames, selected='ADAFA')
          }) 
        }
      })
    }) 
  } 
        
  #Read selected Excel sheet.
  #Loops through all trials because they have the same function
  #Only Single has ADAFA sheet
  for (i in 1:length(trials)) { 
      local({ 
        my_i <- i 
        FileName <- paste0("file", trials[my_i]) 
        dataSheetName <- paste0(trials[my_i], "datasheet") 
        dataSheetName1 <- paste0(trials[my_i], "datasheet1") 
        adafaSheetName <- paste0(trials[my_i], "adafasheet") 
        AlertName <- paste0(trials[my_i], "ImportAlert") 
        observeEvent(input[[dataSheetName]], {  
          if(trials[my_i]=="S1") {sheetlist <- list(data=input[[dataSheetName]],adafa=input[[adafaSheetName]])}
          else {sheetlist <- list(data=input[[dataSheetName]])}
          outdata <- do_import(session,trials[my_i], input[[FileName]]$datapath,sheetlist,AlertName)
          if (is.null(outdata$errmsg)) {
            if (!identical(data[[trials[my_i]]], outdata$data)) {
              Sys.sleep(5)
              shinyjs::alert(paste0("Data import completed!, with ", ncol(outdata$data)," columns and ", nrow(outdata$data), " rows.") )
            }
          }
          data[[trials[my_i]]] <- outdata$data
          adafa[[trials[my_i]]] <- outdata$adafa
        })
        observeEvent(input[[dataSheetName1]], {  
          if(trials[my_i]=="S1") {sheetlist <- list(data=input[[dataSheetName]],adafa=input[[adafaSheetName]])}
          else {sheetlist <- list(data=input[[dataSheetName]])}
          outdata <- do_import(session,trials[my_i], input[[FileName]]$datapath,sheetlist,AlertName)
          if (is.null(outdata$errmsg)) {
            if (!identical(data[[trials[my_i]]], outdata$data)) {
              shinyjs::alert(paste0("Data import completed!, with ", ncol(outdata$data)," columns and ", nrow(outdata$data), " rows.") )
            }
          }
          data[[trials[my_i]]] <- outdata$data
          adafa[[trials[my_i]]] <- outdata$adafa
		  #hotM1 <- NULL
		  #datahot$M1 <- NULL
		  #data_renamedM1 <- NULL
		  data_renamed$M1 <- NULL
        })
        observeEvent(input[[adafaSheetName]], {  
          if(trials[my_i]=="S1") {sheetlist <- list(data=input[[dataSheetName]],adafa=input[[adafaSheetName]])}
          else {sheetlist <- list(data=input[[dataSheetName]])}
          outdata <- do_import(session,trials[my_i], input[[FileName]]$datapath,sheetlist,AlertName)
          data[[trials[my_i]]] <- outdata$data
          adafa[[trials[my_i]]] <- outdata$adafa
        })
      }) 
  } 

  #Build Options area - for plotting use
  #These options are largely the same for all tasks/trials
  #There are minor differences for different trials
  #There are defaults for these options, but they can be overwritten.
  ########
  output$S1Options1a <- renderUI({
    if(is.null(data$S1)) {return()}
    textInput(inputId="S1MainTitle", label="Main Title:", value="Data Summary")
  })
  output$S1Options1b <- renderUI({
    if(is.null(data$S1)) {return()}
    textInput(inputId="S1Subtitle", label="Subtitle:", value="Single Trial Line Plot")
  })
  output$S1Options2a <- renderUI({
    if(is.null(data$S1)) {return()}
    textInput(inputId="S1XLabel", label="X-Label:", value="Day after First Application")
  })
  output$S1Options2b <- renderUI({
    if(is.null(data$S1)) {return()}
    textInput(inputId="S1YLabel", label="Y-Label:", value=names(data$S1)[grep("Eval_Avg",names(data$S1))])
  })
  output$S1Options2c <- renderUI({
    if(is.null(data$S1)) {return()}
    textInput(inputId="S1PlotTitle", label="Plot Title:", value="Raw Data: Average Response vs.DAF")
  })
  
  ########
    
  #Report Options
  #1. Data extract Date
  output$ExtractDateUI <- renderUI({
    if(is.null(data$S2)) {return()}
    textInput(inputId="ExtractDate", label="Data Extract Date:", value=format(Sys.Date(), "%D"))
  })

  #2. Label for Y-axis, default label is "Response"
  output$label_ResponseUI <- renderUI({
    if(is.null(data$S2)) {return()}
    textInput(inputId="label_Response", label="Y-Label:",value="Response")
  })
  
  #3. Label for X-axis, default label is "Days After First Application"
  output$label_TimeXaxisUI <- renderUI({
    if(is.null(data$S2)) {return()}
    textInput(inputId="label_TimeXaxis", label="X-Label:",value="Days After First Application")
  })  
                      
  #4. Optional Additional Title to the title page
  output$TitlePageTitle1UI <- renderUI({
    if(is.null(data$S2)) {return()}
    textInput(inputId="TitlePageTitle1", label="Main Title:")
  })   
                    
  #5. Optional Additional Title to the title page
  output$TitlePageTitle2UI <- renderUI({
    if(is.null(data$S2)) {return()}
    textInput(inputId="TitlePageTitle2", label="Subtitle:")
  })    
  
  #6. User defined title1 for PDF output
  output$PDFHeader1UI <- renderUI({
    if(is.null(data$S2)) {return()}
    textInput(inputId="PDFHeader1", label="Plot Header1:")
  })                                
                             
  #7. User defined title2 for PDF output
  output$PDFHeader2UI <- renderUI({
    if(is.null(data$S2)) {return()}
    textInput(inputId="PDFHeader2", label="Plot Header2:")
  })   
  
  #8. User defined title3 for PDF output
  output$PDFHeader3UI <- renderUI({
    if(is.null(data$S2)) {return()}
    textInput(inputId="PDFHeader3", label="Plot Header3:")
  }) 

  #9. PDF author name
  output$PDFAuthorUI <- renderUI({
    if(is.null(data$S2)) {return()}
    textInput(inputId="PDFAuthor", label="PDF Author:",value=Sys.getenv("USERNAME"))
  }) 
 
  ########
  output$M1Options1a <- renderUI({
    if(is.null(data$M1)) {return()}
    textInput(inputId="M1MainTitle", label="Main Title:", value="Multi Trial Plot")
  })
  
  output$M1Options1b <- renderUI({
    if(is.null(data$M1)) {return()}
    textInput(inputId="M1Subtitle", label="Subtitle:", value="Mean Separation")
  })

  output$M1Options1c <- renderUI({
    if(is.null(data$M1)) {return()}
    checkboxInput("M1Confidential", label = h5("Add confidential label to footer?"), value = FALSE)
  })

  output$M1Options2a <- renderUI({
    if(is.null(data$M1)) {return()}
    textInput(inputId="M1YLabel", label="Response Axis Label:", value=names(data$M1)[grep("Eval_Avg",names(data$M1))])
  })
  
  output$M1Options2b <- renderUI({
    if(is.null(data$M1)) {return()}
    textInput(inputId="M1PlotTitle", label="Plot Title:", value=" ")
  })
  
  ########
  output$M2Options1a <- renderUI({
    if(is.null(data$M2)) {return()}
    textInput(inputId="M2MainTitle", label="Main Title:", value=paste0("Relative to ",input$M2select2eval))
  })
  
  output$M2Options1b <- renderUI({
    if(is.null(data$M2)) {return()}
    textInput(inputId="M2Subtitle", label="Subtitle:", value=" ")
  })
  
  output$M2Options1c <- renderUI({
    if(is.null(data$M2)) {return()}
    textInput(inputId="M2DataExtractionDate", label="Data Extraction Date:", value=format(Sys.Date(), "%Y"))
  })
  
  output$M2Options2a <- renderUI({
    if(is.null(data$M2)) {return()}
    textInput(inputId="M2XLabel", label="X-Label:", value="Treatment Average Response")
  })
  
  output$M2Options2b <- renderUI({
    if(is.null(data$M2)) {return()}
    textInput(inputId="M2YLabel", label="Y-Label:", value="Untreated")
  })
  
  output$M2Options2c <- renderUI({
    if(is.null(data$M2)) {return()}
    textInput(inputId="M2PlotTitle", label="Plot Title:", value=paste0("Relative to ",input$M2select2eval))
  })
  
  ########
  output$M3Options1a <- renderUI({
    if(is.null(data$M3)) {return()}
    textInput(inputId="M3MainTitle", label="Main Title:", value="Two-Response Plot")
  })
  
  output$M3Options1b <- renderUI({
    if(is.null(data$M3)) {return()}
    textInput(inputId="M3Subtitle", label="Subtitle:", value=" ")
  })

  output$M3Options2a <- renderUI({
    if(is.null(data$M3)) {return()}
    textInput(inputId="M3XLabel", label="X-Label:", value="DAF")
  })
  
  output$M3Options2b <- renderUI({
    if(is.null(data$M3)) {return()}
    textInput(inputId="M3YLabel1", label="Y1-Label:", value=input$M3select2eval)
  })
  
  output$M3Options2c <- renderUI({
    if(is.null(data$M3)) {return()}
    textInput(inputId="M3YLabel2", label="Y2-Label:", value=input$M3select2fval)
  })
  
  output$M3Options2d <- renderUI({
    if(is.null(data$M3)) {return()}
    textInput(inputId="M3PlotTitle", label="Plot Title:", value="Two-Response Plot")
  })
  
  output$M4Options1a <- renderUI({
    if(is.null(data$M4)) {return()}
    textInput(inputId="M4MainTitle", label="Main Title:", value="Heat Map")
  })
  
  ########
  output$M4Options1b <- renderUI({
    if(is.null(data$M4)) {return()}
    textInput(inputId="M4Subtitle", label="Subtitle:", value=" ")
  })

  output$M4Options2a <- renderUI({
    if(is.null(data$M4)) {return()}
    textInput(inputId="M4XLabel", label="X-Label:", value="DAF_Range")
  })
  
  output$M4Options2b <- renderUI({
    if(is.null(data$M4)) {return()}
    textInput(inputId="M4YLabel", label="Y-Label:", value="Trial_No_")
  })
  
  output$M4Options2c <- renderUI({
    if(is.null(data$M4)) {return()}
    textInput(inputId="M4PlotTitle", label="Plot Title:", value="Heat Map")
  })

  ########
  output$M5Options1a <- renderUI({
    if(is.null(data$M5)) {return()}
    textInput(inputId="M5MainTitle", label="Main Title:", value="Piano Plot")
  })
  
  output$M5Options1b <- renderUI({
    if(is.null(data$M5)) {return()}
    textInput(inputId="M5Subtitle", label="Subtitle:", value=" ")
  })
  
  output$M5Options1c <- renderUI({
    if(is.null(data$M5)) {return()}
    textInput(inputId="M5DataExtractionDate", label="Data Extraction Date:", value=format(Sys.Date(), "%Y"))
  })
  
  output$M5Options2a <- renderUI({
    if(is.null(data$M5)) {return()}
    textInput(inputId="M5XLabel", label="X-Label:", value="Trial No.")
  })
  
  output$M5Options2b <- renderUI({
    if(is.null(data$M5)) {return()}
    textInput(inputId="M5YLabel", label="Y-Label:", value="Treatment 1 - Treatment 2")
  })
  
  output$M5Options2c <- renderUI({
    if(is.null(data$M5)) {return()}
    textInput(inputId="M5PlotTitle", label="Plot Title:", value="Piano Chart")
  })
  
  ########
  output$M6Options1a <- renderUI({
    if(is.null(data$M6)) {return()}
    textInput(inputId="M6MainTitle", label="Main Title:", value=" ")
  })
  
  output$M6Options1b <- renderUI({
    if(is.null(data$M6)) {return()}
    textInput(inputId="M6Subtitle", label="Subtitle:", value=" ")
  })

  output$M6Options2a <- renderUI({
    if(is.null(data$M6)) {return()}
    textInput(inputId="M6ResponseLabel", label="Response Axis Label:", value="Response")
  })

  output$M6Options2b <- renderUI({
    if(is.null(data$M6)) {return()}
    textInput(inputId="M6PlotTitle", label="Plot Title:", value=" ")
  })

  ########
  output$M7Options1a <- renderUI({
    if(is.null(data$M7)) {return()}
    textInput(inputId="M7MainTitle", label="Main Title:", value="Spider Plot (Avg Response by Species)")
  })
  
  output$M7Options1b <- renderUI({
    if(is.null(data$M7)) {return()}
    Subtitle <- ''
    if (input$M7subsetval1 != "all") Subtitle <- paste(Subtitle,paste(input$M7subsetvar1, input$M7subsetopr1, input$M7subsetval1),sep='|')
    if (input$M7subsetval2 != "all") Subtitle <- paste(Subtitle,paste(input$M7subsetvar2, input$M7subsetopr2, input$M7subsetval2),sep='|')
    if (input$M7subsetval3 != "all") Subtitle <- paste(Subtitle,paste(input$M7subsetvar3, input$M7subsetopr3, input$M7subsetval3),sep='|')
    if (substring(Subtitle, 1, 1)=='|') Subtitle <- substring(Subtitle, 2) 
    textInput(inputId="M7Subtitle", label="Subtitle:", value=Subtitle)
  })

  ########
  output$M8Options1a <- renderUI({
    if(is.null(data$M8)) {return()}
    textInput(inputId="M8MainTitle", label="Main Title:", value=" ")
  })
  
  output$M8Options1b <- renderUI({
    if(is.null(data$M8)) {return()}
    textInput(inputId="M8Subtitle", label="Subtitle:", value=" ")
  })

  output$M8Options2a <- renderUI({
    if(is.null(data$M8)) {return()}
    textInput(inputId="M8ResponseLabel", label="Response Axis Label:", value="Response")
  })

  output$M8Options2b <- renderUI({
    if(is.null(data$M8)) {return()}
    textInput(inputId="M8PlotTitle", label="Plot Title:", value=" ")
  })


  #Build parameters.
  #This is the main column selection area for all trials.
  #Since different tasks/trials have different column selection requirement,
  #we display them individually. The numbers used in the variables names
  #match the numbers shown on UI screen.
  
  #Single Trials
  #1. Select columns for grouping within a trials
  #Default sort order is hard-coded here
  output$S1select1 <- renderUI({
    if(is.null(data$S1) | is.null(input$S1search1val)) return()
    else {
      closeAlert(session, "S1ImportAlert")
      div(style='height:300px; width:91%; overflow: scroll',
          checkboxGroupInput("S1select1val",h5(" "), 
                             choices =get_var_choices(selected$S1,data$S1, input$S1search1val,
                                                      c('Country','Crop','PestID','Symptom','Method','Basis','Part','SMBP')),
                             selected = isolate(input$S1select1val), inline=F)
      )
    }
  })

  output$S1select1a <- renderUI({
    if(is.null(data$S1)) {return()}
    selectInput("S1select1aval", label = h5("Select a Trial No"), width='100%',
                choices = c("all",levels(factor(data$S1$Trial_No_))), selected=input$S1select1aval)
  })
  
  #2. Select columns
  output$S1select2a <- renderUI({
    if(is.null(data$S1)) {return()}
    selectInput("S1select2aval", label = h5("X-axis"), width='100%',
                choices = names(data$S1), selected="DAF")
  })
  
  output$S1select2b <- renderUI({
    if(is.null(data$S1)) {return()}
    selectInput("S1select2bval", label = h5("Response"), width='100%',
                choices = names(data$S1), selected=names(data$S1)[grep("Eval_Avg",names(data$S1))])
  })
  
  output$S1select2c <- renderUI({
    if(is.null(data$S1)) {return()}
    selectInput("S1select2cval", label = h5("Color By (Treatment or Trt Type)"), width='100%',
                choices = names(data$S1), selected="Prog_Alias")
  })
  
  output$S1select2d <- renderUI({
    if(is.null(data$S1)) {return()}
    hidden(
      selectInput("S1select2dval", label = h5("Color"), width='100%',
                choices = names(data$S1), selected=input$S1select2cval)
    )
  })
  
  #4. Select Trt_Type
  output$S1select3a <- renderUI({
    if(is.null(data$S1)) {return()}
    selectInput("S1select3aval", label = h5("Column with Trt Type"), width='100%',
                choices = names(data$S1), selected="Trt_Type")
  })
  
  output$S1select3b <- renderUI({
    if(is.null(data$S1)) {return()}
      selectInput("S1select3bval", label = h5("Standard"), width='100%',
                choices = prioritylist(c('Stdrd'),levels(factor(dataS1()[,input$S1select3aval]))), selected=input$S1select3bval)
  })
  
  output$S1select3c <- renderUI({
    if(is.null(data$S1)) {return()}
    ####set to untrt or the first trt type
    hidden(
      selectInput("S1select3cval", label = h5("Untrt"), width='100%',
                choices = prioritylist(c('untrt'),levels(factor(dataS1()[,input$S1select3aval]))), selected=input$S1select3cval)
    )
  })
  
  output$S1select3d <- renderUI({
    if(is.null(data$S1)) {return()}
    selectInput("S1select3dval", label = h5("FMC Product"), width='100%',
                choices = prioritylist(c('DPX1'),levels(factor(dataS1()[,input$S1select3aval]))), selected="Trt_Type")
  })
  
  output$S1select4a <- renderUI({
    if(is.null(data$S1)) {return()}
    checkboxInput("S1select4aval", label = h5("Display Reference Line"), value = TRUE)
  })

  #AUDPC
  Chars <- reactive({
    colnames(dataS2())[sapply(dataS2(),class) %in%c("factor","character")]
  })

  Nums <- reactive({
    colnames(dataS2())[sapply(dataS2(),class) %in%c("numeric")]
  })
  
  #1. Grouping Variables
  output$S2select1 <- renderUI({
    if(is.null(dataS2()) | is.null(input$S2search1val)) return()
    else {
      closeAlert(session, "S2ImportAlert")
      div(style='height:300px; width:90%; overflow: scroll',
        checkboxGroupInput("S2select1val",h5("Grouping Variables"), 
        choices =get_var_choices(selected$S2,dataS2(), input$S2search1val,
                                 c('Country','Crop','PestID','Symptom','Method','Basis','Part','SMBP')),
        selected = isolate(input$S2select1val), inline=F)
      )
    }
  })  

  #Essential Variables

  #1. Treatment
  output$TreatmentUI <- renderUI({
    if(is.null(dataS2())) {return()}
    selectInput("S2select2aval", label = h5("Treatment"), width='80%',
                choices = names(dataS2()), selected="Prog_Alias")   
  })
  
  #2. Reponse
  output$ReponseUI <- renderUI({
    if(is.null(dataS2())) {return()}
    selectInput("Reponseval", label = h5("Response"), width='80%',
                choices = names(dataS2()), selected=ifelse(is.null(input$Reponseval),"Eval_Avg",input$Reponseval))       
  })
  
  #3. Trial
  output$TrialUI <- renderUI({
    if(is.null(dataS2())) {return()}
    selectInput("Trialval", label = h5("Trial"), width='80%',
                choices = names(dataS2()), selected=ifelse(is.null(input$Trialval),"Trial_No_",input$Trialval))       
  })

  #4. Rep
  output$RepUI <- renderUI({
    if(is.null(dataS2())) {return()}
    selectInput("Repval", label = h5("Rep"), width='80%',
                choices = names(dataS2()), selected=ifelse(is.null(input$Repval),"REP",input$Repval))   
  })

  #5. Untreated- used for calculating the Relative AUDPC
  output$UTCUI <- renderUI({
    if(is.null(dataS2())) {return()}
    if(is.null(input$S2select2aval)) {return()}
    UTCchoices <- unique(dataS2()[,input$S2select2aval])
    #UTCval <- ifelse(length(UTCchoices[grep("Untreated",UTCchoices)])==0,"UTC",UTCchoices[grep("Untreated",UTCchoices)])
    selectInput("UTCval", label = h5("Value for Untreated (UTC)"), width='80%', 
                choices = UTCchoices, 
                #selected=ifelse(is.null(input$UTCval),UTCval,input$UTCval))  
                selected=intersect(c('untreated','untreatedCK','untreated CK'),UTCchoices))  
  })

  #6. X axis Time variable used to plot the graph, e.g. DAF, DAA
  output$TimeXaxisUI <- renderUI({
    if(is.null(dataS2())) {return()}
    dfx <- data.frame(Col=Nums())
    TimeDefault <- sqldf("SELECT max(Col) FROM dfx WHERE Col LIKE 'DAA%' or Col LIKE 'DAF%' order by 1 desc")
    selectInput("TimeXaxisval", label = h5("X axis Time variable"), width='80%', 
                choices = Nums(), selected=ifelse(is.null(input$TimeXaxisval),TimeDefault,input$TimeXaxisval))   
  })
  
  #7. Set to 1 to order the RAUDPC table by ascending trt names. Default sort order by rel AUDPC
  #output$TableSortByTrtUI <- renderUI({
  #  if(is.null(dataS2())) {return()}
  #  checkboxInput("TableSortByTrtval", label = h5("Order Table by Treatment?"), value = ifelse(is.null(input$TableSortByTrtval),FALSE,input$TableSortByTrtval))    
  #})

  #7. Adjust font size for legend
  output$S2LegendFontSizeUI <- renderUI({
    if(is.null(dataS2())) {return()}
    sliderInput("S2LegendFontSize", label = h5(paste0("Adjust legend font size")),  
                min = 5, max = 10, value=7.5,step=.5)
  })

  #8. Name of the variable to be used as Treatment Type
  output$TrtTypeUI <- renderUI({
    if(is.null(dataS2())) {return()}
    selectInput("TrtTypeval", label = h5("Color By (Treatment or Trt Type)"), width='80%', 
                choices = names(dataS2()), selected="Trt_Type")  
  })

  #9. Value of Treatment Type to be labeled
  output$TrtTypeColorsUI <- renderUI({
    if(is.null(dataS2())) {return()}
    h5("Value of Treatment Type to be labeled in")
  })
  
  #10. Value of Treatment Type to be labeled in black, can be empty
  output$TrtBlackUI <- renderUI({
    if(is.null(dataS2())) {return()}
    if(is.null(input$TrtTypeval)) {return()}
    selectInput("TrtBlackval", label = h5("Black"), width='80%', 
                choices = c(" ",unique(dataS2()[,input$TrtTypeval])), selected="Untrt")  
  }) 
  #11. Value of Treatment Type to be labeled in green, can be empty
  output$TrtGreenUI <- renderUI({
    if(is.null(dataS2())) {return()}
    if(is.null(input$TrtTypeval)) {return()}
    selectInput("TrtGreenval", label = h5("Green"), width='80%', 
                choices = c(" ",unique(dataS2()[,input$TrtTypeval])), selected=" ")  
  })
  #12. Value of Treatment Type to be labeled in red, can be empty
  output$TrtRedUI <- renderUI({
    if(is.null(dataS2())) {return()}
    if(is.null(input$TrtTypeval)) {return()}
    selectInput("TrtRedval", label = h5("Red"), width='80%', 
                choices = c(" ",unique(dataS2()[,input$TrtTypeval])), selected="Stdrd")  
  })
  
  #13. Value of Treatment Type to be labeled in pulple, can be empty
  output$TrtPurpleUI <- renderUI({
    if(is.null(dataS2())) {return()}
    if(is.null(input$TrtTypeval)) {return()}
    selectInput("TrtPurpleval", label = h5("Purple"), width='80%', 
                choices = c(" ",unique(dataS2()[,input$TrtTypeval])), selected=" ")  
  })
  
  #14. Value of Treatment Type to be labeled in orange, can be empty
  output$TrtOrangeUI <- renderUI({
    if(is.null(dataS2())) {return()}
    if(is.null(input$TrtTypeval)) {return()}
    selectInput("TrtOrangeval", label = h5("Orange"), width='80%', 
                choices = c(" ",unique(dataS2()[,input$TrtTypeval])), selected=" ")  
  })
  
  #MultiTrials
  #Multi Trial Analysis 
  #1. Select columns for grouping trials
  output$M1select1 <- renderUI({
      if(is.null(data$M1) | is.null(input$M1search1val)) return()
      else {
        closeAlert(session, "M1ImportAlert")
        div(style='height:300px; width:91%; overflow: scroll',
            checkboxGroupInput("M1select1val",h5(" "), 
            choices =get_var_choices(selected$M1,data$M1, input$M1search1val,
                     c('Country','Crop','PestID','Symptom','Method','Basis','Part','SMBP','DAA_Range','DAF_Range')),
            selected = isolate(input$M1select1val), inline=F)
        )
      }
  })
  
  #2. Select columns
  output$M1select2a <- renderUI({
    if(is.null(data$M1)) {return()}
    selectInput("M1select2aval", label = h5("Treatment"), width='100%',
                choices = prioritylist(c("ProgAlias","Prog_Alias"),names(data$M1)))
  })

  output$M1select2b <- renderUI({
    if(is.null(data$M1)) {return()}
    selectInput("M1select2bval", label = h5("Response"), width='100%',
                choices = names(data$M1), selected=names(data$M1)[grep("Eval_Avg",names(data$M1))])
  })
  
  output$M1select2c <- renderUI({
    if(is.null(data$M1)) {return()}
    selectInput("M1select2cval", label = h5("Trial"), width='100%',
                choices = names(data$M1), selected="Trial_No_")
  })
  
  output$M1select2d <- renderUI({
    if(is.null(data$M1)) {return()}
    selectInput("M1select2dval", label = h5("Rep"), width='100%',
                choices = names(data$M1), selected="REP")
  })

  output$M1select2e <- renderUI({
    if(is.null(data$M1)) {return()}
    selectInput("M1select2eval", label = h5("Color by"), width='100%',
                choices = names(data$M1), selected="Trt_Type")
  })

  vals <- reactiveValues(authenticated=FALSE)
    
  passwordModal <- function(message=NULL) {
    modalDialog(
      passwordInput("password", "Password required to change significance level", input$password),
      if (!is.null(message)) div(tags$b(message, style="color: red;")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("authenticate", "OK")
      )
    )
  }  

  observeEvent(input$authenticate, {
    vals$authenticated <- FALSE
    if (!is.null(input$password) && nzchar(input$password)) {
      removeModal()    
      if (input$password == "ChangeSigLevel") {
        vals$authenticated <- TRUE
      } else {
        showModal(passwordModal(message="Incorrect password!"))
      }    
    } else {
      showModal(passwordModal(message="Please fill in your username and password"))
    }
  })
  
  output$M1select2f1 <- renderUI({
    if(is.null(data$M1)) {return()}
    actionButton(inputId="M1select2f1val", "Change significance level from default 95%") 
  })  

  observeEvent(input$M1select2f1val, {
    showModal(passwordModal())
  })
    
  output$M1select2f <- renderUI({
    if(is.null(data$M1)) {return()}
    if (vals$authenticated==FALSE) {
      hidden(
      radioButtons("M1select2fval", label = h5("Significance Level"),
                 choices = list("80%", "85%", "90%", "95%", "99%"), selected = "95%" , inline=TRUE)
      )
    } else {
      radioButtons("M1select2fval", label = h5("Significance Level"),
                 choices = list("80%", "85%", "90%", "95%", "99%"), selected = "95%" , inline=TRUE)
    }
  })
  
  output$M1select2g <- renderUI({
    if(is.null(data$M1)) {return()}
    #selectInput("M1select2gval", label = h5("Type"), width='100%', 
    #            choices = list("Normal", "Rating Scale", "Herbicide Visual Response (0-100)","Phyto"), selected="Normal")
    radioButtons("M1select2gval", label = h5("Type"),
                 choices = list("Normal", "Herbicide Visual Response (0-100)","Rating Scale", "Phyto"), selected = "Normal" , inline=TRUE)
  })
  
  output$M1select2h <- renderUI({
    if(is.null(data$M1)) {return()}
    if(is.null(input$M1select2gval)) {return()}
    if(input$M1select2gval!="Rating Scale") {return()}
    selectInput("M1select2hval", label = h5("If Rating, Which is better"), width='50%', 
                choices = c("Higher","Lower"), selected=input$M1select2hval)
  })
  
  output$M1select2i <- renderUI({
    if(is.null(data$M1)) {return()}
    if(is.null(input$M1select2gval)) {return()}
    if(input$M1select2gval!="Rating Scale") {return()}
    selectInput("M1select2ival", label = h5("If Rating, include mean separation"), width='50%', 
                choices = c("Yes","No"), selected="No")
  })

  output$M1select2j <- renderUI({
    if(is.null(data$M1)) {return()}
    if(is.null(input$M1select2gval)) {return()}
    if(input$M1select2gval!="Normal") {return()}
    selectInput("M1select2jval", label = h5("Plot Type"), width='50%', 
                choices = c("Box Plot","Bar Chart"), selected="Box Plot")
  })

  output$M1select2k <- renderUI({
    if(is.null(data$M1)) {return()}
    if(is.null(input$M1select2gval)) {return()}
    if(input$M1select2gval!="Normal") {return()}
    selectInput("M1select2kval", label = h5("Which is better for response"),  
                choices = c("Higher","Lower"), selected=input$M1select2kval)
  })
  
  output$M1select2n <- renderUI({
    if(is.null(data$M1)) {return()}
    #if (!(input$M1select2pval=="Custom" | input$M1select2pval=="herbicide")) {return()}
    #if(input$M1select2gval!="Herbicide Visual Response (0-100)" & input$M1select2gval!="Phyto") {return()}
    if (input$M1select2pval=="Custom") {
      textInput(inputId="M1select2nval", label=h5("Number of bins for reponses:"), value=6)
    } else {
      if (input$M1select2pval=="5 Bins") {
        hidden(textInput(inputId="M1select2nval", label=h5("Number of bins for reponses:"), value=5))
      } else {
        if (input$M1select2pval=="6 Bins") {
          hidden(textInput(inputId="M1select2nval", label=h5("Number of bins for reponses:"), value=6))
        }
      }
    }
  })
  
  Make.UI <- function(NoB){
    if (input$M1select2pval=="6 Bins") {
      S = c(100,95,85,70,60,0)  
      E = c(100,100,95,85,70,60)  
      L = c("1_100%","2_95-99%","3_85-94%","4_70-84%","5_60-69%","6_0-59%") 
      O1 = c(">=",">=",">=",">=",">=",">=")
      O2 = c("<=","<", "<", "<", "<", "<")
    }
    
    if (input$M1select2pval=="5 Bins") {
      S = c(95,85,70,50,0)  
      E = c(100,95,85,70,50)  
      L = c("1_95-100%","2_85-94%","3_70-84%","4_50-69%","5_0-50%") 
      O1 = c(">=",">=",">=",">=",">=")
      O2 = c("<=","<","<","<","<")
    }  
    
    if (input$M1select2gval=="Herbicide Visual Response (0-100)" & input$M1select2pval=="Custom") {
      S = c(95,85,70,50,0)  
      E = c(100,95,85,70,50)  
      L = c("1_95-100%","2_85-94%","3_70-84%","4_50-69%","5_0-50%") 
      O1 = c(">=",">=",">=",">=",">=")
      O2 = c("<=","<","<","<","<")
    }    
    
    if (input$M1select2gval=="Phyto" & input$M1select2pval=="5 Bins") {
      S = c(20,15,10,5,0)  
      E = c(100,20,15,10,5)  
      L = c("1_>20%","2_16-20%","3_11-15%","4_6-10%","5_0-5%") 
      O1 = c(">",">",">",">",">=")
      O2 = c("<=","<=","<=","<=","<=")
    }
    
    if (input$M1select2gval=="Phyto" & (input$M1select2pval=="6 Bins") | input$M1select2pval=="Custom") {
      S = c(20,15,10,5,0,0)  
      E = c(100,20,15,10,5,0)  
      L = c("1_>20%","2_16-20%","3_11-15%","4_6-10%","5_1-5%","0_0%") 
      O1 = c(">",">",">",">",">",">=")
      O2 = c("<=","<=","<=","<=","<=","<=")
    }
    
    opr1 = c(">=",">","=")
    opr2 = c("<","<=","=")
    #L = paste(1:length(E),paste0(paste(S,E,sep='-'),'%'),sep='_')  
    labs <- c("Greater Than","Lower limit","and Less than","Upper limit","Bin Label")
    output = tagList()    
    for(i in seq_along(1:NoB)){
      output[[i]] = tagList()
      if (i==1) labs <- c("Greater Than","Lower limit","and Less than","Upper limit","Bin Label")
      else labs <- c(NULL,NULL,NULL,NULL,NULL)
      output[[i]][[1]] = div(style="display: inline-block;vertical-align:top; width: 15%;", selectInput(inputId=paste0("O1",i), label =labs[1],choices=opr1,selected=O1[i]))
      output[[i]][[2]] = div(style="display: inline-block;vertical-align:top; width: 15%;", textInput(inputId=paste0("S",i), label =labs[2],value=S[i]))
      output[[i]][[3]] = div(style="display: inline-block;vertical-align:top; width: 15%;", selectInput(inputId=paste0("O2",i), label =labs[3],choices=opr2,selected=O2[i]))
      output[[i]][[4]] = div(style="display: inline-block;vertical-align:top; width: 15%;", textInput(inputId=paste0("E",i), label =labs[4],value=E[i]))
      output[[i]][[5]] = div(style="display: inline-block;vertical-align:top; width: 35%;", textInput(inputId=paste0("L",i), label =labs[5], value = L[i]))
    }  
    output
  }  
  
  output$M1select2o <- renderUI({
    if(is.null(data$M1)) {return()}
    members <- as.integer(input$M1select2nval) 
    #if(input$M1select2gval=="Herbicide Visual Response (0-100)" | input$M1select2gval=="Phyto") {
    if (input$M1select2pval=="Custom") {
      Make.UI(as.numeric(members))
    } else {
      hidden(
        Make.UI(as.numeric(members))
      )
    }
  }) 

  #3. Plot options
  
  output$M1select3a <- renderUI({
    if(is.null(data$M1)) {return()}
    if(input$M1select2gval!="Normal") {return()}
    radioButtons("M1select3aval", label = h5("Chart Orientation"),
                 choices = list("Vertical", "Horizontal"), selected = "Vertical" , inline=TRUE)
  })

  output$M1select3b <- renderUI({
    if(is.null(data$M1)) {return()} 
    if(input$M1select2gval!="Normal") {return()}
    checkboxInput("M1select3bval", label = h5("Show Estimated Means?"), value = FALSE)    
  })

  output$M1select3c <- renderUI({
    if(is.null(data$M1)) {return()}
    if(input$M1select2gval!="Normal") {return()}
    s <- font_size(data$M1[,input$M1select2aval])
    sliderInput("M1select3cval", label = h5(paste0("Adjust Treatment Label Font (Recommended font size: ",s,")")),  
                min = 9, max = 15, value=s,step=.5)
  })


  #Relative to Control Plot  
  #1. Select columns for grouping trials
  output$M2select1 <- renderUI({
      if(is.null(data$M2) | is.null(input$M2search1val)) return()
      else {
        closeAlert(session, "M2ImportAlert")
        div(style='height:300px; width:91%; overflow: scroll',
            checkboxGroupInput("M2select1val",h5(" "), 
            choices =get_var_choices(selected$M2,data$M2, input$M2search1val,
                     c('Country','Crop','PestID','Symptom','Method','Basis','Part','SMBP','DAA_Range','DAF_Range')),
            selected = isolate(input$M2select1val), inline=F)
        )
      }
  })

  output$M1select2p <- renderUI({
    if(is.null(data$M1)) {return()}
    if(input$M1select2gval!="Herbicide Visual Response (0-100)" & input$M1select2gval!="Phyto") {return()}
    selectInput("M1select2pval", label = h5("Binning Choices"), width='100%', 
                choices = c("6 Bins", "5 Bins", "Custom"))
  }) 

  output$M1select2q <- renderUI({
    if(is.null(data$M1)) {return()}
    if(is.null(input$M1select2gval)) {return()}
    if(is.null(input$M1select2pval)) {return()}
    if(input$M1select2gval=="Normal" ) {return()}
    if(input$M1select2gval=="Rating Scale" ) {return()}
    if(input$M1select2pval=="Custom") {return()}
    if(input$M1select2gval=="Phyto") {
      if(input$M1select2pval=="5 Bins")  
        tags$img(src=paste0(ImageDir,"/Phyto5.gif"), 
             width="600", height="500", alt="Image for the bin choice not found")
      else if(input$M1select2pval=="6 Bins") 
        tags$img(src=paste0(ImageDir,"/Phyto6.gif"), 
         width="600", height="500", alt="Image for the bin choice not found")                                   
    } 
    else {
      if(input$M1select2pval=="5 Bins") 
        tags$img(src=paste0(ImageDir,"/5bins.gif"), 
             width="600", height="500", alt="Image for the bin choice not found")
      else if(input$M1select2pval=="6 Bins") 
        tags$img(src=paste0(ImageDir,"/6bins.gif"), 
             width="600", height="500", alt="Image for the bin choice not found")
    }
  }) 
  
  observeEvent(input$M1select2pval,{
    if (input$M1select2pval=="5 Bins") {
      updateTextInput(session, 'M1select2nval', value = 5)
    }
    if (input$M1select2pval=="6 Bins") {
      updateTextInput(session, 'M1select2nval', value = 6)
    }
    if (input$M1select2pval=="5 Bins") {
      updateTextInput(session, 'M1select2nval', value = 5)
    }
    if (input$M1select2pval=="Custom" | input$M1select2pval=="6 Bins") {
      updateTextInput(session, 'M1select2nval', value = 6)
    }
  })

  
  #2. Select columns
  output$M2select2a <- renderUI({
    if(is.null(data$M2)) {return()}
    selectInput("M2select2aval", label = h5("Treatment"), width='100%',
                choices = names(data$M2),  selected="Prog_Alias")
  })
  
  output$M2select2b <- renderUI({
    if(is.null(data$M2)) {return()}
    selectInput("M2select2bval", label = h5("Response"), width='100%',
                choices = names(data$M2), selected=names(data$M2)[grep("Eval_Avg",names(data$M2))])
  })
  
  output$M2select2c <- renderUI({
    if(is.null(data$M2)) {return()}
    selectInput("M2select2cval", label = h5("Trial"), width='100%',
                choices = names(data$M2), selected="Trial_No_")
  })
  
  output$M2select2d <- renderUI({
    if(is.null(data$M2)) {return()}
    selectInput("M2select2dval", label = h5("Color by"), width='100%',
                choices = names(data$M2), selected="Trt_Type")
  })

  ## all treatments
  trt_control_M2 <-reactive({
    if(is.null(data$M2)) {return()}
    if(is.null(input$M2select2aval)) {return()}
    prioritylist(c('untreated','untrt'),levels(factor(data$M2[,input$M2select2aval])))
  })
  
  output$M2select2e <- renderUI({
    if(is.null(data$M2)) {return()}
    selectInput("M2select2eval", label = h5("Control"), width='80%', 
                choices = trt_control_M2(), selected=input$M2select2eval)
  })
  
  
  #Two-Response Plot
  #1. Select columns for grouping trials
  output$M3select1 <- renderUI({
      if(is.null(data$M3) | is.null(input$M3search1val)) return()
      else {
        closeAlert(session, "M3ImportAlert")
        div(style='height:300px; width:91%; overflow: scroll',
            checkboxGroupInput("M3select1val",h5(" "), 
            choices =get_var_choices(selected$M3,data$M3, input$M3search1val,
                     c('Country','Crop','PestID','Symptom','Method','Basis','Part','SMBP','DAA_Range','DAF_Range')),
            selected = isolate(input$M3select1val), inline=F)
        )
      }
  })
  
  #2. Select columns
  #output$M3select2a <- renderUI({
  #  if(is.null(data$M3)) {return()}
  #  selectInput("M3select2aval", label = h5("Treatment"), width='100%',
  #              choices = names(data$M3), selected="Prog_Alias")
  #})
  
  output$M3select2b <- renderUI({
    if(is.null(data$M3)) {return()}
    selectInput("M3select2bval", label = h5("Response"), width='100%',
                choices = names(data$M3), selected=names(data$M3)[grep("Response",names(data$M3))])
  })
  
  output$M3select2c <- renderUI({
    if(is.null(data$M3)) {return()}
    selectInput("M3select2cval", label = h5("Evaluation Timing"), width='100%',
                choices = names(data$M3), selected="DAF")
  })
  
  output$M3select2d <- renderUI({
    if(is.null(data$M3)) {return()}
    if ("Prog_Alias" %in% names(data$M3)) {
      VarSorted <- c("Prog_Alias", setdiff(names(data$M3),c("Prog_Alias")))
    }
    if ("Symptom" %in% VarSorted) {
      VarSorted <- c("Symptom", setdiff(VarSorted,c("Symptom")))
    }
    selectInput("M3select2dval", label = h5("Separated by (Symptom or Treatment)"), width='100%',
                choices = VarSorted, selected="Symptom")
  })
  
  output$M3select2e <- renderUI({
    if(is.null(data$M3)) {return()}
    if(is.null(input$M3select2dval)) {return()}
    selectInput("M3select2eval", label = h5("Group 1"), width='100%', 
                choices = levels(factor(data$M3[,input$M3select2dval])), selected=input$M3select2eval)
  })
  
  output$M3select2f <- renderUI({
    if(is.null(data$M3)) {return()}
    if(is.null(input$M3select2dval)) {return()}
    vals <- levels(factor(data$M3[,input$M3select2dval]))
    val2 <- ifelse(is.na(vals[2]), vals[1], vals[2])
    selectInput("M3select2fval", label = h5("Group 2"), width='100%', 
                choices = levels(factor(data$M3[,input$M3select2dval])), 
                selected=val2)
                #input$M3select2fval)
  })
  
  
  #Heat Map
  #1. Select columns for grouping trials
  output$M4select1 <- renderUI({
      if(is.null(data$M4) | is.null(input$M4search1val)) return()
      else {
        closeAlert(session, "M4ImportAlert")
        div(style='height:300px; width:91%; overflow: scroll',
            checkboxGroupInput("M4select1val",h5(" "), 
            choices =get_var_choices(selected$M4,data$M4, input$M4search1val,
                     c('Country','Crop','PestID','Symptom','Method','Basis','Part','SMBP','DAA_Range','DAF_Range')),
            selected = isolate(input$M4select1val), inline=F)
        )
      }
  })
  
  #2. Select columns
  output$M4select2a <- renderUI({
    if(is.null(data$M4)) {return()}
    selectInput("M4select2aval", label = h5("Treatment"), width='100%',
                choices = names(data$M4), selected="Prog_Alias")
  })
  
  output$M4select2b <- renderUI({
    if(is.null(data$M4)) {return()}
    selectInput("M4select2bval", label = h5("Response"), width='100%',
                choices = names(data$M4), selected=names(data$M4)[grep("Eval_Avg",names(data$M4))])
  })
  
  output$M4select2c <- renderUI({
    if(is.null(data$M4)) {return()}
    selectInput("M4select2cval", label = h5("Trial"), width='100%',
                choices = names(data$M4), selected="Trial_No_")
  })
  
  output$M4select2d <- renderUI({
    if(is.null(data$M4)) {return()}
    selectInput("M4select2dval", label = h5("Evaluation Timing"), width='100%',
                choices = names(data$M4), selected="DAF_Range")
  })
  
  trt_control_M4 <-reactive({
    if(is.null(data$M4)) {return()}
    if(is.null(input$M4select2aval)) {return()}
    prioritylist(c('untreated','untrt'),levels(factor(data$M4[,input$M4select2aval])))
  })
  
  output$M4select2e <- renderUI({
    if(is.null(data$M4)) {return()}
    if(is.null(input$M4select2aval)) {return()}
    selectInput("M4select2eval", label = h5("Display"), width='100%', 
                choices = trt_control_M4(), selected=input$M4select2eval)
  })

  #3. Plot options

  output$M4select3a <- renderUI({
    if(is.null(data$M4)) {return()}
    s <- font_size(data$M4[,input$M4select2aval])
    sliderInput("M4select3aval", label = h5(paste0("Adjust Treatment Label Font (Recommended font size: ",s,")")),  
                min = 9, max = 15, value=s,step=.5)
  })
  
  #Piano Plot
  #1. Select columns for grouping trials
  output$M5select1 <- renderUI({
      if(is.null(data$M5) | is.null(input$M5search1val)) return()
      else {
        closeAlert(session, "M5ImportAlert")
        div(style='height:300px; width:91%; overflow: scroll',
            checkboxGroupInput("M5select1val",h5(" "), 
            choices =get_var_choices(selected$M5,data$M5, input$M5search1val,
            c('Country','Crop','PestID','Symptom','Method','Basis','Part','SMBP','DAA_Range','DAF_Range')),
            selected = isolate(input$M5select1val), inline=F)
        )
      }

  })
  #2. Select columns
  output$M5select2a <- renderUI({
    if(is.null(data$M5)) {return()}
    selectInput("M5select2aval", label = h5("Treatment"), width='100%',
                choices = names(data$M5),selected="Prog_Alias")
  })
  
  output$M5select2b <- renderUI({
    if(is.null(data$M5)) {return()}
    selectInput("M5select2bval", label = h5("Response"), width='100%',
                choices = names(data$M5), selected=names(data$M5)[grep("Eval_Avg",names(data$M5))])
  })
  
  output$M5select2c <- renderUI({
    if(is.null(data$M5)) {return()}
    selectInput("M5select2cval", label = h5("Trial"), width='100%',
                choices = names(data$M5), selected="Trial_No_")
  })
  
  output$M5select2d <- renderUI({
    if(is.null(data$M5)) {return()}
    selectInput("M5select2dval", label = h5("Which is better for response"), width='50%', 
                choices = c("Higher","Lower"), selected=input$M5select2dval)
  })
  
  trt_control_M5 <-reactive({
    if(is.null(dataM5())) {return()}
    if(is.null(input$M5select2aval)) {return()}
    prioritylist(c('untreated','untrt'),levels(factor(dataM5()[,input$M5select2aval])))
  })
  
  output$M5select2e <- renderUI({
    if(is.null(data$M5)) {return()}
    if(is.null(input$M5select2aval)) {return()}
    selectInput("M5select2eval", label = h5("Treatment 1"), width='100%', 
                choices = trt_control_M5(), selected=input$M5select2eval)
  })

  output$M5select2f <- renderUI({
    if(is.null(data$M5)) {return()}
    if(is.null(input$M5select2aval)) {return()}
    selectInput("M5select2fval", label = h5("Treatment 2"), width='100%', 
                choices = trt_control_M5(), selected=input$M5select2fval)
  })

  #Boxplot
  #1. Select columns for grouping trials
  output$M6select1 <- renderUI({
      if(is.null(data$M6) | is.null(input$M6search1val)) return()
      else {
        closeAlert(session, "M6ImportAlert")
        div(style='height:300px; width:91%; overflow: scroll',
            checkboxGroupInput("M6select1val",h5(" "), 
            choices =get_var_choices(selected$M6,data$M6, input$M6search1val,
                     c('Country','Crop','PestID','Symptom','Method','Basis','Part','SMBP','DAA_Range','DAF_Range')),
            selected = isolate(input$M6select1val), inline=F)
        )
      }
  })  
  
  #2. Select columns
  output$M6select2a <- renderUI({
    if(is.null(data$M6)) {return()}
    selectInput("M6select2aval", label = h5("Treatment"), width='100%',
                choices = prioritylist(c("ProgAlias","Prog_Alias"),names(data$M6)))
  })
  
  output$M6select2b <- renderUI({
    if(is.null(data$M6)) {return()}
    selectInput("M6select2bval", label = h5("Response"), width='100%',
                choices = names(data$M6), selected=names(data$M6)[grep("Eval_Avg",names(data$M6))])
  })

  output$M6select2c <- renderUI({
    if(is.null(data$M6)) {return()}
    selectizeInput('M6select2cval', label = h5('Select new treatment order'),
        choices = dataM6()[,input$M6select2aval], multiple = TRUE)
  })
  
  
  output$M6select2d <- renderUI({
    if(is.null(data$M6)) {return()}
    selectInput("M6select2dval", label = h5("Color by"), width='100%',
                choices = names(data$M6), selected="Trt_Type")
  })

  #3. Plot optionss
  
  output$M6select3b <- renderUI({
    if(is.null(data$M6)) {return()}
    radioButtons("M6select3bval", label = h5("Chart Orientation"),
                 choices = list("Vertical", "Horizontal"), selected = "Vertical" , inline=TRUE)
  })

  output$M6select3c <- renderUI({
    if(is.null(data$M6)) {return()}
    sliderInput("M6select3cval", label = h5("Adjust Treatment Label Font"),
                min = 9, max = 15, value=font_size(data$M6[,input$M6select2aval]),step=.5)
  })
  
  output$M6select3d <- renderUI({
    if(is.null(data$M6)) {return()}
    #if(input$M6select3aval!="Box Plot") {return()}
    checkboxInput("M6select3dval", label = h5("Show all outliers?"), value = TRUE)    
  })

 
  #Spider plot
  #1. Select columns for grouping trials
  
  output$M7select1a <- renderUI({
    if(is.null(data$M7)) {return()}
    h5("Do not pick Pest as a column for grouping trials.")
  })
  
  output$M7select1 <- renderUI({
      if(is.null(data$M7) | is.null(input$M7search1val)) return()
      else {
        closeAlert(session, "M7ImportAlert")
        varchoices <- get_var_choices(selected$M7,data$M7, input$M7search1val,
                 c('Country','Crop','PestID','Symptom','Method','Basis','Part','SMBP','DAA_Range','DAF_Range'))
        varchoices <- varchoices[sapply(varchoices,class) %in%c("factor","character")]
        div(style='height:300px; width:91%; overflow: scroll',
            checkboxGroupInput("M7select1val",h5(" "), 
            choices = varchoices,
            selected = isolate(input$M7select1val), inline=F)
        )
      }
  })  
  
  #1. Select chart options
  output$M7select2f <- renderUI({
    if(is.null(data$M7)) {return()}
    selectInput("M7select2fval", label = h5("Chart Type"), width='100%', 
                choices = list("Plot for Each Treatment", "Compare Treatments"))
  })
  
  output$M7select2a <- renderUI({
    if(is.null(data$M7)) {return()}
    selectInput("M7select2aval", label = h5("Treatment"), width='100%',
                choices = names(data$M7), selected="Prog_Alias")
  })

  output$M7select2b <- renderUI({
    if(is.null(data$M7)) {return()}
    selectInput("M7select2bval", label = h5("Spoke(Species)"), width='100%',
                choices = names(data$M7), selected="Species")
  })
  
  output$M7select2c <- renderUI({
    if(is.null(data$M7)) {return()}
    selectInput("M7select2cval", label = h5("Response"), width='100%',
                choices = names(data$M7), selected=ifelse(length(names(data$M7)[grep("Eval_Avg",names(data$M7))])==0,"Response",names(data$M7)[grep("Eval_Avg",names(data$M7))]))
  })

  output$M7select2e <- renderUI({
    if(is.null(data$M7)) {return()}
    sliderInput("M7select2eval", label = h5("Adjust Species Name Font"), width='100%',
                min = 5, max = 15, value=font_size(data$M7[,input$M7select2bval]),step=.5)
  })

  output$M7select2g <- renderUI({
    if(is.null(data$M7)) {return()}
    selectInput("M7select2gval", label = h5("Trials"), width='100%',
                choices = names(data$M7), selected=ifelse(length(names(data$M7)[grep("Trial_No_",names(data$M7))])==0,"Trial",names(data$M7)[grep("Trial_No_",names(data$M7))]))
  })

  trt_control_M7 <-reactive({
    if(is.null(dataM7())) {return()}
    if(is.null(input$M7select2aval)) {return()}
    levels(factor(dataM7()[,input$M7select2aval]))
  })

  output$M7select2d <- renderUI({
    if(is.null(data$M7)) {return()}
    if(is.null(input$M7select2aval)) {return()}
    if(input$M7select2fval=="Plot for Each Treatment") {return()}
    div(style='height:300px; width:90%; overflow: scroll',
      checkboxGroupInput("M7select2dval",h5(" "), choices =trt_control_M7(), inline=F)
    )
  })

  #Bar Chart
  #1. Select columns for grouping trials
  output$M8select1a <- renderUI({
      if(is.null(data$M8) | is.null(input$M8search1val)) return()
      else {
        closeAlert(session, "M8ImportAlert")
        div(style='height:300px; width:91%; overflow: scroll',
            checkboxGroupInput("M8select1aval",h5(" "), 
            choices =get_var_choices(selected$M8,data$M8, input$M8search1val,
                     c('Country','Crop','PestID','Symptom','Method','Basis','Part','SMBP','DAA_Range','DAF_Range')),
            selected = isolate(input$M8select1aval), inline=F)
        )
      }
  })  

  #2. Select columns
  output$M8select2a <- renderUI({
    if(is.null(data$M8)) {return()}
    selectInput("M8select2aval", label = h5("Treatment"), width='100%',
                choices = prioritylist(c("ProgAlias","Prog_Alias"),names(data$M8)))
  })
  
  output$M8select2b <- renderUI({
    if(is.null(data$M8)) {return()}
    selectInput("M8select2bval", label = h5("Response"), width='100%',
                choices = names(data$M8), selected=names(data$M8)[grep("Eval_Avg",names(data$M8))])
  })
  
  output$M8select2d <- renderUI({
    if(is.null(data$M8)) {return()}
    selectInput("M8select2dval", label = h5("Color by"), width='100%',
                choices = names(data$M8), selected="Trt_Type")
  })  


  #3. Plot options

  output$M8select3a <- renderUI({
    if(is.null(data$M8)) {return()}
    radioButtons("M8select3aval", label = h5("Single or grouped"),
                 choices = list("Single", "Grouped"), selected = "Single" , inline=TRUE)
  })
  
  output$M8select3b <- renderUI({
    if(is.null(data$M8)) {return()}
    radioButtons("M8select3bval", label = h5("Chart Orientation"),
                 choices = list("Vertical", "Horizontal"), selected = "Vertical" , inline=TRUE)
  })

  output$M8select3c <- renderUI({
    if(is.null(data$M8)) {return()}
    s <- font_size(data$M8[,input$M8select2aval])
    sliderInput("M8select3cval", label =h5(paste0("Adjust Treatment Label Font (Recommended font size: ",s,")")),  
                min = 9, max = 15, value=s,step=.5)
  })

  output$M8select3d <- renderUI({
    if(is.null(data$M8)) {return()}
    radioButtons("M8select3dval", label = h5("Type of error bar"),
                 choices = list("Confidence Interval", "Standard Error"), selected = "Confidence Interval" , inline=TRUE)   
  })

  output$M8select3e <- renderUI({
    if(is.null(data$M8)) {return()}
    radioButtons("M8select3eval", label = h5("Statistics to display"),
                 choices = list("mean", "sum", "max"), selected = "mean" , inline=TRUE)   
  })
 
  output$M8select3f <- renderUI({
    if(is.null(data$M8)) {return()}
    if(is.null(input$M8select3aval)) {return()}
    if(input$M8select3aval!="Grouped") {return()}
    selectInput("M8select3fval", label = h5("Grouped by"), width='100%',
                choices =setdiff(names(data$M8),input$M8select1aval))
  })



  #Create plot buttons
  #Loops through all trials because they all need the same button
  for (i in 1:length(trials)) { 
      local({ 
        my_i <- i 
        outputName <- paste0(trials[my_i], "PlotButtonUI") 
        IdName <- paste0(trials[my_i], "CreatePlot") 
        output[[outputName]] <- renderUI({
          if(is.null(data[[trials[my_i]]])) return()
          else actionButton(inputId=IdName, "Create Plot") 
        }) 
      }) 
  } 

               
  #Create plots for all trials (S1,S2, M1-M8)
  #Each plot calls a separate function in a separate R program.
  #These separate R programs are included/sourced into this program at the top
  #Please refer to each individual R program for parameters needed to call he functions
  #Some fields are required for the function call, therefore, if they are missing, then
  #the system issues an alert and no plot will be generated.
  #OutputDir is define globally at the top of the program, because it is common for all plot.
  
  #Single line plot, call function in singlelineplot.R  
  observeEvent(input$S1CreatePlot,{
    AlertName <- "S1PlotAlert"
    if(is.null(input$S1select1val)) {
      shinyjs::alert("Columns for grouping trials should be selected!")
	  try(closeAlert(session, AlertName))
      return()
    }
    assign("adafaS1",adafa$S1,envir = .GlobalEnv)
    if(!'ADAFA' %in% colnames(adafa$S1)) {
      shinyjs::alert("No ADAFA column in ADAFA sheet!")
      try(closeAlert(session, AlertName))
      return()
    }
    indata <- dataS1()
    #If missing response values in analysis, delete those rows (only impact Excel and ARM data). EDS will not have missing response values. 
    indata <-indata[!is.na(indata[,input$S1select2bval]),] 
    GroupVars <- input$S1select1val
    for (i in 1:length(GroupVars)) {
      indata[is.na(indata[,GroupVars[i]]),GroupVars[i]] <- "N/A"
    }
    if(is.null(data$S1) | is.null(adafa$S1)) {
      shinyjs::alert("Please wait for data preparation to complete!")
      try(closeAlert(session, AlertName))
      return()
    }
    datadevar=c('Trial_No_','Treat_No_')
    adafadevar=c('Trial_No_','Treat_No_','ADAFA')
    assign("data_S1",indata,envir = .GlobalEnv)
    assign("adafa_S1",adafaS1(),envir = .GlobalEnv)
    datacol=match(toupper(datadevar),toupper(colnames(indata)))
    adafacol=match(toupper(adafadevar),toupper(colnames(adafaS1())))
    adafa_varname=colnames(adafaS1())[adafacol]
    data_varname=colnames(indata)[datacol]
    pdfloopvar <- vector()
    if (input$S1subsetval1 != "all")  pdfloopvar < c(pdfloopvar,input$S1subsetvar1)
    if (input$S1subsetval2 != "all")  pdfloopvar < c(pdfloopvar,input$S1subsetvar2)
    if (input$S1subsetval3 != "all")  pdfloopvar < c(pdfloopvar,input$S1subsetvar3)
    try({
    out1 <- single.pdfplot(data=indata,   
                     adafa=adafaS1(),
                     adafa_varname=adafa_varname,
                     data_varname=data_varname,
                     loopvar=c(unique(input$S1select1val),"Trial_No_"),
                     x=input$S1select2aval,
                     y=input$S1select2bval,
                     groupby=input$S1select2cval,
                     colorby=input$S1select2dval,
                     trt_type_name=c(input$S1select3aval),
                     trt_type=c(input$S1select3bval,input$S1select3cval,input$S1select3dval),
                     ShowADAFA=input$S1select4aval,
                     OutputURL=OutputDir,
                     X_label=input$S1XLabel,
                     Y_label=input$S1YLabel,
                     maintitle=input$S1MainTitle,
                     plot_title=input$S1PlotTitle,
                     subtitle=input$S1Subtitle)   
    })
    URL$S1 <-out1$url
    pdfname$S1 <-out1$pdfname
    closeAlert(session, AlertName)
    shinyjs::alert("Plot created!")
    updateTabsetPanel(session, "tabsetS1", selected = "Plot")
  })  
  
  observeEvent(input$S1CreatePlot,{
    output$S1plotUI <- renderUI({
      if(is.null(dataS1()) | is.null(URL$S1) | !file.exists(URL$S1)) return()
      if(is.null(input$S1select1val)) {
        shinyjs::alert("Columns for grouping trials should be selected!")
        return()
      }
      else {
        if(is.null(URL$S1)) return()
        else tags$iframe(style="height:720px; width:100%; scrolling=yes", src=URL$S1)  
      }  
    })
  })


  
  #AUDPC, call function in AUDPC.R
  observeEvent(input$S2CreatePlot,{
    AlertName <- "S2PlotAlert"
    if(is.null(input$S2select1val)) {
      shinyjs::alert("Columns for grouping trials should be selected!")
	  closeAlert(session, AlertName)
      return()
    }
    indata <- dataS2()
    #If missing response values in analysis, delete those rows (only impact Excel and ARM data). EDS will not have missing response values. 
    indata <-indata[!is.na(indata[,input$Reponseval]),] 
    GroupVars <- c(input$S2select1val,input$TrtTypeval)
    for (i in 1:length(GroupVars)) {
      indata[is.na(indata[,GroupVars[i]]),GroupVars[i]] <- "N/A"
    }
    
    selected_colors <- c(input$TrtBlackval, input$TrtGreenval, input$TrtRedval, input$TrtPurpleval, input$TrtOrangeval)
    if (!all(table(selected_colors[selected_colors>" "])==1)) {
      shinyjs::alert("Please select distinct treatment type colors!")
	  closeAlert(session, AlertName)
      return()
    }

    out <- PDFCreate(indata, 
		loopvar=input$S2select1val,
		Trial=input$Trialval,
		Treatment=input$S2select2aval,
		Rep = input$Repval, 
		UTC = input$UTCval,
		TimeXaxis =input$TimeXaxisval,
		Response= input$Reponseval,
		TrtType= input$TrtTypeval,
		TrtBlack = input$TrtBlackval,
		TrtGreen = input$TrtGreenval,
		TrtRed = input$TrtRedval,
		TrtPurple = input$TrtPurpleval,
		TrtOrange = input$TrtOrangeval,
		#TableSortByTrt=input$TableSortByTrtval,
		OutputURL=OutputDir, 	
		TitlePageTitle1=input$TitlePageTitle1,
		TitlePageTitle2=input$TitlePageTitle2,
		xLabel= input$label_TimeXaxis,
		yLabel= input$label_Response,
		PDFHeader1=input$PDFHeader1,
		PDFHeader2=input$PDFHeader2,
		PDFHeader3=input$PDFHeader3,
		LegendFontSize=input$S2LegendFontSize)
    assign("out",out,envir = .GlobalEnv)
    URL$S2 <-out$url
    pdfname$S2 <-out$pdfname
    data$calculated <- out$calculated_df
    closeAlert(session, AlertName)
    shinyjs::alert("Plot created!")
    updateTabsetPanel(session, "tabsetS2", selected = "Plot")
    output$S2calculated_data <- renderDataTable({
      out$calculated_df
    })
  })  
  
  observeEvent(input$S2CreatePlot,{
    output$S2plotUI <- renderUI({
      if(is.null(dataS2())) return()
      if(is.null(input$S2select1val)) {
        shinyjs::alert("Columns for grouping trials should be selected!")
        return()
      }
      else {
        if(is.null(URL$S2)) return()
        else tags$iframe(style="height:720px; width:100%; scrolling=yes", src=URL$S2)  
      }  
    })
  })
  
  #Download calculated data
  output$downloadData <- downloadHandler(
    filename = function() {'calculated.xlsx'},
    content = function(file) {write.xlsx(data$calculated, file, col.names=TRUE, row.names=FALSE, append=FALSE)
  })  
  
  output$S2downloaddataUI <- renderUI({
    if(is.null(dataS2()) || is.null(data$calculated)) return()
    colnames(data$calculated)<-gsub(' ','_',colnames(data$calculated))
    colnames(data$calculated)<-gsub('\n','_',colnames(data$calculated))
    colnames(data$calculated)<-gsub('__','_',colnames(data$calculated))
    assign("datacalculated",data$calculated, envir = .GlobalEnv)   
    try(downloadButton('downloadData', 'Download Data'))
  })

  #Multitrial plots (Box plot, Bar chart, or Rating plot)
  #Call function in multiplot.R and save to pdf file at location OutputDir
  observeEvent(input$M1CreatePlot,{
    if(is.null(dataM1())) {return()}  
	AlertName <- "M1PlotAlert"
    if(is.null(input$M1select1val)) {
      shinyjs::alert("Columns for grouping trials should be selected!")
	  closeAlert(session, AlertName) 
      return()
    }
    indata <- data_renamedM1()
    #If missing response values in analysis, delete those rows (only impact Excel and ARM data). EDS will not have missing response values. 
    indata <-indata[!is.na(indata[,input$M1select2bval]),] 
    GroupVars <- input$M1select1val
    for (i in 1:length(GroupVars)) {
      indata[is.na(indata[,GroupVars[i]]),GroupVars[i]] <- "N/A"
    }
    
    try({
      trt <- indata[,input$M1select2aval]    
      if(any(nchar(trt)>96)){
        Nchar_message <- paste0("Warnings:","\n","\n","Please either exclude long treatment names using Filter or rename to names shorter than 96 characters.")
        shinyjs::alert(Nchar_message) 
        closeAlert(session, AlertName) 
        return()
      }
    })  

    if(input$M1select2gval=="Herbicide Visual Response (0-100)" | input$M1select2gval=="Phyto") {
      response_range <- range(indata[,input$M1select2bval])
      #if (response_range != c(0,100)) {#TESTING ONLY
      if (response_range[1]<0 | response_range[2]>100) {#TESTING
        Nchar_message <- paste0("Warnings:","\n","\n","Herbicide Visual Response (0-100) should contain responses in the range of 0 to 100. Please filter data to include only rows needed for analysis.")
        shinyjs::alert(Nchar_message) 
        closeAlert(session, AlertName) 
        return()
      }
    }
    
    #For Type=rating, confirm that input column to be used for response in the analysis are integers.  If not, please round to an integer before inputting into the multinomial model.   
    if(input$M1select2gval=="Rating Scale") {
      if (!all(round(indata[,input$M1select2bval])==indata[,input$M1select2bval])) { 
        NonInt_message <- paste0("Warnings:","\n","\n","Not all values in the input column to be used for response in the analysis are integers. The responses will be rounded to integers.")
        shinyjs::alert(NonInt_message) 
        closeAlert(session, AlertName) 
        return()
      }
    }
    
    if(input$M1select2gval=="Herbicide Visual Response (0-100)" | input$M1select2gval=="Phyto") {
      if (input$M1select2pval=="5 Bins") {
        xticks <- c(input$L1,input$L2,input$L3,input$L4,input$L5)
      }
      if (input$M1select2pval=="6 Bins") {
        xticks <- c(input$L1,input$L2,input$L3,input$L4,input$L5,input$L6)
      }
      if (input$M1select2pval=="Custom" | input$M1select2pval=="5 Bins") {
        xticks <- c(input$L1,input$L2,input$L3,input$L4,input$L5)
      }
    }
    assign("indata",indata, envir = .GlobalEnv)
    assign("datahotM1",datahot$M1(), envir = .GlobalEnv)
    assign("Plot_Sort_OrderM1",Plot_Sort_Order$M1, envir = .GlobalEnv)
    out1 <- multi.pdfplot(data=indata,
            pdfloopvar=vector(),
            loopvar=unique(input$M1select1val),
            x=input$M1select2aval,
            y=input$M1select2bval,
            Plot_Sort_Order = Plot_Sort_Order$M1,  
            trial=input$M1select2cval,
            alphalevel=1-as.numeric(gsub("[% ]","",input$M1select2fval))/100,
            response_type=input$M1select2gval,
            normal_better=input$M1select2kval,
            rating_better=input$M1select2hval, 
            modeled=ifelse(input$M1select2ival=='Yes',TRUE,FALSE),
            rep=input$M1select2dval,
            colorby=input$M1select2eval,
            Orientation=input$M1select3aval,
            ShowEstMean=input$M1select3bval,
            trt_label_font=input$M1select3cval,
            OutputURL=OutputDir,
            X_label="",
            Y_label=input$M1YLabel,
            pdf_plot_title=input$M1MainTitle,
            plot_title=input$M1PlotTitle,
            subtitle=input$M1Subtitle,
            plot_type=input$M1select2jval,
            significance=input$M1select2fval,
            xticks=xticks,
            Confidential=input$M1Confidential
            )
    URL$M1 <-out1$url
    pdfname$M1 <-out1$pdfname
    #showModal(modalDialog(title = "pdfname$M1= ", pdfname$M1))
    closeAlert(session, AlertName)
    shinyjs::alert("Plot created!")
    updateTabsetPanel(session, "tabsetM1", selected = "Plot")
  })  

  
  #Create plot UI  
  observeEvent(input$M1CreatePlot,{
    output$M1plotUI <- renderUI({
      if(is.null(dataM1())) return()
      if(is.null(input$M1select1val)) {
        shinyjs::alert("Columns for grouping trials should be selected!")
        return()
      }
      else {
        if(is.null(URL$M1)) return()
        else tags$iframe(style="height:720px; width:100%; scrolling=yes", src=URL$M1)  
      }  
    })
  })
  
  #Relative to control plot, call function in relativeplot.R
  observeEvent(input$M2CreatePlot,{
    AlertName <- "M2PlotAlert"
    if(is.null(input$M2select1val)) {
      shinyjs::alert("Columns for grouping trials should be selected!")
	  closeAlert(session, AlertName) 
      return()
    }
    indata <- dataM2()
    #If missing response values in analysis, delete those rows (only impact Excel and ARM data). EDS will not have missing response values. 
    indata <-indata[!is.na(indata[,input$M2select2bval]),] 
    GroupVars <- input$M2select1val
    for (i in 1:length(GroupVars)) {
      indata[is.na(indata[,GroupVars[i]]),GroupVars[i]] <- "N/A"
    }
    trt <- indata[,input$M2select2aval]    
    variablelist=colnames(indata)
    repprioritylist=c('rep')
    repvariablelist=prisort(repprioritylist,variablelist)
    rep=repvariablelist[1]
    out2 <- relative.pdfplot(data=indata,
                     pdfloopvar=vector(),
                     loopvar=unique(input$M2select1val),
                     x=input$M2select2aval,
                     y=input$M2select2bval,
                     trial=input$M2select2cval,
                     rep=rep,
                     colorby=input$M2select2dval,
                     control1=input$M2select2eval,
                     OutputURL=OutputDir,
                     X_label=input$M2XLabel,
                     Y_label=input$M2YLabel,
                     pdf_plot_title=input$M2MainTitle,
                     plot_title=input$M2PlotTitle,
                     subtitle=input$M2Subtitle,
                     DataDate=input$M2DataExtractionDate)   
    URL$M2 <-out2$url
    pdfname$M2 <-out2$pdfname
    closeAlert(session, AlertName)
    shinyjs::alert("Plot created!")
    updateTabsetPanel(session, "tabsetM2", selected = "Plot")
  })  
  
  observeEvent(input$M2CreatePlot,{
    output$M2plotUI <- renderUI({
      if(is.null(dataM2())) return()
      if(is.null(input$M2select1val)) {
        shinyjs::alert("Columns for grouping trials should be selected!")
        return()
      }
      else {
        if(is.null(URL$M2)) return()
        else tags$iframe(style="height:720px; width:100%; scrolling=yes", src=URL$M2)  
      }  
    })
  })
  
  #Two-Response plot, call function in tworesponseplot.R 
  observeEvent(input$M3CreatePlot,{
    AlertName <- "M3PlotAlert"
    if(is.null(input$M3select1val)) {
      shinyjs::alert("Columns for grouping trials should be selected!")
	  closeAlert(session, AlertName)
      return()
    }
    indata <- dataM3()
    #If missing response values in analysis, delete those rows (only impact Excel and ARM data). EDS will not have missing response values. 
    indata <-indata[!is.na(indata[,input$M3select2bval]),] 
    GroupVars <- input$M3select1val
    for (i in 1:length(GroupVars)) {
      indata[is.na(indata[,GroupVars[i]]),GroupVars[i]] <- "N/A"
    }
    out3 <- tworesponse.pdfplot(data=indata,
                     pdfloopvar=vector(),
                     loopvar=unique(input$M3select1val),
                     y=input$M3select2bval,
                     DAF=input$M3select2cval,
                     sep=input$M3select2dval,
                     se1=input$M3select2eval,
                     se2=input$M3select2fval,
                     OutputURL=OutputDir,
                     X_label=input$M3XLabel,
                     Y_label1=input$M3YLabel1,
                     Y_label2=input$M3YLabel2,
                     pdf_plot_title=input$M3MainTitle,
                     plot_title=input$M3PlotTitle,
                     subtitle=input$M3Subtitle) 
    URL$M3 <-out3$url
    pdfname$M3 <-out3$pdfname
    closeAlert(session, AlertName)
    shinyjs::alert("Plot created!")
    updateTabsetPanel(session, "tabsetM3", selected = "Plot")
  })  
  
  observeEvent(input$M3CreatePlot,{
    output$M3plotUI <- renderUI({
      if(is.null(dataM3())) return()
      if(is.null(input$M3select1val)) {
        shinyjs::alert("Columns for grouping trials should be selected!")
        return()
      }
      else {
        if(is.null(URL$M3)) return()
        else tags$iframe(style="height:720px; width:100%; scrolling=yes", src=URL$M3)  
      }  
    })
  })
  
  #Heatmap, call function in heatmap.R 
  observeEvent(input$M4CreatePlot,{
    AlertName <- "M4PlotAlert"
    if(is.null(input$M4select1val)) {
      shinyjs::alert("Columns for grouping trials should be selected!")
	  closeAlert(session, AlertName) 
      return()
    }
    indata <- dataM4()
    #If missing response values in analysis, delete those rows (only impact Excel and ARM data). EDS will not have missing response values. 
    indata <-indata[!is.na(indata[,input$M4select2bval]),] 
    GroupVars <- input$M4select1val
    for (i in 1:length(GroupVars)) {
      indata[is.na(indata[,GroupVars[i]]),GroupVars[i]] <- "N/A"
    }
    trt <- indata[,input$M4select2aval]    
    if(any(nchar(trt)>96)){
      Nchar_message <- paste0("Warnings:","\n","\n","Treatment greater than 96 characters. Please select a different field as treatment, or return to Excel and shorten the long treatment names.")
      shinyjs::alert(Nchar_message) 
      closeAlert(session, AlertName) 
      return()
    }
    variablelist=colnames(indata)
    repprioritylist=c('rep')
    repvariablelist=prisort(repprioritylist,variablelist)
    rep=repvariablelist[1]
    out4 <- heatmap.pdfplot(data=indata,
                     loopvar=unique(input$M4select1val),
                     x=input$M4select2aval,
                     y=input$M4select2bval,
                     #Plot_Sort_Order = datahot$M4()[order(datahot$M4()$Plot_Sort_Order),input$M4select2aval],
                     trial=input$M4select2cval,
                     rep=rep,
                     DAFRange=input$M4select2dval,
                     control1=input$M4select2eval,
                     OutputURL=OutputDir,
                     X_label=input$M4XLabel,
                     Y_label=input$M4YLabel,
                     pdf_plot_title=input$M4MainTitle,
                     plot_title=input$M4PlotTitle,
                     subtitle=input$M4Subtitle)   
    URL$M4 <-out4$url
    pdfname$M4 <-out4$pdfname
    closeAlert(session, "M4PlotAlert")
    shinyjs::alert("Plot created!")
    updateTabsetPanel(session, "tabsetM4", selected = "Plot")
  })  
  
  observeEvent(input$M4CreatePlot,{
    output$M4plotUI <- renderUI({
      if(is.null(dataM4())) return()
      if(is.null(input$M4select1val)) {
        shinyjs::alert("Columns for grouping trials should be selected!")
        return()
      }
      else {
        if(is.null(URL$M4)) return()
        else tags$iframe(style="height:720px; width:100%; scrolling=yes", src=URL$M4)  
      }  
    })
  })

  #Piano plot, call function in pianochart.R    
  observeEvent(input$M5CreatePlot,{
    AlertName <- "M5PlotAlert"
    if(is.null(input$M5select1val)) {
      shinyjs::alert("Columns for grouping trials should be selected!")
	  closeAlert(session, AlertName)
      return()
    }
    indata <- dataM5()
    #If missing response values in analysis, delete those rows (only impact Excel and ARM data). EDS will not have missing response values. 
    indata <-indata[!is.na(indata[,input$M5select2bval]),] 
    GroupVars <- input$M5select1val
    for (i in 1:length(GroupVars)) {
      indata[is.na(indata[,GroupVars[i]]),GroupVars[i]] <- "N/A"
    }
    out5 <- piano.pdfplot(data=indata,
                     pdfloopvar=vector(),
                     loopvar=unique(input$M5select1val),
                     x=input$M5select2aval,
                     y=input$M5select2bval,
                     trial=input$M5select2cval,
                     det=input$M5select2dval,
                     trt1=input$M5select2eval,
                     trt2=input$M5select2fval,
                     OutputURL=OutputDir,
                     pdf_plot_title=input$M5MainTitle,
                     plot_title=input$M5PlotTitle,
                     subtitle=input$M5Subtitle,
                     DataDate=input$M5DataExtractionDate) 
    URL$M5 <-out5$url
    pdfname$M5 <-out5$pdfname
    closeAlert(session, AlertName)
    shinyjs::alert("Plot created!")
    updateTabsetPanel(session, "tabsetM5", selected = "Plot")
  })  
  
  observeEvent(input$M5CreatePlot,{
    output$M5plotUI <- renderUI({
      if(is.null(dataM5())) return()
      if(is.null(input$M5select1val)) {
        shinyjs::alert("Columns for grouping trials should be selected!")
        return()
      }
      else {
        if(is.null(URL$M5)) return()
        else tags$iframe(style="height:720px; width:100%; scrolling=yes", src=URL$M5)  
      }  
    })
  })
  
  #Box plot, call function in boxplotbarchart.R
  observeEvent(input$M6CreatePlot,{
    AlertName <- "M6PlotAlert"
    if(is.null(input$M6select1val)) {
      shinyjs::alert("Columns for grouping trials should be selected!")
	  closeAlert(session, AlertName)
      return()
    }
    indata <- data_renamedM6()
    #If missing response values in analysis, delete those rows (only impact Excel and ARM data). EDS will not have missing response values. 
    indata <-indata[!is.na(indata[,input$M6select2bval]),] 
    GroupVars <- input$M6select1val
    for (i in 1:length(GroupVars)) {
      indata[is.na(indata[,GroupVars[i]]),GroupVars[i]] <- "N/A"
    }
    out6 <- box.barchart.pdfplot(data=indata,
                     loopvar=unique(input$M6select1val),
                     x=input$M6select2aval,
                     y=input$M6select2bval,
                     Plot_Sort_Order = Plot_Sort_Order$M6,  
                     single_grouped='Single',
                     grouped_var='',
                     plot_type='Box Plot',            #input$M6select3aval,
                     outlier=input$M6select3dval,
                     ErrorBarType='',
                     Statistics='',
                     colorby=input$M6select2dval,
                     Orientation=input$M6select3bval,
                     OutputURL=OutputDir,
                     pdf_plot_title=input$M6MainTitle,
                     plot_title=input$M6PlotTitle,
                     subtitle=input$M6Subtitle,
                     trt_label_font=input$M6select3cval,
                     ResponseLabel=input$M6ResponseLabel)                 
    URL$M6 <-out6$url
    pdfname$M6 <-out6$pdfname
    closeAlert(session, AlertName)
    shinyjs::alert("Plot created!")
    updateTabsetPanel(session, "tabsetM6", selected = "Plot")
  })  
  
  observeEvent(input$M6CreatePlot,{
    output$M6plotUI <- renderUI({
      if(is.null(dataM6())) return()
      if(is.null(input$M6select1val)) {
        shinyjs::alert("Columns for grouping trials should be selected!")
        return()
      }
      else {
        if(is.null(URL$M6)) return()
        else tags$iframe(style="height:720px; width:100%; scrolling=yes", src=URL$M6)  
      }  
    })
  })

  #Spider plot, call function in radarplot.R
  observeEvent(input$M7CreatePlot,{
    indata <- dataM7()
	AlertName <- "M7PlotAlert"
    #If missing response values in analysis, delete those rows (only impact Excel and ARM data). EDS will not have missing response values. 
    indata <-indata[!is.na(indata[,input$M7select2cval]),] 
    GroupVars <- input$M7select1val
    for (i in 1:length(GroupVars)) {
      indata[is.na(indata[,GroupVars[i]]),GroupVars[i]] <- "N/A"
    }
    spoke <- indata[,input$M7select2aval]    
    if(any(nchar(spoke)>96)){
      Nchar_message <- paste0("Warnings:","\n","\n","Spoke column greater than 96 characters. Please select a different field as spoke, or return to Excel and shorten the long spoke names.")
      shinyjs::alert(Nchar_message) 
      closeAlert(session, AlertName) 
      return()
    } 
    if(is.null(input$M7select1val)) {
      shinyjs::alert("Columns for grouping trials should be selected!")
      closeAlert(session, AlertName) 
      return()
    }
    if(input$M7select2aval %in% input$M7select1val) {
      shinyjs::alert("Columns for grouping trials can not contain the selected treatment variable!")
      closeAlert(session, AlertName) 
      return()
    }
    if (input$M7select2fval=="Plot for Each Treatment") {
      out7 <- radar.pdfplot(data=indata,
                     loopvar=unique(c(input$M7select1val,input$M7select2aval)),
                     ChartType=input$M7select2fval,
                     x=input$M7select2bval,
                     y=input$M7select2cval,
                     trial=input$M7select2gval, 
                     treatment=input$M7select2aval, 
                     treatments=input$M7select2dval,
                     SpeciesFont=input$M7select2eval,
                     OutputURL=OutputDir,
                     pdf_plot_title=input$M7MainTitle,
                     subtitle=input$M7Subtitle)  
    } else {
      if(length(input$M7select2dval)<1) {
        shinyjs::alert("At least 1 treatment to compare should be selected!")
		closeAlert(session, AlertName) 
        return()
      }
      data7 <- subset(indata,(get(input$M7select2aval) %in% c(input$M7select2dval,input$M7select2dval)))
      assign("data7",data7,envir = .GlobalEnv)
      out7 <- radar.pdfplot(data=data7,
                     loopvar=unique(c(input$M7select1val,input$M7select2aval)),
                     ChartType=input$M7select2fval,
                     x=input$M7select2bval,
                     y=input$M7select2cval,
                     trial=input$M7select2gval, 
                     treatment=input$M7select2aval, 
                     treatments=input$M7select2dval,
                     SpeciesFont=input$M7select2eval,
                     OutputURL=OutputDir,
                     pdf_plot_title=input$M7MainTitle,
                     subtitle=input$M7Subtitle) 
    }                
    URL$M7 <-out7$url
    pdfname$M7 <-out7$pdfname
    closeAlert(session, AlertName) 
    shinyjs::alert("Plot created!")
    updateTabsetPanel(session, "tabsetM7", selected = "Plot")
  })  
  
  observeEvent(input$M7CreatePlot,{
    output$M7plotUI <- renderUI({
      if(is.null(dataM7())) return()
      if(is.null(input$M7select1val)) {
        shinyjs::alert("Columns for grouping trials should be selected!")
        return()
      } 
      else {
        if(is.null(URL$M7)) return()
        else tags$iframe(style="height:720px; width:100%; scrolling=yes", src=URL$M7)  
      }  
    })
  })

  #Bar Chart, call function in boxplotbarchart.R
  observeEvent(input$M8CreatePlot,{
    AlertName <- "M8PlotAlert"
    if(is.null(input$M8select1aval)) {
      shinyjs::alert("Columns for grouping trials should be selected!")
	  closeAlert(session, AlertName)
      return()
    }
    indata <- dataM8()
    #If missing response values in analysis, delete those rows (only impact Excel and ARM data). EDS will not have missing response values. 
    indata <-indata[!is.na(indata[,input$M8select2bval]),] 
    #If missing values in grouping columns, insert N/A and continue to run
    GroupVars <- input$M8select1aval
    for (i in 1:length(GroupVars)) {
      indata[is.na(indata[,GroupVars[i]]),GroupVars[i]] <- "N/A"
    }
    assign("indata", indata ,envir = .GlobalEnv)
    assign("datahotM8", datahot$M8() ,envir = .GlobalEnv)
    assign("M8select2aval", input$M8select2aval ,envir = .GlobalEnv)
    out8 <- box.barchart.pdfplot(data=indata,
                     loopvar=unique(input$M8select1aval),
                     x=input$M8select2aval,
                     y=input$M8select2bval,
                     Plot_Sort_Order = datahot$M8()[order(datahot$M8()$Plot_Sort_Order),input$M8select2aval],   
                     single_grouped=input$M8select3aval,
                     grouped_var=input$M8select3fval,
                     plot_type='Bar Chart',            
                     outlier=F,
                     ErrorBarType=input$M8select3dval,
                     Statistics=input$M8select3eval,
                     colorby=input$M8select2dval,
                     Orientation=input$M8select3bval,
                     OutputURL=OutputDir,
                     pdf_plot_title=input$M8MainTitle,
                     plot_title=input$M8PlotTitle,
                     subtitle=input$M8Subtitle,
                     trt_label_font=input$M8select3cval,
                     ResponseLabel=input$M8ResponseLabel)                 
    URL$M8 <-out8$url
    pdfname$M8 <-out8$pdfname
    closeAlert(session, AlertName)
    shinyjs::alert("Plot created!")
    updateTabsetPanel(session, "tabsetM8", selected = "Plot")
  })  
  
  observeEvent(input$M8CreatePlot,{
    output$M8plotUI <- renderUI({
      if(is.null(dataM8())) return()
      if(is.null(input$M8select1aval)) {
        shinyjs::alert("Columns for grouping trials should be selected!")
        return()
      }
      else {
        if(is.null(URL$M8)) return()
        else tags$iframe(style="height:720px; width:100%; scrolling=yes", src=URL$M8)  
      }  
    })
  })


  #Example Plots, stored in /image subfolder.
  #Image file names are set as the vector examples.
  for (i in 1:length(examples)) { 
    local({ 
      my_i <- i 
      UIName <- paste0("ExampleUI", my_i) 
      ButtonName <- paste0("ExampleButton", my_i) 
      srcname <-paste0(ImageDir,"/",examples[my_i])
      output[[UIName]] <- renderUI({ 
        if (input[[ButtonName]]%%2==1) {
          tags$img(src= srcname ,
	           width="900" ,
	           height="700" ,
	           alt="Image not found")
        }
        else return()
      }) 
    }) 
  } 

  
  #Loops through all trials because they all need the same button
  for (i in 1:length(trials)) { 
      local({ 
        my_i <- i 
        #Download buttons
        outputSource <- paste0(trials[my_i], "downloadSource") 
        dataName <- paste0("data",trials[my_i])  
        data_renamedName <- paste0("data_renamed",trials[my_i])  
        if (trials[my_i] %in% trials_trt) dsName <- data_renamedName ##Renamed
        else dsName <- dataName
        outputUI <- paste0(trials[my_i], "downloadsourceUI") 
        
        output[[outputSource]] <- downloadHandler(
          filename = function() {"downloaded.xlsx"},
          content = function(file) {write.xlsx(get(dsName)(), file, col.names=TRUE, row.names=FALSE, append=FALSE)
        })   
        
        output[[outputUI]] <- renderUI({
          if(is.null(get(dataName)())) return()
          try(downloadButton(outputSource, 'Download Data'))
        })

        #Download/Save buttons, prompts users to select a folder and file name
        downloadID <- paste0(trials[my_i],'download')
        
        output[[downloadID]] <- downloadHandler(
          contentType="image/png",
          filename = paste0(pdfname[[trials[my_i]]],".pdf"),
          content = function(file) {
            file.copy(URL[[trials[my_i]]],file)
          }
        )
        
        observeEvent(input[[paste0(trials[my_i],"CreatePlot")]],{
          output[[paste0(trials[my_i],"downloadUI")]] <- renderUI({
          if(is.null(URL[[trials[my_i]]])) {return( )}
          try(downloadButton(downloadID, 'Download/Save'))
          })
        }) 
      })
  } 

  #Start rename processing
  # Custom renderer function
  color_renderer <- "
    function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      td.style.background = value;
    }
  "
  
  #Loops through all trials with treatments to create Treatment Rename tables    
  for (i in 1:length(trials_trt)) { 
    local({ 
      my_i <- i 
      TrtID <- paste0(trials_trt[my_i], "select2aval") 
      TrtTypeID <- paste0(trials_trt[my_i], "select2dval") ##?????
      dataName <- paste0("data",trials_trt[my_i])  
      data_renamedName <- paste0("data_renamed",trials_trt[my_i]) 
      OutputID <- paste0("hot",trials_trt[my_i]) 
      RenameID <- paste0(trials_trt[my_i],"Rename") 
      RenameUI <- paste0(trials_trt[my_i],"renameUI")
      RenameYNID <- paste0(trials_trt[my_i],"RenameYN") 
      RenameYNUI <- paste0(trials_trt[my_i],"RenameYNUI") 
      ColorID <- paste0(trials_trt[my_i],"Color") 
      ColorUI <- paste0(trials_trt[my_i],"ColorUI")
      ColorByID <- paste0(trials_trt[my_i],"ColorBy") 
      ColorByUI <- paste0(trials_trt[my_i],"ColorByUI")
      hot_select <- paste0("hot",trials_trt[my_i],"_select") 
	  FileName <- paste0("file", trials_trt[my_i])
      values = reactiveValues()

      trt_levels = reactive({
        if(is.null(get(data_renamedName)())) {return()}  
        unique(get(data_renamedName)()[,input[[TrtID]]])
      })                  

      trt_type_levels = reactive({
        if(is.null(get(dataName)())) {return()}  
        unique(get(dataName)()[,input[[TrtTypeID]]])
      })   
      
      datahot[[trials_trt[my_i]]] = reactive({
        if (!is.null(input[[OutputID]])) {
          DF = hot_to_r(input[[OutputID]])
        } else {
          if (is.null(values[["DF"]])) {
            #DF = data.frame(Original_Treatment=trt_levels(),
            #                Treatment=trt_levels(),
            #                Plot_Sort_Order=1:length(trt_levels()),
            #                Colors=fmc_colors()[1],
            #                stringsAsFactors=F)
			DF =data.frame()
          } else
            DF = values[["DF"]]
        }
        values[["DF"]] = DF
        #colnames(DF)[1] <- paste0(input[[TrtID]],"1")
        #colnames(DF)[2] <- input[[TrtID]]
        DF
      })

      output[[OutputID]] <- renderRHandsontable({
        if(is.null(data[[trials_trt[my_i]]])) {return()}
        if(input[[RenameYN]]==FALSE) {return()}
        DF = datahot[[trials_trt[my_i]]]()
        if (!is.null(DF))
          rhandsontable(DF, stretchH = "all",readOnly=F, useTypes = FALSE, selectCallback = TRUE) %>%
            hot_cols(columnSorting = TRUE) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE)  
      })


      output[[RenameYNUI]] <- renderUI({
        if(is.null(data[[trials_trt[my_i]]])) {return()}
        radioButtons(RenameYNID, label = h5("Do you want to rename Treatments?"),
                 choices = list("Yes", "No"), selected = "No" , inline=TRUE)
      })
  
      output[[ColorByUI]] <- renderUI({
        if(is.null(data[[trials_trt[my_i]]])) {return()}
        hidden(######V1.2
        radioButtons(ColorByID, label = h5("Do you want to color Plots by Treatment or use/edit Trt Type for that purpose?"),
                 choices = list("By Treatment", "Use Trt Type"), selected = "By Treatment" , inline=TRUE)
        )######V1.2
      })
      
      output[[ColorUI]] <- renderUI({
        if(is.null(data[[trials_trt[my_i]]])) {return()}
        colourInput(ColorID, h5("Choose a color"), palette = "limited",allowedCols=fmc_colors())
      }) 
      
      output[[RenameUI]] <- renderUI({
        if(is.null(data[[trials_trt[my_i]]])) {return()}
        actionButton(inputId=RenameID, "Rename/Reorder Treatment") 
      }) 

      observeEvent(c(input[[TrtID]], input[[FileName]], input[[RenameYNID]]),{                      
        output[[OutputID]] <- renderRHandsontable({
          if(is.null(get(dataName)())) {return()}
          v <- unique(isolate(get(data_renamedName)())[,input[[TrtID]]])
          v1 <- unique(isolate(get(data_renamedName)())[,paste0(input[[TrtID]],"1")])
          if (is.null(trt_selected[[trials_trt[my_i]]])) {
            New_Plot_Sort_Order <- 1:length(v)
          } else {
            if (trt_selected[[trials_trt[my_i]]] != input[[TrtID]]) {
              New_Plot_Sort_Order <- 1:length(v)
            } else {
              New_Plot_Sort_Order <- c()
              for (i in 1:length(v)) {
                New_Plot_Sort_Order[i] <- max(isolate(get(data_renamedName)())[isolate(get(data_renamedName)())[,input[[TrtID]]]==v[i],"Plot_Sort_Order"])
              }
            }
          }
          if (input[[ColorByID]]=="By Treatment") {
            DF = data.frame(Original_Treatment=v1,
                          Treatment=v,
                          Plot_Sort_Order=New_Plot_Sort_Order,
                          #Colors=fmc_colors()[1:length(v)],  ######V1.2
                          stringsAsFactors=F)       
          } else {
            DF = data.frame(Original_Treatment=v1,
                          Treatment=v,
                          Plot_Sort_Order=New_Plot_Sort_Order,
                          Trt_Type=trt_type_levels(),
                          Color_By_Grp=trt_type_levels(),
                          Colors=fmc_colors()[1:length(v)],
                          stringsAsFactors=F)       
          } 
          
          colnames(DF)[1] <- paste0(input[[TrtID]],"1")
          colnames(DF)[2] <- input[[TrtID]]
          DF <- DF[order(DF$Plot_Sort_Order),] 
          if (!is.null(DF))
		  rhandsontable(DF, stretchH = "all",readOnly=F, useTypes = FALSE, selectCallback = TRUE) %>%
		    hot_col(1, readOnly = TRUE) %>%
		    hot_col(1, readOnly = TRUE) %>%
		    hot_cols(columnSorting = TRUE) %>%
		    hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%  	 
                    #hot_cell(1, 4, "Enter color code") %>%							######V1.2
  		    #hot_col("Colors", renderer = color_renderer) %>% #Download CSV right-click menu item	######V1.2
		    hot_context_menu(
		      customOpts = list(
		        csv = list(name = "Download to CSV",
			  callback = htmlwidgets::JS(
			      "function (key, options) {
			         var csv = csvString(this, sep=',', dec='.');
				 var link = document.createElement('a');
				 link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
				   encodeURIComponent(csv));
				 link.setAttribute('download', 'data.csv');
				 document.body.appendChild(link);
				 link.click();
				 document.body.removeChild(link);
		      }")))) 
        })
      })


      output$debug <- renderPrint({
        str(input[[hot_select]]$select[[1]])
      })  



      observeEvent(input[[RenameID]], {  
        #Order check
        if(length(unique(datahot[[trials_trt[my_i]]]()[,input[[TrtID]]])) !=length(unique(datahot[[trials_trt[my_i]]]()[,"Plot_Sort_Order"]))) {
          shinyjs::alert("New treatment order should be unique!")
          return()
        }
        #Rename and reorder treatements
        df1 <- datahot[[trials_trt[my_i]]]()
        df2 <- data[[trials_trt[my_i]]][,colnames(data[[trials_trt[my_i]]])!='Plot_Sort_Order']
        df1$key <- df1[,paste0(input[[TrtID]],"1")]
        df2$key <- df2[,input[[TrtID]]]
        df2 <- df2[,colnames(df2)!=input[[TrtID]]]
        df <- merge(df1,df2, by="key")
		assign('df1',df1,envir=.GlobalEnv)
		assign('df2',df2,envir=.GlobalEnv)
		assign('df',df,envir=.GlobalEnv)
        data_renamed[[trials_trt[my_i]]] <- df[,!(colnames(df) %in% c('key'))]
        #colnames(data[[trials_trt[my_i]]])[1] <- input[[TrtID]]
        trt_selected[[trials_trt[my_i]]] <- input[[TrtID]]
        Plot_Sort_Order[[trials_trt[my_i]]] <- df1[order(df1$Plot_Sort_Order),input[[TrtID]]]
        shinyjs::alert("Treatment names have been changed and sort order in plots defined. In Data tab original treatments now in Prog_Alias1 column. Renamed treatment in Prog_Alias column. You may save updated data table from Data tab.")
      })
    }) 
  }

  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}


#Some useful commands to run the application 
#require(shiny)
#setwd('E:/REVEAL20180126/RScript')
#source('server.R')
#source('ui.R')
#shinyApp(ui, server)
#runApp(host="0.0.0.0",port=5050)
#http://10.237.149.27:5050/
#runApp(host="0.0.0.0",port=5050)
#http://HWWRND2.DDNET1.NET:5050/
#Access R app http://rtpvmntctx07d.ddnet1.net/Citrix/XenApp/site/default.aspx  
#mstsc rtpvmntctx02d.ddnet1.net.ddnet1.net  prod
#http://rtpvmntctx02d.ddnet1.net.ddnet1.net:5050/
#mstsc rtpvmntctx02d.ddnet1.net  dev
#http://rtpvmntctx02d.ddnet1.net:5060/

#REFERENCES
#https://stackoverflow.com/questions/40477484/inserting-control-inputs-and-html-widgets-inside-rhandsontable-cells-in-shiny