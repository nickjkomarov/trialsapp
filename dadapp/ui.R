#########################################################################
#Purpose: Define User Interface for the trial application               #
#Created: 4/20/2017                                                     # 
#Naming:  S1-Single Line Plot                                           # 
#         S2-AUDPC                                                      #  
#         M1-Multi Trial Analysis                                       # 
#         M2-Relative to Control                                        # 
#         M3-Two-Response Plot                                          # 
#         M4-Heat Map                                                   # 
#         M5-Piano Chart                                                # 
#         M6-Box Plot                                                   # 
#         M7-Spider Plot                                                # 
#         M8-Bar Chart                                                  # 
#Setup:   All R programs should be in the same application home         #
#         directory on the server.                                      # 
#         All image files should be gif files in the image sub folder   #
#         of the application home directory on the server. These gif    # 
#         image files are used in the MoreInfo section as examples.     #
#         There should be sub folder named output under the application #
#         home directory on the server.                                 #
#Note:    Widths are defined as percents to be flexible for all screens.#
#         resolutions.                                                  #
#         The main area on the right side of the screen are devided     #
#         into smaller box areas, to group file imports, subsetting,    #
#         column selections, report options logically.                  #
#         The output subfolder is used to store plots created before    #
#         it is saved or downloaded to the desired location. The file   #
#         names have timestamps appended to ensure uniqueness and to    #
#         avoid contention.                                             #                                     
#########################################################################
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinydashboard)
library(htmltools)

#HomeDir="\\\\rtpvmntctx02d.ddnet1.net/Trials App"
HomeDir="\\\\sbyddng3240a/HSB-FMC/Research_and_Development/common/641-psr-biostat/xiaopeijin"
#HomeDir="C:/Users/xjin/E/RScript/RScript"
HomeDir="C:/Users/Nick/Desktop/Dads app files" 
ImageDir=paste0(HomeDir,"/image")

ui <- dashboardPage(
  dashboardHeader(title = "Reveal"),
#Defines a dashboard layout with 4 memu items on the left side.
#The 3rd menu item has 7 sub-menu items.
  dashboardSidebar(
      sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "OVERVIEW", icon = icon("table")),
      menuItem("Single Trial", tabName = "Single", icon = icon("line-chart"),
      menuSubItem("Single Trial Plot", tabName = "S1Plot",href = NULL, newtab = TRUE,   icon = shiny::icon("angle-double-right"), selected = NULL),
      menuSubItem("AUDPC Plot/Calculations", tabName = "S2Plot",href = NULL, newtab = TRUE,   icon = shiny::icon("angle-double-right"), selected = NULL)
      ),
      menuItem("Multiple Trials", tabName = "Multiple", icon = icon("bar-chart-o"),
      menuSubItem("Multi Trial Analysis", tabName = "M1Plot",href = NULL, newtab = TRUE,   icon = shiny::icon("angle-double-right"), selected = NULL),
      menuSubItem("Heat Map", tabName = "M4Plot",href = NULL, newtab = TRUE,   icon = shiny::icon("angle-double-right"), selected = NULL),
      menuSubItem("Piano Plot", tabName = "M5Plot",href = NULL, newtab = TRUE,   icon = shiny::icon("angle-double-right"), selected = NULL),
      menuSubItem("Spider Plot", tabName = "M7Plot",href = NULL, newtab = TRUE,   icon = shiny::icon("angle-double-right"), selected = NULL),
      menuSubItem("Boxplot", tabName = "M6Plot",href = NULL, newtab = TRUE,   icon = shiny::icon("angle-double-right"), selected = NULL),
      #menuSubItem("Barchart", tabName = "M8Plot",href = NULL, newtab = TRUE,   icon = shiny::icon("angle-double-right"), selected = NULL),	###V1.2
      menuSubItem("Two-Response Plot", tabName = "M3Plot",href = NULL, newtab = TRUE,   icon = shiny::icon("angle-double-right"), selected = NULL) 
      #menuSubItem("Relative to ... Plot", tabName = "M2Plot",href = NULL, newtab = TRUE,   icon = shiny::icon("angle-double-right"), selected = NULL)
      ),
      menuItem("More Info", tabName = "MoreInfo", icon = icon("list-alt"))
    )
  ),
#Defines a dashboard layout with 2 tabs on the right side.
  dashboardBody(
    useShinyjs(),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: visible; content: 'An error occurred. Please contact the admin.'; }"
    ),
    tabItems(
      #Application OVERVIEW
      tabItem(tabName = "OVERVIEW",
          tags$img(src=paste0(ImageDir,"/overview.gif"),
	           width="100%" ,
	           height="100%",
	           alt="Image not found")
      ),  
      
      #Single Line Plot tab content
      tabItem(tabName = "S1Plot",
  div(style='height:100%; width:100%; overflow: scroll',    
              fluidRow(
                tabBox(
                  title = "Single Trial Line Plot",
                  id = "tabsetS1",  width=12,
                  tabPanel("Selections", 
                           fluidRow( 
                             box(
                               title = "1. Choose an xlsx file",
                               width=7,
                               fileInput("fileS1", label = "",accept=".xlsx"),
                               uiOutput("S1PlotButtonUI"),
                               bsAlert("S1PlotAlertID"),
                               br(),
                               uiOutput("S1datasheet1UI"),
			       div(style="display: inline-block;vertical-align:top; width: 45%;", uiOutput("S1datasheetUI")),
			       div(style="display: inline-block;vertical-align:top; width: 45%;", uiOutput("S1adafasheetUI")),
			       div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("S1subsetvar1UI")),
			       div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("S1subsetval1UI")),
			       div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("S1subsetopr1UI")),
			       div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("S1subsetvar2UI")),
			       div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("S1subsetval2UI")),
			       div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("S1subsetopr2UI")),
			       div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("S1subsetvar3UI")),
			       div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("S1subsetval3UI")),
			       div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("S1subsetopr3UI"))
                             ),
                             box(
                               title ="Options",
                               width=5,
                               collapsible = FALSE,
                               collapsed=TRUE,
                               h4("Title Page Setting"),
                               uiOutput("S1Options1a"),
                               uiOutput("S1Options1b"),
                               h4(" Label Setting"),
                               uiOutput("S1Options2a"),
                               uiOutput("S1Options2b"),
                               uiOutput("S1Options2c")
                             )
                           ),
                           br(),
                           fluidRow( 
                             box(
                               title = "2. Select columns for grouping within a trial",
                               width = 5,
                               bsAlert("S1ImportAlertID"),
                               uiOutput("S1select1")
                             ),
                             box(
                               title = "Column search",
                               width=6,
                               uiOutput("S1search1"),
                               br(),
                               br(),
                               uiOutput("S1select1a")
                             )
                           ),
                           br(),
                           fluidRow( 
                             box(
                               title = "3. Select columns",
                               width = 5,
                               uiOutput("S1select2a"),
                               uiOutput("S1select2b"),
                               uiOutput("S1select2c"),
                               uiOutput("S1select2d")
                             ),
                             box( 
                               title = "For Coloring Application Reference Lines",
                               width=6,
                               uiOutput("S1select4a"),
                               uiOutput("S1select3a"),
                               uiOutput("S1select3b"),
                               uiOutput("S1select3c"),
                               uiOutput("S1select3d")
                             )
                           )            
                  ),
                  tabPanel("Data",    
                    uiOutput("S1downloadsourceUI"),
	            dataTableOutput('S1source_data')
                  ) ,
                  tabPanel("Plot",                                
                           uiOutput("S1downloadUI"),
                           uiOutput("S1plotUI")
                  )
                )
              )
            )
      ),      
      
      

      #AUDPC tab content
      tabItem(tabName = "S2Plot",
  div(style='height:100%; width:100%; overflow: scroll',    
              fluidRow(
                tabBox(
                  title = "AUDPC",
                  id = "tabsetS2",  width=12,
                  tabPanel("Selections", 
                           fluidRow( 
                             box(
                               title = "1. Choose an xlsx file",
                               width=7,
                               fileInput("fileS2", label = "",accept=".xlsx"),
                               uiOutput("S2PlotButtonUI"),
                               bsAlert("S2PlotAlertID"),
                               br(),
                               uiOutput("S2datasheetUI"),
                               uiOutput("S2datasheet1UI"),
			       div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("S2subsetvar1UI")),
			       div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("S2subsetval1UI")),
			       div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("S2subsetopr1UI")),
			       div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("S2subsetvar2UI")),
			       div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("S2subsetval2UI")),
			       div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("S2subsetopr2UI")),
			       div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("S2subsetvar3UI")),
			       div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("S2subsetval3UI")),
			       div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("S2subsetopr3UI"))
                             ),
                             box(
                               title ="Options",
                               width=5,
                               collapsible = FALSE,
                               collapsed=TRUE,
                               h4("Title Page Setting"),
                               uiOutput("TitlePageTitle1UI"),
                               uiOutput("TitlePageTitle2UI"),
                               uiOutput("label_TimeXaxisUI"),
                               uiOutput("label_ResponseUI"),
                               uiOutput("PDFHeader1UI"),
                               uiOutput("PDFHeader2UI"),
                               uiOutput("PDFHeader3UI") 
                             )
                           ),
                           br(),
                           fluidRow( 
                             box(
                               title = "2. Select columns for grouping within a trial",
                               width = 5,
                               bsAlert("S2ImportAlertID"),
                               uiOutput("S2select1")
                             ),
                             box(
                               title = "Column search",
                               width=6,
                               uiOutput("S2search1")
                             )
                           ),
                           br(),
                           fluidRow( 
                             box(
                             title ="3. Essential Variables",
                             width=8,
                             div(style="display: inline-block;vertical-align:top; width: 45%;", uiOutput("TreatmentUI")),
                             div(style="display: inline-block;vertical-align:top; width: 45%;", uiOutput("ReponseUI")),
                             div(style="display: inline-block;vertical-align:top; width: 45%;", uiOutput("TrialUI")),
                             div(style="display: inline-block;vertical-align:top; width: 45%;", uiOutput("RepUI")),
                             div(style="display: inline-block;vertical-align:top; width: 45%;", uiOutput("UTCUI")),
                             div(style="display: inline-block;vertical-align:top; width: 45%;", uiOutput("TimeXaxisUI")),
                             div(style="display: inline-block;vertical-align:top; width: 45%;", uiOutput("TrtTypeUI")),
                             #div(style="display: inline-block;vertical-align:top; width: 45%;", uiOutput("TableSortByTrtUI")),  
                             div(style="display: inline-block;vertical-align:top; width: 45%;", uiOutput("S2LegendFontSizeUI")),
                             div(style="display: inline-block;vertical-align:top; width: 90%;", uiOutput("TrtTypeColorsUI")),
                             div(style="display: inline-block;vertical-align:top; width: 19%;", uiOutput("TrtBlackUI")),
                             div(style="display: inline-block;vertical-align:top; width: 19%;", uiOutput("TrtGreenUI")),
                             div(style="display: inline-block;vertical-align:top; width: 19%;", uiOutput("TrtRedUI")),
                             div(style="display: inline-block;vertical-align:top; width: 19%;", uiOutput("TrtPurpleUI")),
                             div(style="display: inline-block;vertical-align:top; width: 19%;", uiOutput("TrtOrangeUI"))
                             )
                           )            
                  ),
                  tabPanel("Data",     
                    uiOutput("S2downloadsourceUI"),
		    dataTableOutput('S2source_data')
                  ) ,
                  tabPanel("Calculation",         
                           uiOutput("S2downloaddataUI"),
		           dataTableOutput('S2calculated_data')
                  ),
                  tabPanel("Plot",                                
                           uiOutput("S2downloadUI"),
                           uiOutput("S2plotUI")
                  )
                )
              )
            )
      ),      
      
      
      
      
      #Multi Trial Analysis tab content
      tabItem(tabName = "M1Plot",
        div(style='height:100%; width:100%; overflow: scroll',
          fluidRow(
            tabBox(
              title = "Multi Trial Analysis",
              id = "tabsetM1",  width=12,  
              tabPanel("Selections", 
                     fluidRow( 
                       box(
                         title = "1. Choose an xlsx file",
                         width=7,
                         fileInput("fileM1", label = "",accept=".xlsx"),
                         uiOutput("M1PlotButtonUI"),
                         bsAlert("M1PlotAlertID"),
                         br(),
                         uiOutput("M1datasheetUI"),
                         uiOutput("M1datasheet1UI"),
                         div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M1subsetvar1UI")),
                         div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M1subsetval1UI")),
                         div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M1subsetopr1UI")),
                         div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M1subsetvar2UI")),
                         div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M1subsetval2UI")),
                         div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M1subsetopr2UI")),
                         div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M1subsetvar3UI")),
                         div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M1subsetval3UI")),
                         div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M1subsetopr3UI"))
                         ),
                       box(
                         title ="Options",
                         width=5,
                         collapsible = FALSE,
                         collapsed=TRUE,
                         h4("Title Page Setting"),
                         uiOutput("M1Options1a"),
                         uiOutput("M1Options1b"),
                         uiOutput("M1Options1c"),
                         h4(" Label Setting"),
                         uiOutput("M1Options2a"),
                         uiOutput("M1Options2b")
                       )
                     ),
                     fluidRow( 
                       box(
                         title = "2. Select columns for grouping trials",
                         width = 5,
                         bsAlert("M1ImportAlertID"),
                         uiOutput("M1select1")
                       ),
                       box(
                         title = "Column search",
                         width=6,
                         uiOutput("M1search1")
                       ),
		       box(
		         title = "Rename Treatments",
		         width=11,
		         uiOutput("M1RenameYNUI"),
		         conditionalPanel( condition = "input.M1RenameYN=='Yes'",
		           uiOutput("M1ColorByUI"),   
		           div(style="display: inline-block;vertical-align:top; width: 80%;", rHandsontableOutput("hotM1")),
		           #div(style="display: inline-block;vertical-align:top; width: 15%;", uiOutput("M1ColorUI")),  	###V1.2
		           br(),
		           br(),
		           uiOutput("M1renameUI")
		         )
		       )
                     ),
                     br(),
                     fluidRow( 
                       box(
                         title = "3. Select columns",
                         width = 5,
                         uiOutput("M1select2a"),
                         div(style="display: inline-block;vertical-align:top; width: 49%;", uiOutput("M1select2b")),
                         div(style="display: inline-block;vertical-align:top; width: 49%;", uiOutput("M1select2k")),
                         uiOutput("M1select2c"),
                         uiOutput("M1select2d"),
                         uiOutput("M1select2e"),
                         uiOutput("M1select2f1"),
                         uiOutput("M1select2f")
                         ),
                       box(
                         title = "Response Type",
                         width = 6,
                         uiOutput("M1select2g"),
                         div(style="display: inline-block;vertical-align:top; width: 45%;", uiOutput("M1select2p")),
                         uiOutput("M1select2q"),
                         uiOutput("M1select2h"),
                         uiOutput("M1select2i"),
                         conditionalPanel(   
			   condition = "input.M1select2gval == 'Herbicide Visual Response (0-100)' || input.M1select2gval == 'Phyto'",
                 div(style="display: inline-block;vertical-align:top; width: 45%;", uiOutput("M1select2n")),
                           uiOutput("M1select2o")
                         )
                       ),
			 box(
			   title = "Plot options",
			   width = 6,
			   uiOutput("M1select2j"),
			   uiOutput("M1select3a"),
			   uiOutput("M1select3b"),
			   uiOutput("M1select3c")
			 )
                      ) 
                     ),
              tabPanel("Data",   
                uiOutput("M1downloadsourceUI"),
                dataTableOutput('M1source_data')
              ) ,
              tabPanel("Plot",    
                     uiOutput("M1downloadUI"),
                     uiOutput("M1plotUI")
                     )        
            )
          )  
        )
       ),
      
      #Relative to Control tab content
      tabItem(tabName = "M2Plot",
    div(style='height:100%; width:100%; overflow: scroll',
                  fluidRow(
                    tabBox(
                      title = "Relative to ... Plot",
                      id = "tabsetM2",  width=12,
                      tabPanel("Selections", 
                               fluidRow( 
                                 box(
                                   title = "1. Choose an xlsx file",
                                   width=7,
                                   fileInput("fileM2", label = "",accept=".xlsx"),
                                   uiOutput("M2PlotButtonUI"),
                                   bsAlert("M2PlotAlertID"),
                                   br(),
                                   uiOutput("M2datasheetUI"),
                                   uiOutput("M2datasheet1UI"),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M2subsetvar1UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M2subsetval1UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M2subsetopr1UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M2subsetvar2UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M2subsetval2UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M2subsetopr2UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M2subsetvar3UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M2subsetval3UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M2subsetopr3UI"))
                                 ),
                                 box(
                                   title ="Options",
                                   width=5,
                                   collapsible = FALSE,
                                   collapsed=TRUE,
                                   h4("Title Page Setting"),
                                   uiOutput("M2Options1a"),
                                   uiOutput("M2Options1b"),
                                   uiOutput("M2Options1c"),
                                   h4(" Label Setting"),
                                   uiOutput("M2Options2a"),
                                   uiOutput("M2Options2b"),
                                   uiOutput("M2Options2c")
                                 )
                               ),
                               fluidRow( 
                                 box(
                                   title = "2. Select columns for grouping trials",
                                   width = 5,
                                   bsAlert("M2ImportAlertID"),
                                   uiOutput("M2select1")
                                 ),
                                 box(
                                   title = "Column search",
                                   width=6,
                                   uiOutput("M2search1")
                                 )
                               ),
                               br(),
                               fluidRow( 
                                 box(
                                   title = "3. Select columns",
                                   width = 6,
                                   uiOutput("M2select2a"),
                                   uiOutput("M2select2b"),
                                   uiOutput("M2select2c"),
                                   uiOutput("M2select2d")
                                 ),
                                 box(
                                   #title = "Control",
                                   width = 5,
                                   uiOutput("M2select2e")
                                 )
                               ) 
                      ),
                      tabPanel("Data",   
                        uiOutput("M2downloadsourceUI"),
		        dataTableOutput('M2source_data')
                      ) ,
                      tabPanel("Plot", 
                               uiOutput("M2downloadUI"),
                               uiOutput("M2plotUI")
                      )
                    )
                  )  
              )
      ),
      
      #Two-Response tab content
      tabItem(tabName = "M3Plot",
    div(style='height:100%; width:100%; overflow: scroll',
                  fluidRow(
                    tabBox(
                      title = "Two-Response Plot",
                      id = "tabsetM3",  width=12,
                      tabPanel("Selections", 
                               fluidRow( 
                                 box(
                                   title = "1. Choose an xlsx file",
                                   width=7,
                                   fileInput("fileM3", label = "",accept=".xlsx"),
                                   uiOutput("M3PlotButtonUI"),
                                   bsAlert("M3PlotAlertID"),
                                   br(),
                                   uiOutput("M3datasheetUI"),
                                   uiOutput("M3datasheet1UI"),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M3subsetvar1UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M3subsetval1UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M3subsetopr1UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M3subsetvar2UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M3subsetval2UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M3subsetopr2UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M3subsetvar3UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M3subsetval3UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M3subsetopr3UI"))
                                 ),
                                 box(
                                   title ="Options",
                                   width=5,
                                   collapsible = FALSE,
                                   collapsed=TRUE,
                                   h4("Title Page Setting"),
                                   uiOutput("M3Options1a"),
                                   uiOutput("M3Options1b"),
                                   h4(" Label Setting"),
                                   uiOutput("M3Options2a"),
                                   uiOutput("M3Options2b"),
                                   uiOutput("M3Options2c"),
                                   uiOutput("M3Options2d")
                                 )
                               ),
                               fluidRow( 
                                 box(
                                   title = "2. Select columns for grouping trials",
                                   width = 5,
                                   bsAlert("M3ImportAlertID"),
                                   uiOutput("M3select1")
                                 ),
                                 box(
                                   title = "Column search",
                                   width=6,
                                   uiOutput("M3search1")
                                 )
                               ),
                               br(),
                               fluidRow( 
                                 box(
                                   title = "3. Select columns",
                                   width = 6,
                                   #uiOutput("M3select2a"),
                                   uiOutput("M3select2b"),
                                   uiOutput("M3select2c"),
                                   uiOutput("M3select2d")
                                 ),
                                 box(
                                   #title = "Control",
                                   width = 5,
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   uiOutput("M3select2e"),
                                   uiOutput("M3select2f")
                                 )
                               ) 
                      ),
                      tabPanel("Data",  
                        uiOutput("M3downloadsourceUI"),
		        dataTableOutput('M3source_data')
                      ) ,
                      tabPanel("Plot", 
                               uiOutput("M3downloadUI"),
                               uiOutput("M3plotUI")
                      )
                    )
                  )  
              )
      ),
   
      #Heat Map tab content
      tabItem(tabName = "M4Plot",
    div(style='height:100%; width:100%; overflow: scroll',
                  fluidRow(
                    tabBox(
                      title = "Heat Map",
                      id = "tabsetM4",  width=12,
                      tabPanel("Selections", 
                               fluidRow( 
                                 box(
                                   title = "1. Choose an xlsx file",
                                   width=7,
                                   fileInput("fileM4", label = "",accept=".xlsx"),
                                   uiOutput("M4PlotButtonUI"),
                                   bsAlert("M4PlotAlertID"),
                                   br(),
                                   uiOutput("M4datasheetUI"),
                                   uiOutput("M4datasheet1UI"),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M4subsetvar1UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M4subsetval1UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M4subsetopr1UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M4subsetvar2UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M4subsetval2UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M4subsetopr2UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M4subsetvar3UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M4subsetval3UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M4subsetopr3UI"))
                                 ),
                                 box(
                                   title ="Options",
                                   width=5,
                                   collapsible = FALSE,
                                   collapsed=TRUE,
                                   h4("Title Page Setting"),
                                   uiOutput("M4Options1a"),
                                   uiOutput("M4Options1b"),
                                   h4(" Label Setting"),
                                   uiOutput("M4Options2a"),
                                   uiOutput("M4Options2b"),
                                   uiOutput("M4Options2c")
                                 )
                               ),
                               fluidRow( 
                                 box(
                                   title = "2. Select columns for grouping trials",
                                   width = 5,
                                   bsAlert("M4ImportAlertID"),
                                   uiOutput("M4select1")
                                 ),
                                 box(
                                   title = "Column search",
                                   width=6,
                                   uiOutput("M4search1")
                                 )
                               ),
                               br(),
                               fluidRow( 
                                 box(
                                   title = "3. Select columns",
                                   width = 5,
                                   uiOutput("M4select2a"),
                                   uiOutput("M4select2b"),
                                   uiOutput("M4select2c"),
                                   uiOutput("M4select2d")
                                 ),
                                 box(
                                   #title = "Control",
                                   width = 6,
                                   uiOutput("M4select2e")
                                 ),
                                 box(
                                   title = "Plot options",
                                   width = 6,
                                   uiOutput("M4select3a")
                                 )
                               ) 
                      ),
                      tabPanel("Data",    
                        uiOutput("M4downloadsourceUI"),
		        dataTableOutput('M4source_data')
                      ) ,
                      tabPanel("Plot", 
                               uiOutput("M4downloadUI"),
                               uiOutput("M4plotUI")          
                      )
                    )
                  )  
              )
      ),
      
      #Piano Chart tab content
      tabItem(tabName = "M5Plot",
    div(style='height:100%; width:100%; overflow: scroll',
                  fluidRow(
                    tabBox(
                      title = "Piano Plot",
                      id = "tabsetM5",  width=12,
                      tabPanel("Selections", 
                               fluidRow( 
                                 box(
                                   title = "1. Choose an xlsx file",
                                   width=7,
                                   fileInput("fileM5", label = "",accept=".xlsx"),
                                   uiOutput("M5PlotButtonUI"),
                                   bsAlert("M5PlotAlertID"),
                                   br(),
                                   uiOutput("M5datasheetUI"),
                                   uiOutput("M5datasheet1UI"),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M5subsetvar1UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M5subsetval1UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M5subsetopr1UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M5subsetvar2UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M5subsetval2UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M5subsetopr2UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M5subsetvar3UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M5subsetval3UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M5subsetopr3UI"))
                                 ),
                                 box(
                                   title ="Options",
                                   width=5,
                                   collapsible = FALSE,
                                   collapsed=TRUE,
                                   h4("Title Page Setting"),
                                   uiOutput("M5Options1a"),
                                   uiOutput("M5Options1b"),
                                   uiOutput("M5Options1c"),
                                   h4(" Label Setting"),
                                   uiOutput("M5Options2a"),
                                   uiOutput("M5Options2b"),
                                   uiOutput("M5Options2c")
                                 )
                               ),
                               fluidRow( 
                                 box(
                                   title = "2. Select columns for grouping trials",
                                   width = 5,
                                   bsAlert("M5ImportAlertID"),
                                   uiOutput("M5select1")
                                 ),
                                 box(
                                   title = "Column search",
                                   width=6,
                                   uiOutput("M5search1")
                                 )
                               ),
                               br(),
                               fluidRow( 
                                 box(
                                   title = "3. Select columns",
                                   width = 6,
                                   uiOutput("M5select2a"),
                                   uiOutput("M5select2b"),
                                   uiOutput("M5select2c"),
                                   uiOutput("M5select2d")
                                 ),
                                 box(
                                   #title = "Control",
                                   width = 5,
                                   uiOutput("M5select2e"),
                                   uiOutput("M5select2f")
                                 )
                               ) 
                      ),
                      tabPanel("Data",  
                        uiOutput("M5downloadsourceUI"),
		        dataTableOutput('M5source_data')
                      ) ,
                      tabPanel("Plot", 
                               uiOutput("M5downloadUI"),
                               uiOutput("M5plotUI")
                      )
                    )
                  )  
              )
      ),
      
      #Box Plot tab content
      tabItem(tabName = "M6Plot",
    div(style='height:100%; width:100%; overflow: scroll',
                  fluidRow(
                    tabBox(
                      title = "Boxplot",
                      id = "tabsetM6",  width=12,
                      tabPanel("Selections", 
                               fluidRow( 
                                 box(
                                   title = "1. Choose an xlsx file",
                                   width=7,
                                   fileInput("fileM6", label = "",accept=".xlsx"),
                                   uiOutput("M6PlotButtonUI"),
                                   bsAlert("M6PlotAlertID"),
                                   br(),
                                   uiOutput("M6datasheetUI"),
                                   uiOutput("M6datasheet1UI"),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M6subsetvar1UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M6subsetval1UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M6subsetopr1UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M6subsetvar2UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M6subsetval2UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M6subsetopr2UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M6subsetvar3UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M6subsetval3UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M6subsetopr3UI"))
                                 ),
                                 box(
                                   title ="Options",
                                   width=5,
                                   collapsible = FALSE,
                                   collapsed=TRUE,
                                   h4("Title Page Setting"),
                                   uiOutput("M6Options1a"),
                                   uiOutput("M6Options1b"),                                 
                                   h4(" Label Setting"),
                                   uiOutput("M6Options2a"),
                                   uiOutput("M6Options2b")
                                 )
                               ),
                               fluidRow( 
                                 box(
                                   title = "2. Select columns for grouping trials",
                                   width = 5,
                                   bsAlert("M6ImportAlertID"),
                                   uiOutput("M6select1")
                                 ),
                                 box(
                                   title = "Column search",
                                   width=6,
                                   uiOutput("M6search1")
                                 ),
			         box(
			           title = "Rename Treatments",
			           width=11,
			           uiOutput("M6RenameYNUI"),
			           conditionalPanel( condition = "input.M6RenameYN=='Yes'",
			             uiOutput("M6ColorByUI"),   
			             div(style="display: inline-block;vertical-align:top; width: 80%;", rHandsontableOutput("hotM6")),
			             #div(style="display: inline-block;vertical-align:top; width: 15%;", uiOutput("M6ColorUI")),  	###V1.2
			             br(),
			             br(),
			             uiOutput("M6renameUI")
				   )
			         )
			       ),
			       br(),
                               fluidRow( 
                                 box(
                                   title = "3. Select columns",
                                   width = 5,
                                   uiOutput("M6select2a"),
                                   uiOutput("M6select2b"),
                                   uiOutput("M6select2d")
                                 ),
                                 box(
                                   title = "Plot options",
                                   width = 6,
                                   #uiOutput("M6select3a"),
                                   uiOutput("M6select3b"),
                                   uiOutput("M6select3c"),
                                   uiOutput("M6select3d")
                                 )
                               ) 
                      ),
                      tabPanel("Data",   
                        uiOutput("M6downloadsourceUI"),
		        dataTableOutput('M6source_data')
                      ) ,
                      tabPanel("Plot", 
                               uiOutput("M6downloadUI"),
                               uiOutput("M6plotUI")
                      )
                    )
                  )  
              )
      ),

      


      #Spider Plot tab content
      tabItem(tabName = "M7Plot",
    div(style='height:100%; width:100%; overflow: scroll',
                  fluidRow(
                    tabBox(
                      title = "Spider Plot",
                      id = "tabsetM7",  width=12,
                      tabPanel("Selections", 
                               fluidRow( 
                                 box(
                                   title = "1. Choose an xlsx file",
                                   width=7,
                                   fileInput("fileM7", label = "",accept=".xlsx"),
                                   uiOutput("M7PlotButtonUI"),
                                   bsAlert("M7PlotAlertID"),
                                   br(),
                                   uiOutput("M7datasheetUI"),
                                   uiOutput("M7datasheet1UI"),
			    	   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M7subsetvar1UI")),
				   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M7subsetval1UI")),
				   div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M7subsetopr1UI")),
				   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M7subsetvar2UI")),
				   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M7subsetval2UI")),
				   div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M7subsetopr2UI")),
				   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M7subsetvar3UI")),
				   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M7subsetval3UI")),
				   div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M7subsetopr3UI"))
                                 ),
                                 box(
                                   title ="Options",
                                   width=5,
                                   collapsible = FALSE,
                                   collapsed=TRUE,
                                   h4("Title Page Setting"),
                                   uiOutput("M7Options1a"),
                                   uiOutput("M7Options1b")                                   
                                 )
                               ),
                               fluidRow( 
                                 box(
                                   title = "2. Select columns for grouping trials",
                                   width = 6,
                                   uiOutput("M7select1a"),
                                   bsAlert("M7ImportAlertID"),
                                   uiOutput("M7select1")
                                 ),
                                 box(
                                   title = "Column search",
                                   width=5,
                                   uiOutput("M7search1")
                                 )
                               ),
                               br(),
                               fluidRow( 
                                 box(
                                   title = "3. Select chart options",
                                   width = 6,
				   div(style="display: inline-block;vertical-align:top; width: 45%;", uiOutput("M7select2f")),
				   div(style="display: inline-block;vertical-align:top; width: 45%;", uiOutput("M7select2a")),
				   div(style="display: inline-block;vertical-align:top; width: 45%;", uiOutput("M7select2b")),
				   div(style="display: inline-block;vertical-align:top; width: 45%;", uiOutput("M7select2c")),
				   div(style="display: inline-block;vertical-align:top; width: 45%;", uiOutput("M7select2g")),
				   div(style="display: inline-block;vertical-align:top; width: 45%;", uiOutput("M7select2e"))
                                 ),
                                 box(
                                   title = "Treatments to compare",
                                   width = 5,
                                   uiOutput("M7select2d")
                                 )
                               ) 
                      ),
                      tabPanel("Data",  
                        uiOutput("M7downloadsourceUI"),
		        dataTableOutput('M7source_data')
                      ) ,
                      tabPanel("Plot", 
                               uiOutput("M7downloadUI"),
                               uiOutput("M7plotUI")
                      )
                    )
                  )  
              )
      ),
      
      
      #Bar chart tab content
      tabItem(tabName = "M8Plot",
    div(style='height:100%; width:100%; overflow: scroll',
                  fluidRow(
                    tabBox(
                      title = "Bar Chart",
                      id = "tabsetM8",  width=12,
                      tabPanel("Selections", 
                               fluidRow( 
                                 box(
                                   title = "1. Choose an xlsx file",
                                   width=7,
                                   fileInput("fileM8", label = "",accept=".xlsx"),
                                   uiOutput("M8PlotButtonUI"),
                                   bsAlert("M8PlotAlertID"),
                                   br(),
                                   uiOutput("M8datasheetUI"),
                                   uiOutput("M8datasheet1UI"),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M8subsetvar1UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M8subsetval1UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M8subsetopr1UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M8subsetvar2UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M8subsetval2UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M8subsetopr2UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M8subsetvar3UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 40%;", uiOutput("M8subsetval3UI")),
                                   div(style="display: inline-block;vertical-align:top; width: 18%;", uiOutput("M8subsetopr3UI"))
                                 ),
                                 box(
                                   title ="Options",
                                   width=5,
                                   collapsible = FALSE,
                                   collapsed=TRUE,
                                   h4("Title Page Setting"),
                                   uiOutput("M8Options1a"),
                                   uiOutput("M8Options1b"),                                 
                                   h4(" Label Setting"),
                                   uiOutput("M8Options2a"),
                                   uiOutput("M8Options2b")
                                 )
                               ),
                               fluidRow( 
                                 box(
                                   title = "2. Select columns for grouping trials",
                                   width = 5,
                                   bsAlert("M8ImportAlertID"),
                                   uiOutput("M8select1a")
                                 ),
                                 box(
                                   title = "Column search",
                                   width=6,
                                   uiOutput("M8search1")
                                 ),
                                 box(
                                   title = "Rename Treatments",
                                   width=11,
                                   uiOutput("M8RenameYNUI"),
                                   conditionalPanel( condition = "input.M8RenameYN=='Yes'",
                                     uiOutput("M8ColorByUI"),   
				     div(style="display: inline-block;vertical-align:top; width: 80%;", rHandsontableOutput("hotM8")),
				     #div(style="display: inline-block;vertical-align:top; width: 15%;", uiOutput("M8ColorUI")),  	###V1.2
                                     br(),
                                     br(),
                                     uiOutput("M8renameUI")
                                   )
                                 )
                               ),
                               br(),
                               fluidRow( 
                                 box(
                                   title = "3. Select columns",
                                   width = 5,
                                   uiOutput("M8select2a"),
                                   uiOutput("M8select2b"),
                                   uiOutput("M8select2d")
                                 ),
                                 box(
                                   title = "Plot options",
                                   width = 6,
                                   uiOutput("M8select3a"),
                                   uiOutput("M8select3f"),
                                   uiOutput("M8select3b"),
                                   uiOutput("M8select3c"),
                                   uiOutput("M8select3d"),
                                   uiOutput("M8select3e")
                                 )
                               ) 
                      ),
                      tabPanel("Data",  
                        uiOutput("M8downloadsourceUI"),
		        dataTableOutput('M8source_data')
                      ) ,
                      tabPanel("Plot", 
                               uiOutput("M8downloadUI"),
                               uiOutput("M8plotUI")
                      )
                    )
                  )  
              )
      ),

      
      #MoreInfo menu content
      tabItem(tabName = "MoreInfo",
        fluidRow( 
          box(width = 11.5,
  div(style='height:100%; width:100%; overflow: scroll',
              h3("Plot Interpretation"), 
              br(),
              h4("1. Multi Trial Analysis"),
              h4("For Normal (normally distributed) response type: Cross trial analysis, with trials treated as random."),
              h4("Choice of Plots for normal data:"),
              h4("\b\t\b\t\b\tBox Plot - display of spread of data for each treatment. Minimum, maximum, median, mean and 25th and 75th percentiles"),
              h4("\b\t\b\t\b\tare displayed for each treatment. A table at the right shows # of trials, estimated mean from analysis and Tukey "),
              h4("\b\t\b\t\b\tmean separation letters."),
              actionButton("ExampleButton1", "Example"),
              uiOutput("ExampleUI1"),
              h4("\b\t\b\t\b\tor"),
              h4("\b\t\b\t\b\tBar Plot - bar for each treatment based on estimated mean from analysis.  Error bar is confidence interval."),
              h4("\b\t\b\t\b\tA table at the right shows # of trials, estimated mean from analysis and Tukey mean separation letters."),
              actionButton("ExampleButton2", "Example"),
              uiOutput("ExampleUI2"),
              br(),
              h4("	Rating Scale Plot - For Rating Scale (non-normally distributed) response type: Ordinal logistic regression, with trials treated as random, providing Tukey mean separation based on Odds Ratio Estimates and Odds Ratio Estimates."),
              actionButton("ExampleButton3", "Example"),
              uiOutput("ExampleUI3"),
              #br(),
              #h4("2. Relative to ... Plot :"),
              #h4("Relative to ... plot displays the mean responses of each treatments and the percent change of mean response relative to a user defined control group."),
              #actionButton("ExampleButton4", "Example"),
              #uiOutput("ExampleUI4"),
              br(),
              h4("2. Two-Response Plot :"),
              h4("Two-Response plot displays the change of two response types or two treatments within a single plot with the same evaluation timing ranges. Plot explores the relationship between these."),
              actionButton("ExampleButton5", "Example"),
              uiOutput("ExampleUI5"),
              br(),
              h4("3. Heat Map :"),
              h4("Heat map displays colored scale of average responses for a treatment over evaluation timings.When selecting to display untreated control, heat map will display pest pressure/disease pressure for all trials over evaluation timings."),
              actionButton("ExampleButton6", "Example"),
              uiOutput("ExampleUI6"),
              br(),
              h4("4. Piano Chart :"),
              h4("Piano Chart performs a paired comparison between treatments and outputs the difference bar charts based on the trials."),
              actionButton("ExampleButton7", "Example"),
              uiOutput("ExampleUI7"),
              br(),
              h4("5. Boxplot :"),
              h4("Display of spread of data for each treatment. Minimum, maximum, median, mean and 25th and 75th percentiles are displayed for each treatment."),
              actionButton("ExampleButton8", "Example"),
              uiOutput("ExampleUI8"),
              br(),
              h4("6. Spider Plot :"),
              h4("For herbicide data: creates a plot for a treatment, where each 'spoke', displays average response for a species across trials. Number of trials for that species is in ( ) at the end of that spoke.  Line connects the averages. Also option to compare two treatments.  In this case, only trials with data on both species are used in the plot. Different color line for each treatment."),
              actionButton("ExampleButton9", "Example"),
              uiOutput("ExampleUI9"),
              br()
            )
          )    
        )
      )
    )
  )
)