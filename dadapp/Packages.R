CheckInstallPackages <- function(pkgs) {                     #pkgs is a vector of strings with length >= 1
  #check and install the package specified by pkgs 
  x <- lapply(pkgs, function(pkg){                           #For each pkg in pkgs (attempt to load each package one at a time):
    if(!do.call("require", list(pkg))) {                     #  Load the package if available,
      try(install.packages(pkg, lib=.Library,
                           repos="http://cran.rstudio.com")) #    Silently attempt to install into the default library
      tryCatch(do.call("library", list(pkg)),                #    Now attempt to load the package, catch error if it wasn't installed
               error = function(err) {                              #    Catch if we're unable to install into the default library
                 if(!interactive()) {                               #      If non-interactive, install into this user's personal library
                   personalLibPath <- Sys.getenv("R_LIBS_USER")     #        Get the path to this user's personal library
                   if(is.na(match(personalLibPath, .libPaths()))) { #        If the personal library is not in the list of libraries
                     dir.create(personalLibPath, recursive = TRUE)  #          Then create the personal library
                     .libPaths(personalLibPath)                     #          And add the personal library to the list of libraries
                   }
                   install.packages(pkg, lib=personalLibPath,       #        Attempt to install the package into the personal library
                                    repos="http://cran.rstudio.com") #          if this fails, raise the error back to the report
                   do.call("library", list(pkg))                    #        Finally, attempt to load the package
                 }
               }
      )
    }
  })
}

#install package#
CheckInstallPackages(c("shiny","shinyjs","shinyBS","shinydashboard"))
CheckInstallPackages(c("openxlsx","data.table","zoo","graphics","grid","sqldf","RColorBrewer"))
CheckInstallPackages(c("gdata","data.table","grDevices","graphics","gtable","zoo","stringr"))
CheckInstallPackages(c("lsmeans","parallel","lme4","lmerTest","pbkrtest","multcomp","R.utils"))
CheckInstallPackages(c("shiny","shinyjs","shinyBS","shinydashboard","htmltools","gsubfn","blob"))
CheckInstallPackages(c("proto","RSQLite","ggplot2","grid","gridExtra","plyr","dplyr","bit64","bit"))  
CheckInstallPackages(c("plyr","plotrix","scales","cowplot","reshape","reshape2","LPCM","jsonlite"))
CheckInstallPackages(c("sp","ordinal","multcompView","rgl","tibble","lattice","fmsb","htmlwidgets"))
CheckInstallPackages(c("agricolae","devtools","xlsx","Cairo","afex","rhandsontable","colourpicker"))

##The following commands locally, only needed to save package zip files to a folder and then manually copied into server
#setwd("Q:/packages/")
#pckges <- rownames(installed.packages())
#pkgInfo <- download.packages(pkgs = pckges, destdir = getwd(), type = "win.binary")
  
  
##The following commands on non-internet server, only needed to save package zip files to a folder and then manually copied into server    
##Copy zip files from local directory to non-internet server directory
#setwd("D:/Trials App/packages")
#install.packages(list.files(), repos = NULL, type = "win.binary")          
          