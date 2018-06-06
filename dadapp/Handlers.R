do_import=function(session,trial,ExcelURL, SheetNameList,DataSheetName,AlertName){
  data <- NULL
  adafa <- NULL
  errmsg <- NULL
  assign("ExcelURL",ExcelURL, envir = .GlobalEnv)
  assign("SheetNameList",SheetNameList, envir = .GlobalEnv)
  assign("DataSheetName",DataSheetName, envir = .GlobalEnv)
  assign("SheetNameListdata",SheetNameList[["data"]], envir = .GlobalEnv)
  try({ 
    data=openxlsx::read.xlsx(ExcelURL,sheet=SheetNameList[["data"]]) 
    colnames(data)=chartr('.','_',colnames(data))
    colnames(data)=chartr('-','_',colnames(data)) 
    adafa=openxlsx::read.xlsx(ExcelURL,sheet=SheetNameList[["adafa"]]) 
    colnames(adafa)=chartr('.','_',colnames(adafa))
    colnames(adafa)=chartr('-','_',colnames(adafa))
    #CharVars <- colnames(data)[sapply(data,class) %in% c("character","factor")]  
    #for (i in 1:length(CharVars)) {
    #  data[is.na(data[,CharVars[i]]),CharVars[i]] <- "N/A"
    #}
  })
  try({
    if(any(nchar(data$Prog_Alias)>96)){
      Nchar_message <- paste0("Warnings:","\n","\n","Prog Alias greater than 96 characters. Return to Excel and shorten Prog Alias names.")
      errmsg <- Nchar_message
      try(shinyjs::alert(Nchar_message))
      try(closeAlert(session, AlertName))
      data <- NULL
      adafa <- NULL
    }else{
        if ( ! ("REP" %in%  toupper(colnames(data)))  ){
        data$REP=1
       }
     assign('data',data,envir = .GlobalEnv)
    }
  })
  return(list(data=data,adafa=adafa,errmsg=errmsg))
}

datafilter=function (data,loopvar,n=1){
  filter=sqldf(paste("select distinct", paste(loopvar,collapse=','),'from data' ))
  filterrow=filter[n,]
  datafilter=sqldf(paste("select *",'from data natural inner join filterrow' ))
}

containstr=function(main,sub){
  containstr=character(0)
  for (i in 1:length(sub)){
    index=grep(sub[i],main,ignore.case = TRUE)
    if (length(index)!=0) {
      containstr=c(containstr,main[index])
      main=main[-index]
    }
  }
  return(containstr)
}

prioritylist=function(prilist,list){
  prioritylist=c()
  for (i in 1:length(prilist)){
    try({
      index=grep(toupper(prilist[i]),toupper(list),ignore.case = TRUE)
      if (length(index)!=0) {
        prioritylist=c(prioritylist,list[index])
        list=list[-index]
      }
    })  
  }
  prioritylist=c(prioritylist,list)
  return(prioritylist)
}

prisort=function(prilist,list){
  index=match(toupper(prilist),toupper(list),nomatch=0)
  prisort=c()
  if (index!=0) {
    prisort=c(list[index])
    list=list[-index]
  }
  rest=prioritylist(prilist,list)
  prisort=c(prisort,rest)
  return(prisort)
}


matchvalue=function(value="Prog_Alias",list){
  list<-gsub("_","",list)
  list<-gsub("^\\s+|\\s+$", "", list)
  list<-toupper(list)
  value<-gsub("_","","Prog_alias") 
  value<-gsub("^\\s+|\\s+$", "", value)
  value<-toupper(value)
  index=grep(value,list,ignore.case = TRUE)
  list[index]
return(list[index])
}

get_wraper <- function(width) {
    function(x) {
        lapply(strwrap(x, width = width, simplify = FALSE), paste, collapse="\n")
    }
}

break_str=function(x,at){
  if (nchar(x)<=at) newstr=x
  else {
    line1 <- substr(x,1,at)
    strleft <-substr(x,at+1,nchar(x)) 
    x<-strleft
    if (nchar(x)<=at) newstr <- paste(line1,strleft,sep="\n")
    else {
      line2 <- substr(x,1,at)
      strleft <-substr(x,at+1,nchar(x))
      newstr <- paste(line1,line2,strleft,sep="\n")
    }
  }
  return(newstr)
}

break_str1=function(x,at,pipe=NULL){
   x <- sub('\n', '', trimws(x))
  if (!is.null(pipe)) {
    pos = gregexpr(pipe, substr(x,1,at))
    p <- pos[[1]][length(pos[[1]])] 
    if (p > 0 & (length(x)-p) >=at) at <-p
  }
  if (nchar(x)<=at) newstr=x
  else {
    line1 <- substr(x,1,at)
    strleft <-substr(x,at+1,nchar(x)) 
    x<-strleft
    if (nchar(x)<=at) newstr <- paste(line1,strleft,sep="\n")
    else {
      line2 <- substr(x,1,at)
      strleft <-substr(x,at+1,nchar(x))
      newstr <- paste(line1,line2,strleft,sep="\n")
    }
  }
  return(newstr)
}

get_company=function(Confidential=FALSE){
  if (Confidential) company <- "FMC Confidential"
  else company <- "FMC"
  return(company)
}

add_footer=function(x=.1,y1=.04,y2=.01,f=9, footer1="", footer2="", Confidential=FALSE){
  footer2 <- paste0(get_company(Confidential=Confidential), ", Reveal V1.1,",footer2,format(as.Date(Sys.Date(), "%b%d, %Y"), "%d-%B-%Y"))
  grid.text(footer1,gp=gpar(fontsize=f),y=y1,x=x,just='left') 
  grid.text(footer2,gp=gpar(fontsize=f),y=y2,x=x,just='left') 
}

add_footer_g=function(p=NULL,footer1="",footer2="",f=9, Confidential=FALSE){
  footer2 <- paste0(get_company(Confidential=Confidential), ", Reveal V1.1, ",footer2,", ", significance,", ",format(as.Date(Sys.Date(), "%b%d, %Y"), "%d-%B-%Y"))
  footers <- paste0(footer1,'\n',footer2)
  p <- grid.arrange(p, bottom = textGrob(footers, x = .05, 
                       hjust = 0, gp = gpar(fontsize = f)))
  print(p)
}
  
fmc_colors=function(){
  fmc_colorpalette=c(rgb(239,64,52,maxColorValue = 255),
    #rgb(0,0,0,maxColorValue = 255), #Exclude black
    rgb(37,64,143,maxColorValue = 255),rgb(56,135,166,maxColorValue = 255),rgb(75,128,181,maxColorValue = 255),
    rgb(244,125,32,maxColorValue = 255), rgb(246,193,35,maxColorValue = 255),rgb(147,149,152,maxColorValue = 255),
    rgb(150,27,30,maxColorValue = 255),rgb(117,129,191,maxColorValue = 255), rgb(178,172,52,maxColorValue = 255),
    rgb(132,187,59,maxColorValue = 255),rgb(62,123,98,maxColorValue = 255),
    rgb(9,174,237,maxColorValue = 255),rgb(255,192,0,maxColorValue = 255),rgb(89,89,89,maxColorValue = 255)
    )
  fmc_colorpalette <- c(fmc_colorpalette,fmc_colorpalette,fmc_colorpalette,fmc_colorpalette,fmc_colorpalette, fmc_colorpalette)
  return(fmc_colorpalette)    
}   

fmc_boxplot_colors=function(){
#for boxplot and barcharts in standalone and multitrial modules  
  colorpalette=c(
	rgb(159,154,49,maxColorValue = 255),     	#olive green
	rgb(233,140,51,maxColorValue = 255),     	#light orange
	rgb(137,137,137,maxColorValue = 255),     	#grey, default for untrt when color by Trt Type
	rgb(239,64,52,maxColorValue = 255),     	#red/orange 100% red/orange, default for std when color by Trt Type
	rgb(7,131,181,maxColorValue = 255),   		#blueish, 100%  blueish
	rgb(246,193,35,maxColorValue = 255),   		#yellow, 
	rgb(62,123,98,maxColorValue = 255),   		#blue green, 100% blue green
	rgb(243,112,103,maxColorValue = 255),   	#red/orange, 75% red/orange
	rgb(247,160,154,maxColorValue = 255),   	#red/orange, 50% red/orange
	rgb(251,207,204,maxColorValue = 255),   	#red/orange, 25% red/orange
	rgb(183,179,101,maxColorValue = 255),   	#olive green, 75% olive green
	rgb(207,205,152,maxColorValue = 255),   	#olive green, 50% olive green
	rgb(231,230,204,maxColorValue = 255),   	#olive green, 25% olive green
	rgb(69,162,200,maxColorValue = 255),   		#blueish, 75% blueish
	rgb(131,193,218,maxColorValue = 255),   	#blueish, 50% blueish
	rgb(193,224,237,maxColorValue = 255),   	#blueish, 25% blueish
	rgb(110,156,137,maxColorValue = 255),   	#blue green, 75% blue green
	rgb(159,189,177,maxColorValue = 255),   	#blue green, 50% blue green
	rgb(207,222,216,maxColorValue = 255)   		#blue green, 25% blue green
    )
  return(c(colorpalette,colorpalette,colorpalette))    
}  

trt_colors=function(tmpdata,untrt=c("untreated CK","untreatedCK"),std=c("Stdrd")){
  #tmpdata  - input data set
  #tmpdata$Trt - as.factor(datafilter$x)
  #tmpdata$color - as.factor(datafilter$colvar)
  #untrt std - optional values for untrt and std. If noyt possible to be selected by users, then need to hardcode all possible values
  TrtLevel <- levels(factor(tmpdata$Trt))
  colorpalette=fmc_boxplot_colors() 
  if (any(TrtLevel %in% untrt)) colorpalette=colorpalette[colorpalette != rgb(137,137,137,maxColorValue = 255)]
  if (any(TrtLevel %in% std)) colorpalette=colorpalette[colorpalette != rgb(239,64,52,maxColorValue = 255)]
  colors <- c()
  df <- sqldf("select distinct color, Trt from tmpdata")
  for (j in 1:length(TrtLevel)) {
    colors <- c(colors,as.character(df[df$Trt==TrtLevel[j],"color"]))
    names(colors)[j] <- TrtLevel[j] 
  }
  colorbyLevel <- levels(as.factor(df$color))
  for (k in 1:length(colorbyLevel)) {
    colors <- replace(colors,colors==colorbyLevel[k],colorpalette[k]) 
  }
  #grey, default for untrt when color by Trt Type
  colors[names(colors) %in% untrt ] <- rgb(137,137,137,maxColorValue = 255)    	
  #red/orange 100% red/orange, default for std when color by Trt Type
  colors[names(colors) %in% std ] <- rgb(239,64,52,maxColorValue = 255)     	
  return(colors)    
}   
   
      
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)  
} 
 
font_size=function(trt){
  trt_len <- max(str_length(trt)) 
  if (trt_len >= 29) max_size <- 9
  if (trt_len >= 26 & trt_len <= 28) max_size <- 10
  if (trt_len >= 23 & trt_len <= 25) max_size <- 11
  if (trt_len >= 21 & trt_len <= 22) max_size <- 12
  if (trt_len >= 19 & trt_len <= 20) max_size <- 13
  if (trt_len <= 18) max_size <- 14
  return(max_size)
}

options(error = recover)