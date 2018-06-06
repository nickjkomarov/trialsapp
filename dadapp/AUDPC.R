##############PDF Title page#############
PDFCreate <- function(data, 
	loopvar, 
	Trial,
	Treatment,
	Rep, 
	UTC,
	TimeXaxis,
	Response,
    TrtType,
    TrtBlack,
    TrtGreen,
    TrtRed,
    TrtPurple,
    TrtOrange,
	OutputURL,
	TitlePageTitle1,
	TitlePageTitle2,
	xLabel,
	yLabel,
	PDFHeader1,
	PDFHeader2,
	PDFHeader3,
	LegendFontSize) {
  assign("data",data,envir = .GlobalEnv)	
  assign("Trial",Trial,envir = .GlobalEnv)
  assign("Treatment",Treatment,envir = .GlobalEnv)
  assign("Trial",Trial,envir = .GlobalEnv)
  assign("Rep",Rep,envir = .GlobalEnv)
  assign("UTC",UTC,envir = .GlobalEnv)
  assign("TimeXaxis",TimeXaxis,envir = .GlobalEnv)
  assign("Response",Response,envir = .GlobalEnv)
  assign("TrtType",TrtType,envir = .GlobalEnv)
  assign("loopvar",loopvar,envir = .GlobalEnv)
  
  data[,"Treatment"] <- data[,Treatment]

  calculated_df <- data.frame()

  MainTitle <- "Data Summary"
  pdfname=paste0("audpcByTrial", format(Sys.time(),format='%Y%m%d%H%M%S'),'.pdf')
  fullpdffilename<-paste(OutputURL,pdfname,sep='/')
  pdf(fullpdffilename, onefile = TRUE, width=10, height=8.5,  paper="USr")
  plot.new()
  grid.text(MainTitle,gp=gpar(cex=3,font=1),y=0.7,x=0.5,just='center')
  grid.text("AUDPC",gp=gpar(cex=1.6,font=1),y=0.45,x=0.5,just='center')
  grid.text(TitlePageTitle1,gp=gpar(cex=1.6,font=1),y=0.4,x=0.5,just='center')
  grid.text(TitlePageTitle2,gp=gpar(cex=1.6,font=1),y=0.35,x=0.5,just='center')
  add_footer() 
  sqlgroupvar <- paste0(paste(loopvar,collapse=','), ', ',Trial)
  contentsummary=sqldf(paste("select distinct ", sqlgroupvar, " from data"))
  perpage=20
  totaltrialnum=dim(contentsummary)[1]
  tableno=ceiling(totaltrialnum/perpage)
  for (i in 1:tableno) {
    plot.new()
    grid.table(contentsummary[((i-1)*perpage+1):min(i*perpage,dim(contentsummary)[1]),], rows=NULL)
    tabletitle='Table of Contents'
    if (tableno!=1){
      tabletitle=paste0(tabletitle,'(',i,')')
    }
    grid.text(PDFHeader1,gp=gpar(cex=0.9,font=1),y=0.95,x=0.1,just='left')
    grid.text(PDFHeader2,gp=gpar(cex=0.9,font=1),y=0.93,x=0.1,just='left')
    grid.text(PDFHeader3,gp=gpar(cex=0.9,font=1),y=0.91,x=0.1,just='left')
    grid.text(tabletitle,gp=gpar(cex=1.2,font=2),y=0.9,x=0.5,just='center')
    add_footer() 
  }
    
  tocnum=dim(contentsummary)[1]
  for (i in 1:tocnum){
    currentkeys<-as.data.frame(contentsummary[i,])
    currentdata=sqldf(paste("select *",'from data natural inner join currentkeys' ))
    currentdata_utc= currentdata[currentdata[,Treatment]==UTC,]
    sqlgroupvar <- paste(sqlgroupvar, Trial, TrtType, Treatment, sep=',') 
    sqlgroupvar <- paste(sqlgroupvar,"TimeXaxis",sep=',')
    sqlordervar <- paste(sqlgroupvar,Rep,sep=',')
    currentdata$TimeXaxis=currentdata[,TimeXaxis] #Use TimeXaxis as var name in sql in case Excel column names contains invalid characters
    currentsummary=sqldf(paste('select ', sqlgroupvar, ',', Rep, ', avg(', Response,') as "Response" from currentdata group by ',sqlgroupvar, ' order by ', sqlordervar ))
    colnames(currentsummary)[which(colnames(currentsummary)=="TimeXaxis")] <- TimeXaxis #Use TimeXaxis as var name in sql in case Excel column names contains invalid characters
    currentfinal <- unique(currentsummary[,Trial])
    title1 <- paste(currentfinal,": Plot of ",yLabel, " vs ", xLabel)
    singlelineplot(data=currentsummary,
        loopvar=loopvar, 
        Trial,
        Treatment,
        TimeXaxis,
        Response,
        TrtType,
        TrtBlack,
        TrtGreen,
        TrtRed,
        TrtPurple,
        TrtOrange,
        xLabel,
        yLabel,
        title1,
        title2,
        title3,
        PDFHeader1,
        PDFHeader2,
        PDFHeader3,
        LegendFontSize)
    title1 <- paste(currentfinal,": Calculated AUDPC, RAUDPC, stAUDPC")
    title2 <- paste("Matched by :",paste(c(loopvar,Trial),collapse='|'))
    title3 <- paste(unique(currentsummary[,c(loopvar,Trial)]),collapse = '|')
    out <- CalculateAUDPC(currentkeys=currentkeys,
	currentdata=currentdata,
	currentdata_utc=currentdata_utc,
	Trial= Trial,
	Treatment=Treatment,
	Rep=Rep, 
	UTC=UTC,
	TimeXaxis=TimeXaxis,
	Response=Response,
	title1=title1,
	title2=title2,
	title3=title3,
	PDFHeader1=PDFHeader1,
	PDFHeader2=PDFHeader2,
	PDFHeader3=PDFHeader3,
	Export=FALSE)	
    out <- CalculateAUDPC(currentkeys=currentkeys,
	currentdata=currentdata,
	currentdata_utc=currentdata_utc,
	Trial= Trial,
	Treatment=Treatment,
	Rep=Rep, 
	UTC=UTC,
	TimeXaxis=TimeXaxis,
	Response=Response,
	title1=title1,
	title2=title2,
	title3=title3,
	PDFHeader1=PDFHeader1,
	PDFHeader2=PDFHeader2,
	PDFHeader3=PDFHeader3,
	Export=TRUE)
    calculated_df <- rbind(calculated_df,out$df)
  }
  dev.off()
  return(list(url=fullpdffilename,pdfname=pdfname,calculated_df=calculated_df))
}

CalculateAUDPC <- function(currentkeys,
			currentdata,
			currentdata_utc,
			Trial,
			Treatment,
			Rep, 
			UTC,
			TimeXaxis,
			Response,
			title1,
			title2,
			title3,
			PDFHeader1,
			PDFHeader2,
			PDFHeader3,
			Export) {
  assign("Export",Export,envir = .GlobalEnv)
  if (Export==TRUE) {
    sqlgroupvars <-paste0('Treatment,',Trial,', TimeXaxis, ', Rep) 
   } else {
    sqlgroupvars <-paste0('Treatment,', Trial, ',TimeXaxis')
  }
  currentdata$TimeXaxis=currentdata[,TimeXaxis] #Use TimeXaxis as var name in sql in case Excel column names contains invalid characters
  currentdata_utc$TimeXaxis=currentdata_utc[,TimeXaxis] #Use TimeXaxis as var name in sql in case Excel column names contains invalid characters
  assign("sqlgroupvars",sqlgroupvars,envir = .GlobalEnv)	
  assign("currentdata",currentdata,envir = .GlobalEnv)	
  assign("currentdata_utc",currentdata_utc,envir = .GlobalEnv)	
  currentaverage=sqldf(paste0('select ', sqlgroupvars,  ', avg(', Response, ') as "', Response, '" from currentdata group by ',sqlgroupvars))
  currentaverage_utc=sqldf(paste0('select ', sqlgroupvars,  ', avg(', Response, ') as "', Response, '" from currentdata_utc group by ',sqlgroupvars))
  colnames(currentaverage)[which(colnames(currentaverage)=="TimeXaxis")] <- TimeXaxis #Use TimeXaxis as var name in sql in case Excel column names contains invalid characters
  colnames(currentaverage_utc)[which(colnames(currentaverage_utc)=="TimeXaxis")] <- TimeXaxis #Use TimeXaxis as var name in sql in case Excel column names contains invalid characters

  raudpc <-data.frame()
  if (Export==TRUE) {
    for (Replicate in levels(factor(currentaverage[,Rep]))) {
      currentaverage_rep <- subset(currentaverage,get(Rep)==Replicate)
      currentaverage_utc_rep <- subset(currentaverage_utc,get(Rep)==Replicate)
      for (trtmnt in levels(factor(currentaverage_rep[,"Treatment"]))) {
        try({
          evaluation<-currentaverage_rep[currentaverage_rep[,"Treatment"]==trtmnt,Response]
          dates<-currentaverage_rep[currentaverage_rep$Treatment==trtmnt,TimeXaxis]
          evaluation_utc<-currentaverage_utc_rep[currentaverage_utc_rep$Trial==currentkeys$Trial,Response]
          dates_utc<-currentaverage_utc_rep[currentaverage_utc_rep$Trial==currentkeys$Trial,TimeXaxis]
          df <- currentkeys
          df$Treatment <- trtmnt
          df$Rep <- Replicate
          if (length(evaluation)==1) evaluation <-c(0,evaluation)
          if (length(dates)==1) evaluation <-c(0,dates)
          if (length(evaluation_utc)==1) evaluation <-c(0,evaluation_utc)
          if (length(dates_utc)==1) evaluation <-c(0,dates_utc)
          df$totalarea = round(audpc(evaluation, dates, type = "absolute"),2)
          df$u_area = round(audpc(evaluation_utc, dates_utc, type = "absolute"),2)
          df$r_area = round(df$totalarea/df$u_area,6)
          df$std_area = round(df$totalarea/(max(dates)-min(dates)),6)
          raudpc <- rbind(raudpc,df)
        })
      }
    } 
    assign("raudpc",raudpc,envir = .GlobalEnv)  
    if (dim(raudpc)[1] > 0) {
      colnames(raudpc)<-c(colnames(raudpc)[1:(length(colnames(raudpc))-6)],Treatment, Rep, 'AUDPC','Untreated \nAUDPC','AUDPC Relative \nto Untreated','Standardized \nAUDPC')
    }
    return (list(df=raudpc))
  } else {
    for (trtmnt in levels(factor(currentaverage[,"Treatment"]))) {
      try({
        evaluation<-currentaverage[currentaverage[,"Treatment"]==trtmnt,Response]
        dates<-currentaverage[currentaverage$Treatment==trtmnt,TimeXaxis]
        evaluation_utc<-currentaverage_utc[currentaverage_utc$Trial==currentkeys$Trial,Response]
        dates_utc<-currentaverage_utc[currentaverage_utc$Trial==currentkeys$Trial,TimeXaxis]
        df <- data.frame(A=trtmnt)
        df$B=currentkeys$Trial 
        if (length(evaluation)==1) evaluation <-c(0,evaluation)
        if (length(dates)==1) evaluation <-c(0,dates)
        if (length(evaluation_utc)==1) evaluation <-c(0,evaluation_utc)
        if (length(dates_utc)==1) evaluation <-c(0,dates_utc)
        df$totalarea = round(audpc(evaluation, dates, type = "absolute"),2)
        df$u_area = round(audpc(evaluation_utc, dates_utc, type = "absolute"),2)
        df$r_area = round(df$totalarea/df$u_area,6)
        df$std_area = round(df$totalarea/(max(dates)-min(dates)),6)
        raudpc <- rbind(raudpc,df)
      })
    }
    assign("currentaverage",currentaverage,envir = .GlobalEnv)
    assign("currentaverage_utc",currentaverage_utc,envir = .GlobalEnv)
    assign("raudpc",raudpc,envir = .GlobalEnv)  
    if (dim(raudpc)[1] > 0) {
      raudpc <- raudpc[order(raudpc$totalarea),] 
      colnames(raudpc)<-c('Treatment','Trial No','AUDPC','Untreated \nAUDPC','AUDPC Relative \nto Untreated','Standardized \nAUDPC')
      #raudpc$Treatment <- gsub('\n',' ',raudpc$Treatment) 
      perpage=13
      tableno=ceiling(dim(raudpc)[1]/perpage)
      for (i in 1:tableno) {
        plot.new()
        grid.table(raudpc[((i-1)*perpage+1):min(i*perpage,dim(raudpc)[1]),], rows=NULL)
        tabletitle <- title1
        if (tableno!=1){
          tabletitle=paste0(tabletitle,'(',i,')')
        }
        grid.text(PDFHeader1,gp=gpar(cex=0.9,font=1),y=0.95,x=0.1,just='left')
        grid.text(PDFHeader2,gp=gpar(cex=0.9,font=1),y=0.93,x=0.1,just='left')
        grid.text(PDFHeader3,gp=gpar(cex=0.9,font=1),y=0.91,x=0.1,just='left')
        grid.text(title2,gp=gpar(cex=1.2,font=2),y=0.84,x=0.5,just='center')
        grid.text(title3,gp=gpar(cex=1.2,font=2),y=0.80,x=0.5,just='center')
        grid.text(tabletitle,gp=gpar(cex=1.2,font=2),y=0.88,x=0.5,just='center')
        add_footer() 
      }
    }
  }
}	
	
singlelineplot <- function(data,
			loopvar,
            Trial,
            Treatment,
			TimeXaxis,
			Response,
			TrtType,
			TrtBlack,
			TrtGreen,
			TrtRed,
			TrtPurple,
		        TrtOrange,
			xLabel,
			yLabel,
			title1,
			title2,
			title3,
			PDFHeader1,
			PDFHeader2,
			PDFHeader3,
			LegendFontSize) {
  assign("data",data,envir = .GlobalEnv)
  assign("loopvar",loopvar,envir = .GlobalEnv)
  assign("TimeXaxis",TimeXaxis,envir = .GlobalEnv)
  assign("Response",Response,envir = .GlobalEnv)
  assign("TrtType",TrtType,envir = .GlobalEnv)
  assign("TrtBlack",TrtBlack,envir = .GlobalEnv)
  assign("TrtGreen",TrtGreen,envir = .GlobalEnv)
  assign("TrtRed",TrtRed,envir = .GlobalEnv)
  assign("TrtPurple",TrtPurple,envir = .GlobalEnv)
  assign("TrtOrange",TrtOrange,envir = .GlobalEnv)
  assign("xLabel",xLabel,envir = .GlobalEnv)
  assign("yLabel",yLabel,envir = .GlobalEnv)
  assign("title1",title1,envir = .GlobalEnv)
  assign("title2",title2,envir = .GlobalEnv)
  assign("title3",title3,envir = .GlobalEnv)
  assign("PDFHeader1",PDFHeader1,envir = .GlobalEnv)
  assign("PDFHeader2",PDFHeader2,envir = .GlobalEnv)
  assign("PDFHeader3",PDFHeader3,envir = .GlobalEnv)
  # Create Line Chart
  x <- data[,TimeXaxis]
  y <- data$Response
  h <- (max(y)-min(y))/20
  color <- factor(data[,Treatment])
  titles=paste(title1, '\n',title2, '\n',title3)
  gp <- ggplot(data=data,aes(x = data[,TimeXaxis],y = data$Response, group=color,
                            color=color,shape=color))  +
  scale_shape_manual(values=1:nlevels(color)) + 
  #geom_line(aes(color=as.factor(get(Treatment))),size=1.2) +
  #geom_point(aes(shape=as.factor(get(Treatment) %% 6),color=as.factor(get(Treatment))),size=4)
  geom_line() + 
  geom_point(,size=4) +
  geom_text(x=min(x), y=max(y), label=PDFHeader1) +
  geom_text(x=min(x), y=max(y)-h, label=PDFHeader2) +
  geom_text(x=min(x), y=max(y)-2*h, label=PDFHeader3) +
  labs(title=titles,x =xLabel, y = yLabel) + 
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) 

  color.codes <- vector(mode="character", length=nlevels(color))
  assign("data",data,envir = .GlobalEnv)
  assign("TrtBlack",TrtBlack,envir = .GlobalEnv)
  assign("Treatment",Treatment,envir = .GlobalEnv)
  assign("TrtType",TrtType,envir = .GlobalEnv)
  if (TrtBlack > " "){
    for (i in 1:nlevels(color)) {
      if (unique(subset(data,get(Treatment)==levels(color)[i])[,TrtType])==TrtBlack)
        color.codes[i] <- "black" 
    }
  }
  if (TrtGreen > " "){
    for (i in 1:nlevels(color)) {
      if (unique(subset(data,get(Treatment)==levels(color)[i])[,TrtType])==TrtGreen)
        color.codes[i] <- "green"
    }
  }
  if (TrtRed > " "){
    for (i in 1:nlevels(color)) {
      if (unique(subset(data,get(Treatment)==levels(color)[i])[,TrtType])==TrtRed)
        color.codes[i] <- "red"
    }
  }
  if (TrtPurple > " "){
    for (i in 1:nlevels(color)) {
      if (unique(subset(data,get(Treatment)==levels(color)[i])[,TrtType])==TrtPurple)
        color.codes[i] <- "purple"
    }
  }  
  if (TrtOrange > " "){
    for (i in 1:nlevels(color)) {
      if (unique(subset(data,get(Treatment)==levels(color)[i])[,TrtType])==TrtOrange)
        color.codes[i] <- "orange"
    }
  } 
  mycolors <- color.codes[color.codes>" "] 
  color.codes[color.codes==""] <- fmc_colors()[1:length(color.codes[color.codes==""])]
  gp <- gp + scale_color_manual(values=setNames(color.codes,levels(color)))
  gp <-  gp + theme(legend.position="bottom",legend.text = element_text(size=LegendFontSize))
  footertext <- paste0("FMC Confidential     Reveal V1.1      ", Sys.Date())
  gp <- grid.arrange(gp, bottom = textGrob(footertext, 
                         just = "center", vjust=1,
                         gp = gpar(fontface = 3L, fontsize = 7)))
  print(gp)
}
