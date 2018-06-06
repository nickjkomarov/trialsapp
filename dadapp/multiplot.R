# pdfplotfunction.R
multi.pdfplot=function(data,pdfloopvar,loopvar,x,y,Plot_Sort_Order,trial,rep,colorby,alphalevel,response_type,normal_better, 
                       rating_better,modeled,Orientation, ShowEstMean, trt_label_font, OutputURL,Key_Weed_Species,
                       X_label,Y_label,pdf_plot_title,plot_title,subtitle, plot_type,significance,xticks, Confidential){
  
  assign('data',data,envir=.GlobalEnv)
  assign('pdfloopvar',pdfloopvar,envir=.GlobalEnv)
  assign('loopvar',loopvar,envir=.GlobalEnv)
  assign('x',x,envir=.GlobalEnv)
  assign('y',y,envir=.GlobalEnv)
  assign('Plot_Sort_Order',Plot_Sort_Order,envir=.GlobalEnv)
  assign('trial',trial,envir=.GlobalEnv)
  assign('rep',rep,envir=.GlobalEnv)
  assign('colorby',colorby,envir=.GlobalEnv)
  assign('alphalevel',alphalevel,envir=.GlobalEnv)
  assign('response_type',response_type,envir=.GlobalEnv)
  assign('normal_better',normal_better,envir=.GlobalEnv)
  assign('rating_better',rating_better,envir=.GlobalEnv)
  assign('modeled',modeled,envir=.GlobalEnv)
  assign('OutputURL',OutputURL,envir=.GlobalEnv)
  assign('X_label',X_label,envir=.GlobalEnv)
  assign('Y_label',Y_label,envir=.GlobalEnv)
  assign('pdf_plot_title',pdf_plot_title,envir=.GlobalEnv)
  assign('plot_title',plot_title,envir=.GlobalEnv)
  assign('subtitle',subtitle,envir=.GlobalEnv)
  assign('plot_type',plot_type,envir=.GlobalEnv)
  assign('significance',significance,envir=.GlobalEnv)
  
  #pdfset#
  pdfheight=8
  pdfwidth=10.5
  
  if(response_type=='Normal'){
    if(plot_type=="Bar Chart") {
      name='MultiBarChart'
    } else {
      name='MultiBoxPlot'
    }
  }
  if(response_type=='Rating Scale'){
    name='MultiRatingPlot'
  }
  if(response_type=='Herbicide Visual Response (0-100)'){
    name='HerbVisualResp'
  }
  if(response_type=='Phyto'){
    name='PhytoVisualResp'
  }
  pdfdatafilter=data
  pdfname=name

  perpage=20      
    
  OUtputURLlast=substr(OutputURL, nchar(OutputURL), nchar(OutputURL))
  separator=ifelse(OUtputURLlast!='\\','\\','')
  file.URL=paste0(OutputURL,separator,pdfname,format(Sys.time(), format='%Y%m%d%H%M%S'),'.pdf')
    
  pdf(onefile=TRUE,file=file.URL,paper="USr",width=pdfwidth,height=pdfheight)
  
  byvari=unique(c(loopvar,trial))
  content=unique(pdfdatafilter[,byvari])
  contentsummary=data.table(content)
    
  byvari2=unique(c(loopvar))
  contentsummary=contentsummary[,list(Evals.No=.N),by=byvari2]
    
  assign('byvari',byvari,envir=.GlobalEnv)
  contentsummary=as.data.frame(contentsummary) 
  colnames(contentsummary)[dim(contentsummary)[2]] <- "# of\nTrials"
  contentsummary <- sqldf(paste("select * from contentsummary order by ", paste(loopvar,collapse = ',')))
  
  if (response_type == 'Herbicide Visual Response (0-100)' | response_type=='Phyto') {
    data$x=data[,x]
    data$y=data[,y]
    tcdata1 <- plyr::ddply(data, byvari2, summarise, z = length(unique(x)))
    assign('tcdata1',tcdata1,envir=.GlobalEnv)
    contentsummary <- cbind(contentsummary,tcdata1$z)
    colnames(contentsummary)[dim(contentsummary)[2]] <- "# of\nTrts"
    tcdata2 <- plyr::count(data,vars = byvari2)
    contentsummary <- cbind(contentsummary,tcdata2$freq)
    colnames(contentsummary)[dim(contentsummary)[2]] <- "# of\nResps"
    tcdata3 <- plyr::ddply(data, byvari2, summarise, z = min(y))
    contentsummary <- cbind(contentsummary,tcdata3$z)
    colnames(contentsummary)[dim(contentsummary)[2]] <- "Min of\nResps" 
    tcdata4 <- plyr::ddply(data, byvari2, summarise, z = max(y))
    contentsummary <- cbind(contentsummary,tcdata4$z)
    colnames(contentsummary)[dim(contentsummary)[2]] <- "Max of\nResps" 
    contentsummary$"Summary\nType" <-"Statistical"
    contentsummary[contentsummary$"# of\nTrials"<2,"Summary\nType"]<-"Visual"
    colnames(contentsummary) <- gsub("_", "\n", colnames(contentsummary))
    assign('contentsummary',contentsummary,envir=.GlobalEnv)
    contentsummary1 <- contentsummary[contentsummary[,"Summary\nType"]=="Statistical",]
    contentsummary2 <- contentsummary[contentsummary[,"Summary\nType"]=="Visual",]
    contentsummary1<- contentsummary1[,c(1:(length(colnames(contentsummary1))-3))]
    contentsummary2<- contentsummary2[,c(1:(length(colnames(contentsummary2))-3))]
    if (dim(contentsummary1)[1]>0) {
      tableno=ceiling(dim(contentsummary1)[1]/perpage)
      for (i in 1:tableno) {
        plot.new()
        grid.table(contentsummary1[((i-1)*perpage+1):min(i*perpage,dim(contentsummary1)[1]),], rows=NULL)
        #tabletitle='Statistical Summary'
        tabletitle=" "
        if (tableno!=1){
          tabletitle=paste0(tabletitle,'(',i,')')
        }
        grid.text(tabletitle,gp=gpar(cex=1.2,font=2),y=0.9,x=0.5,just='center')
        add_footer(Confidential=Confidential) 
      }
    } 
    if (dim(contentsummary2)[1]>0) {
      tableno=ceiling(dim(contentsummary2)[1]/perpage)
      for (i in 1:tableno) {
        plot.new()
        grid.table(contentsummary2[((i-1)*perpage+1):min(i*perpage,dim(contentsummary2)[1]),], rows=NULL)
        tabletitle='Not Enough Trials for Statistical Analysis'
        if (tableno!=1){
          tabletitle=paste0(tabletitle,'(',i,')')
        }
        grid.text(tabletitle,gp=gpar(cex=1.2,font=2),y=0.9,x=0.5,just='center')
        add_footer(Confidential=Confidential) 
      }
    } 
  } else {   
    colnames(contentsummary) <- gsub("_", "\n", colnames(contentsummary))
    assign('contentsummary',contentsummary,envir=.GlobalEnv)
    tableno=ceiling(dim(contentsummary)[1]/perpage)
    for (i in 1:tableno) {
      plot.new()
      grid.table(contentsummary[((i-1)*perpage+1):min(i*perpage,dim(contentsummary)[1]),], rows=NULL)
      tabletitle=paste0(pdf_plot_title,'\n',subtitle,'\nTable of Contents')
      if (tableno!=1){
        tabletitle=paste0(tabletitle,'(',i,')')
      }
      grid.text(tabletitle,gp=gpar(cex=1.2,font=2),y=0.9,x=0.5,just='center')
      add_footer(Confidential=Confidential) 
    }
  }
  
  #add addional condition for response type    o 
   
  if (response_type == "Normal") {
    barboxplot(data=pdfdatafilter,loopvar=loopvar,x=x,y=y,Plot_Sort_Order=Plot_Sort_Order, trial=trial,rep=rep,normal_better=normal_better,colorby=colorby,
                    alphalevel=alphalevel,X_label=X_label,Y_label=Y_label,plot_title=plot_title,plot_type=plot_type, Orientation=Orientation, 
                    ShowEstMean=ShowEstMean, trt_label_font=trt_label_font, significance=significance, Confidential=Confidential) 
  } else if (response_type == "Rating Scale" ) {
    ordinal(data=pdfdatafilter,loopvar=loopvar,x=x,y=y,trial=trial,rep=rep,rating_better=rating_better,modeled=modeled,alphalevel=alphalevel,
                 X_label=X_label,Y_label=Y_label,plot_title=plot_title, Confidential=Confidential)
  } else if (response_type == 'Herbicide Visual Response (0-100)' | response_type=='Phyto') {
    multinomial(data=pdfdatafilter,loopvar=loopvar,x=x,y=y,trial=trial,rep=rep,alphalevel=alphalevel,Key_Weed_Species=Key_Weed_Species,
                 X_label=X_label,Y_label=Y_label,plot_title=plot_title,xticks=xticks, Confidential=Confidential)
  } 
    
  pdfid=dev.list()[names(dev.list())=='pdf']
  lapply(pdfid,function(x) {dev.off(which=x)})
    
  return(list(url=file.URL,pdfname=pdfname))
}


# plotfunction.R

barboxplot=function(data,loopvar,x,y,Plot_Sort_Order, trial,rep,normal_better,colorby,alphalevel,X_label,Y_label,
                    plot_title,plot_type,Orientation, ShowEstMean, trt_label_font, significance, Confidential)
{
  defaultSort=c('Country','Crop','Pestid','SMBP','symptom','method','basis','part')
  loopvar=prioritylist(defaultSort,loopvar)  
  fmc_footer <- paste0("\nFMC Confidential   Reveal V1.1 – Cross Trial Analysis, Tukey, ", significance, " ",Sys.Date())
  plotnum=dim(filter)[1]
  
  if (length(loopvar)==0){
    loopflag=FALSE
    plotnum=1
    datafilter=data
  } else {
    loopflag=TRUE
    filter=sqldf(paste("select distinct", paste(loopvar,collapse=','),'from data'))
    plotnum=dim(filter)[1]
  }
  
  for (ploti in 1:plotnum){
    if (loopflag){
      head=filter[ploti,] 
      if (length(loopvar)>1) {
        datafilter=sqldf(paste("select *",'from data natural inner join head' ))
      }else{
        datafilter=data[data[loopvar]==head,]
      } 
    }
    datafilter=as.data.frame(datafilter)
    datafilter$x=datafilter[,x]
    datafilter$y=datafilter[,y]
    datafilter$trial=datafilter[,trial]
    datafilter$rep=datafilter[,rep]
    datafilter$colvar=datafilter[,colorby]
    
    datafilter$y=as.numeric(datafilter[,'y'])
        
    datafilterby=unique(cbind(datafilter$trial,datafilter$colvar))
    datafilterby=as.data.frame(datafilterby)
    colnames(datafilterby)=c('trial','colvar')
    
    
    Trt<-as.factor(datafilter$x)
    Response<-datafilter$y
    Trial<-as.factor(datafilter$trial)
    Rep<-as.factor(datafilter$rep)
    color<-as.factor(datafilter$colvar)
    
    assign("data",data, envir = .GlobalEnv)
    assign("loopvar",loopvar, envir = .GlobalEnv)
    assign("Trt",Trt, envir = .GlobalEnv)
    assign("Response",Response, envir = .GlobalEnv)
    assign("Trial",Trial, envir = .GlobalEnv)
    assign("Rep",Rep, envir = .GlobalEnv)
    assign("color",color, envir = .GlobalEnv)
    
    newdata<-data.frame(Trial, Trt, Rep, Response, color)
    colortrt<-unique(newdata[c('Trt','color')])
    rownames(colortrt) <- NULL
    coloruni<-unique(colortrt$color)
    
    if (   (length(unique(Trt))!=1) & (length(unique(Trial))!=1) & (length(unique(Response))!=1) ){
      #XPJIN added 07/20/2017
      if (length(unique(Rep))==1) 
        model1<-tryCatch(lme4::lmer(Response~Trt+(1|Trial)),error=function(e){e})  
      else 
        #model1<-tryCatch(lme4::lmer(Response~1+Trt+(1|Trial:Trt)+(1|Trial/Rep)),error=function(e){e}) #interaction term ":", Rep nested in Trial(Trial/Rep)#
        model1<-tryCatch(lme4::lmer(Response~1+Trt+(1|Trial)+(1|Trial:Trt)+(1|Trial/Rep)),error=function(e){e})
      SumMod<-tryCatch(summary(model1),error=function(e){e}) #Intercept est is the baseline trt's est#
      modelEst<-tryCatch(lsmeans::lsmeans(model1,"Trt"),error=function(e){e})
      s <- tryCatch(summary(modelEst),error=function(e){e})
      Estable<-tryCatch(s[c("Trt","lsmean", "SE", "df", "lower.CL", "upper.CL")],error=function(e){e})
      glht1<-tryCatch(glht(model1, linfct = mcp(Trt = "Tukey")),error=function(e){e})
      
      modelflag=(c(class(model1)[1],
                   class(SumMod)[1],
                   class(modelEst)[1],
                   class(s)[1],
                   class(Estable)[1],
                   class(glht1)[1])=="simpleError")
      
      if (any(modelflag)){
        errorindex=min(which(modelflag))
        errorstring=c('model1','SumMod','modelEst','s','Estable','glht1')[errorindex]
        errormeg=conditionMessage(get(errorstring))
        
        plot.new()
        grid.text(errormeg,gp=gpar(cex=1.6,font=1),y=0.55,x=0.5,just='center')
        title(main=plot_title,xpd=T,line=3)
        
        plot_title2index=unique(c(loopvar))
        plot_title2index=prioritylist(defaultSort,plot_title2index)  
        if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])
        } else{
          plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')
        }
        
        par(font.main=1,cex.main=1)
        title(main=plot_title2,xpd=NA,line=1)
        assign("plot_title2",plot_title2, envir = .GlobalEnv)
        add_footer(Confidential=Confidential) 
      }else{
        if (length(glht1$coef)<=50){
          letter<-cld(summary(glht1),level=alphalevel,decreasing = (normal_better=="Higher"))  #summary(glht1) very slow when level of factors are high
          MeanSep<-toupper(letter$mcletters$Letters)
          #generate table for plot#
          trialcnt<- aggregate(data=newdata, Trial ~ Trt, function(x) length(unique(x))) #calculate no of trials by trt#
          total <- cbind(Estable,trialcnt,MeanSep)
          finaltable<-subset(total, select=c(Trt,lsmean, SE, df, lower.CL, upper.CL, Trial, MeanSep))
          #finaltable<-merge(colortrt,finaltable,by="Trt")
          #names(finaltable)[names(finaltable)=="color"] <- "Trt_Type"
          assign("finaltable",finaltable, envir = .GlobalEnv)
          #XPJ NEW
	  plot_title2index=unique(c(loopvar))
	  plot_title2index=prioritylist(defaultSort,plot_title2index)  
	  if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])
	  } else{
	    plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')
          }
          if(plot_type=="Bar Chart") {
            #Barchart  
	    X_label <- ""
	    if(is.null(Y_label)) Y_label <- "Response"
	    finaltable<-merge(colortrt,finaltable,by="Trt")
            if(!is.null(Plot_Sort_Order)) {
              finaltable$Trt <- factor(finaltable$Trt, Plot_Sort_Order) 
            }
	    outplot <- ggplot(finaltable, aes(x=Trt, y=lsmean, fill=Trt)) + geom_bar(stat="identity",width=0.6)
	    if (Orientation=='Horizontal') {
	      outplot <- outplot +  coord_flip()  
	    } else {
	      outplot <- outplot + theme(axis.text.x=element_text(angle=45, size=trt_label_font, hjust=.95, face="bold"))
	    }
            if (min(finaltable[,"Trial"])==max(finaltable[,"Trial"])) plot_title2a <- paste0("(n = ",min(finaltable[,"Trial"])," trials)")
            else plot_title2a <- " "
            #Plot title
            plot_footer <- plot_title2 
            #Table title
            plot_title2 <- break_str1(plot_title2,80,'\\|') 

	    outplot <- outplot +
	               labs(title = plot_title, subtitle =plot_title2a, x = X_label, y = Y_label) +
	               theme(legend.position="none") +
	               theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"))+
	               theme(plot.subtitle = element_text(hjust = 0.5, size=14, face="bold")) +  
	               geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), size=.3, width=.2, position=position_dodge(.9)) +
	               theme(axis.text.y = element_text(hjust = 1,size=13)) +
	               theme(plot.margin = unit(c(1,.25,1,1), "cm"))+
	               scale_fill_manual(values=trt_colors(finaltable)) +
                       scale_x_discrete(labels = get_wraper(32))
	    assign("outplot",outplot, envir = .GlobalEnv)
	    outplot.build = ggplot_build(outplot)  
	    xpos <- max(outplot.build$data[[2]]$xmax)*1.02
	    ypos <- 1.2*(layer_scales(outplot)$y$range$range[[2]]-layer_scales(outplot)$y$range$range[[1]])+layer_scales(outplot)$y$range$range[[1]]
	    ygap <- .2*(layer_scales(outplot)$y$range$range[[2]]-layer_scales(outplot)$y$range$range[[1]])
            if (ShowEstMean==TRUE) {
              if (Orientation=='Horizontal')  
                outplot <- outplot +  
                         geom_text(aes(label = "EstMean", x=xpos, y = ypos+ygap), size = 4)
              else
                outplot <- outplot +  
                         geom_text(aes(label = "EstMean", x=0, y = ypos+ygap), hjust=0, size = 4)
              outplot <- outplot +  
                         geom_text(aes(label = paste0('\n',round(lsmean,1)), y = ypos+ygap), size = 4) +
                         geom_text(aes(label = paste0('\n',MeanSep), y = ypos+2*ygap), size = 4)
            } else
              outplot <- outplot +  
                         geom_text(aes(label = paste0('\n',MeanSep), y = ypos+ygap), size = 4)
	    if (min(finaltable[,"Trial"])!=max(finaltable[,"Trial"])) {
              if (Orientation=='Horizontal')  
                outplot <- outplot +  
                         geom_text(aes(label = "Trials", x=xpos, y = ypos), size = 4)
              else
                outplot <- outplot +  
                         geom_text(aes(label = "Trials", x=0, y = ypos), hjust=0, size = 4)
	      outplot <- outplot +  
                         geom_text(aes(label = paste0('\n',Trial), y = ypos), size = 4) 
            }
          }
          if(plot_type=="Box Plot") {
            #Boxplot 
	    newdata1 <- sqldf("select A.*,B.Trial as 'Num.Trials', B.lsmean, B.MeanSep from newdata A, finaltable B where A.Trt=B.Trt")  
	    assign("newdata1",newdata1, envir = .GlobalEnv)
	    if(is.null(plot_title)) plot_title <- "Boxplot of Treatments"
	    #if(is.null(X_label)) X_label <- "Trt@Rate"
	    X_label <- ""
	    if(is.null(Y_label)) Y_label <- "Response"
            if(!is.null(Plot_Sort_Order)) {
              newdata1$Trt <- factor(newdata1$Trt, Plot_Sort_Order) 
            }
	    outplot <- ggplot(newdata1, aes(x=Trt, y=Response, fill=Trt))  
	    if (Orientation=='Horizontal') {
	      outplot <- outplot +  coord_flip() 
	    } else {
	      outplot <- outplot + theme(axis.text.x=element_text(angle=45, size=trt_label_font, hjust=.95, face="bold"))
	    }
            if (min(newdata1[,"Num.Trials"])==max(newdata1[,"Num.Trials"])) plot_title2a <- paste0("(n = ",min(newdata1[,"Num.Trials"])," trials)")
            else plot_title2a <- " "
            #Plot title
            plot_footer <- plot_title2
              
            #Table title
            plot_title2 <- break_str1(plot_title2,80,'\\|') 

	    outplot <- outplot + geom_boxplot(width=0.6) +
	                 stat_summary(fun.y="mean", geom="point", size=5,
	                 position=position_dodge(width=0.75),color="white") +  
	                 background_grid(minor='none') +
	                 labs(title = plot_title, subtitle =plot_title2a, x = X_label, y = Y_label) +
	                 theme(legend.position="none") +
	                 theme(plot.title = element_text(hjust = 0.5, size=14, face="bold"))+
	                 theme(plot.subtitle = element_text(hjust = 0.5, size=14, face="bold")) +   
	                 theme(axis.text.y = element_text(size=13,hjust = 1)) +
	                 theme(plot.margin = unit(c(1,.25,1,1), "cm")) +
	                 scale_fill_manual(values=trt_colors(newdata1)) +
                     scale_x_discrete(labels = get_wraper(32))
					 
	    assign("outplot",outplot, envir = .GlobalEnv)
	    outplot.build = ggplot_build(outplot)
	    xpos <- max(outplot.build$data[[1]]$xmax)  
	    ypos <- 1.2*(layer_scales(outplot)$y$range$range[[2]]-layer_scales(outplot)$y$range$range[[1]])+layer_scales(outplot)$y$range$range[[1]]
	    ygap <- .2*(layer_scales(outplot)$y$range$range[[2]]-layer_scales(outplot)$y$range$range[[1]])

          if (ShowEstMean==TRUE) {
            if (Orientation=='Horizontal')  
              outplot <- outplot +  
                       geom_text(aes(label = "EstMean", x=xpos, y = ypos+ygap), size = 4)
            else
              outplot <- outplot +  
                       geom_text(aes(label = "EstMean", x=0, y = ypos+ygap), hjust=0, size = 4)
            outplot <- outplot +  
                       geom_text(aes(label = paste0('\n',round(lsmean,1)), y = ypos+ygap), size = 4) +
                       geom_text(aes(label = paste0('\n',MeanSep), y = ypos+2*ygap), size = 4)
          } else
            outplot <- outplot +  
                       geom_text(aes(label = paste0('\n',MeanSep), y = ypos+ygap), size = 4)
	  if (min(newdata1[,"Num.Trials"])!=max(newdata1[,"Num.Trials"])) {
            if (Orientation=='Horizontal')  
              outplot <- outplot +  
                       geom_text(aes(label = "Trials", x=xpos, y = ypos), size = 4)
            else
              outplot <- outplot +  
                       geom_text(aes(label = "Trials", x=0, y = ypos), hjust=0, size = 4)
	    outplot <- outplot +  
                       geom_text(aes(label = paste0('\n',Num.Trials), y = ypos), size = 4) 
          } 
        }
	plot.new()
        perpage=20
        summarytable<-subset(finaltable, select=c(Trt,lsmean, SE, lower.CL, upper.CL, MeanSep, Trial))
        assign("summarytable",summarytable, envir = .GlobalEnv)
        summarytable <- summarytable[!duplicated(summarytable),]
        summarytable$lsmean<-round(summarytable$lsmean,2)
        summarytable$SE<-round(summarytable$SE,2)
        summarytable$lower.CL<-round(summarytable$lower.CL,2)
        summarytable$upper.CL<-round(summarytable$upper.CL,2)
        summarytable <- summarytable[order(summarytable$lsmean,decreasing = TRUE),]
        colnames(summarytable) <- c("Treatment", "Estimated\nMean", "Estimated\nStdErr", "Estimated\nLower\nBound", "Estimated\nUpper\nBound", "Tukey", "# Trials")
        tableno=ceiling(dim(summarytable)[1]/perpage)
        for (i in 1:tableno) {
          if (i>1) plot.new()
          grid.table(summarytable[((i-1)*perpage+1):min(i*perpage,dim(summarytable)[1]),], rows=NULL)
          if (tableno!=1){
            plot_title2=paste0(plot_title2,'(',i,')')
          }
          grid.text(plot_title2,gp=gpar(cex=1.2,font=2),y=0.95,x=0.5,just='center')
          footer1 <- paste0("Cross Trial Analysis, Tukey Means Comparison, ",significance)
          footer2 <- ""
          add_footer(footer1=footer1, footer2=footer2, Confidential=Confidential)
        } 
        footer1 <- plot_title2
        if(plot_type=="Bar Chart") footer2 <- "Cross Trial Analysis, Bar chart with estimated mean and confidence interval"
        if(plot_type=="Box Plot") footer2 <- "Cross Trial Analysis, Boxplot, Tukey"
        add_footer_g(outplot,footer1,footer2,f=9,Confidential=Confidential)

        #XPJ NEW
        }else{
          plot.new()
          grid.text("Error: Over 1000 Treatment Pairs.",gp=gpar(cex=1.6,font=1),y=0.55,x=0.5,just='center')
          title(main=plot_title,xpd=T,line=3)
          
          plot_title2index=unique(c(loopvar))
          plot_title2index=prioritylist(defaultSort,plot_title2index)  
          if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])
          } else{
            plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')
          }
          par(font.main=1,cex.main=1)
          title(main=plot_title2,xpd=NA,line=1)
          add_footer(Confidential=Confidential) 
        }
      } 
      } else if(length(unique(Response))==1){
        plot.new()
        grid.text("All responses are the same and no standard error is calcuated. \nModel is not fitted.",gp=gpar(cex=1.6,font=1),y=0.55,x=0.5,just='center')
        title(main=plot_title,xpd=T,line=3)
        
        plot_title2index=unique(c(loopvar))
        plot_title2index=prioritylist(defaultSort,plot_title2index)  
        if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])
        } else{
          plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')
        }
        par(font.main=1,cex.main=1)
        title(main=plot_title2,xpd=NA,line=1)
        add_footer(Confidential=Confidential) 
      }
    else{
      plot.new()
      grid.text("Only 1 trial or only 1 treatment is found for the grouping factors, \nno cross trial analysis is executed.",gp=gpar(cex=1.6,font=1),y=0.55,x=0.5,just='center')
      title(main=plot_title,xpd=T,line=3)
      
      plot_title2index=unique(c(loopvar))
      plot_title2index=prioritylist(defaultSort,plot_title2index)  
      if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])
      } else{
        plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')
      }
      par(font.main=1,cex.main=1)
      title(main=plot_title2,xpd=NA,line=1)
      add_footer(Confidential=Confidential) 
    }
  }
}


ordinal <- function(data,loopvar,x,y,trial,rep,rating_better,modeled,alphalevel,X_label,Y_label,plot_title,Confidential)
{
  assign("data",data, envir = .GlobalEnv)
  assign("x",x, envir = .GlobalEnv)
  assign("y",y, envir = .GlobalEnv)
  assign("trial",trial, envir = .GlobalEnv)
  assign("rep",rep, envir = .GlobalEnv)
  assign("rating_better",rating_better, envir = .GlobalEnv)
  assign("modeled",modeled, envir = .GlobalEnv)
  assign("alphalevel",alphalevel, envir = .GlobalEnv)
  assign("X_label",X_label, envir = .GlobalEnv)
  assign("Y_label",Y_label, envir = .GlobalEnv)
  assign("plot_title",plot_title, envir = .GlobalEnv)
  i=1
  if(plot_title=="Multi Trial Plot"){plot_title=paste0("Rating Plot\n(",rating_better," is better)")}
  
  defaultSort=c('Country','Crop','Pestid','SMBP','symptom','method','basis','part')
  loopvar=prioritylist(defaultSort,loopvar)  
  
  plotnum=dim(filter)[1]
  
  if (length(loopvar)==0){
    loopflag=FALSE
    plotnum=1
    datafilter=data
  } else {
    loopflag=TRUE
    filter=sqldf(paste("select distinct", paste(loopvar,collapse=','),'from data'))
    plotnum=dim(filter)[1]
    
  }
  
  
  for (ploti in 1:plotnum){
    if (loopflag){
      head=filter[ploti,]
      # head = as.character(head)
      if (length(loopvar)>1) {
        datafilter=sqldf(paste("select *",'from data natural inner join head' ))
      }else{
        datafilter=data[data[loopvar]==head,]
      }
    }
    
    datafilter=as.data.frame(datafilter)
    
    datafilter[,y] <- round(datafilter[,y])
    datafilter$x=datafilter[,x]
    datafilter$y=datafilter[,y]
    datafilter$trial=datafilter[,trial]
    datafilter$rep=datafilter[,rep]    
    
    Trt<-as.factor(datafilter$x)
    Response<-datafilter$y
    Trial<-as.factor(datafilter$trial)
    Rep<-datafilter$rep
    # color<-as.factor(datafilter$colvar)

    assign("Trt",Trt, envir = .GlobalEnv)
    assign("Response",Response, envir = .GlobalEnv)
    assign("Trial",Trial, envir = .GlobalEnv)
    assign("Rep",Rep, envir = .GlobalEnv)
 
    data1 = datafilter
    assign("data1",data1, envir = .GlobalEnv)
    
    #count the trial No in each treatment
    agg <- aggregate(x=data1[,trial], by=list(data1[,x]),function(x) length(unique(x)))
    #the treatment name list, whose trial No. is 1
    trt_name_list <- agg[agg[,2]==1,1]
    #remove treatment only appear in 1 trial
    data2<-data1[!data1[,x] %in% trt_name_list,]
    assign("data2",data2, envir = .GlobalEnv)
    
    if(nrow(data2)!=0){
      # levels of Response
      data2[,y] <- as.factor(as.numeric(data2[,y]))
      data2$NewResponse <- data2[,y]
      
      ModelData<-as.data.frame(data2)
      assign("ModelData",ModelData, envir = .GlobalEnv)
      
      # cdata
      cdata <- plyr::count(ModelData,vars = c(x,'NewResponse'))
      cdata <- data.table(cdata)
      assign("cdata",cdata, envir = .GlobalEnv)
      
      cumData <- cdata[,list(cumsum = cumsum(freq)), by=x]
      total1 <- cbind(cdata, cumData$cumsum)
      colnames(total1)[4] <- 'cumsum'

      sumtable <- aggregate(cdata$freq, by=list(Category=cdata[[x]]), FUN=sum)
      colnames(sumtable) <- c(x, 'totalCnt')
      total <- merge(total1,sumtable,by=x)
      total$cumFreq <- total$cumsum / total$totalCnt
      
      total$prop.freq <- total$freq/total$totalCnt
      
      total$sum <- as.numeric(total$NewResponse) * total$prop.freq
      
      SUMmean <-  total %>%
                    group_by(total[[x]]) %>%
                      summarise(mean=sum(sum))
      SUMmean <- as.data.frame(SUMmean)
      SUMmean$mean<-round(SUMmean$mean,2)
      colnames(SUMmean)[1] <- x
      try(total <- merge(total,SUMmean,by=x))
      total$mean <- total$mean -1
      try(total$mean <- paste0('mean=',total$mean))
      
      themeplot=theme(axis.text=element_text(size=8, face="bold"),                                                    #define theme for plot
                       axis.title=element_text(size=10,face="bold"),
                       legend.text=element_text(size=8,face="bold"),
                       legend.title=element_text(size=8,face="bold"),
                       strip.text=element_text(size=8,face="bold"),
                       plot.background = element_rect(fill = 'white', colour = 'white'),
                       panel.background = element_rect(fill = 'white', colour = 'grey', size=2),
                       panel.grid.major = element_line(colour = "white", size = 0.1, linetype = "dotted"),
                       panel.grid.minor = element_blank(),
                       plot.title = element_text(size = 8, face="bold"),
                       legend.position="bottom", legend.box = "horizontal",
                       plot.margin = unit(c(3,2,1,1), "cm"))
      
      
      
      
      
      plot <- ggplot()+
        geom_bar(data=total,aes(x=factor(NewResponse),y=prop.freq,fill=NewResponse),colour='black',stat='identity')+
        #geom_text(data=total,aes(x=factor(NewResponse),y=(freq+8),label=freq),size=2.5)+
         geom_text(data=total,aes(x=factor(NewResponse),y=(prop.freq+.03),label=percent(prop.freq)),size=2.8)+
         scale_y_continuous(labels = percent) +
        facet_wrap(~total[[x]])+
        themeplot+
        xlab('Rating')+
        geom_text(data=total,aes(x=mean(as.numeric(NewResponse)),y=max(prop.freq),label = mean))
        
      themeplot1=theme(axis.text=element_text(size=10, face="bold"),                                                    #define theme for plot
                       axis.title=element_text(size=16,face="bold"),
                       legend.text=element_text(size=10,face="bold"),
                       #legend.title=element_text(size=14,face="bold"),
                       strip.text=element_text(size=14,face="bold"),
                       plot.background = element_rect(fill = 'white', colour = 'white'),
                       panel.background = element_rect(fill = 'white', colour = 'grey', size=2),
                       panel.grid.major = element_line(colour = "white", size = 0.1, linetype = "dotted"),
                       panel.grid.minor = element_blank(),
                       plot.title = element_text(size = 16, face="bold"),
                       legend.position="bottom", legend.box = "horizontal",
                       plot.margin = unit(c(3,2,1,1), "cm"))
      
      themeplot2=theme(axis.text=element_text(size=12, face="bold"),                                                    #define theme for plot
                       axis.title=element_text(size=16,face="bold"),
                       legend.text=element_text(size=9,face="bold"),
                       #legend.title=element_text(size=14,face="bold"),
                       strip.text=element_text(size=14,face="bold"),
                       plot.background = element_rect(fill = 'white', colour = 'white'),
                       panel.background = element_rect(fill = 'white', colour = 'grey', size=2),
                       panel.grid.major = element_line(colour = "white", size = 0.1, linetype = "dotted"),
                       panel.grid.minor = element_blank(),
                       plot.title = element_text(size = 16, face="bold"),
                       legend.position="right", legend.box = "vertical",
                       plot.margin = unit(c(3,0.5,1,2), "cm"))
      
      pal <- colorRampPalette(brewer.pal(nlevels(ModelData[,y])*2,"Greens"))                  
      assign("pal",pal, envir = .GlobalEnv)
      
      ModelData$NewResponse <- factor(ModelData$NewResponse,levels=rev(levels(ModelData$NewResponse)))
      ModelData$x <- as.factor(ModelData$x)
      try(levels(ModelData$x) <- sapply(levels(ModelData$x),"break_str",45))
      plot1 = ggplot(ModelData,aes(x, fill=NewResponse)) + 
        geom_bar(position='fill') +      
        scale_fill_manual(values = rev(pal((nlevels(ModelData[,y])+2))[-c(1,2)])) + 
        coord_flip() + 
        scale_y_continuous(labels=percent) + 
        xlab('') +
        ylab('Percent') +
        themeplot1 +
        ggtitle("Bar Chart")  
      
      assign("total",total, envir = .GlobalEnv)
      
      total$Responses_Levels <- NA
      tr <- unique(total[[x]])
      sh <- sort(unique(total$NewResponse))
      Newtotal <- as.data.frame(matrix(NA,ncol=dim(total)[2]))
      colnames(Newtotal) <- colnames(total)
      for(t in tr){
        sub <- total[total[[x]]==t]
        if(any(!sh %in% sub$NewResponse)){
          No <- which(!sh %in% sub$NewResponse)
          if(any(No %in% 1)){
            makeUp <- sub[which(sh %in% sub$NewResponse)[1],]
            makeUp$NewResponse <- 0
            makeUp$Responses_Levels <- "Missing Point"
            sub <- rbind(sub,makeUp)
            
          }
          if(any(!No %in% 1)){
            No_ <- No[!No %in% 1]
            for(N in No_){
              makeUp <- sub[N-1,]
              makeUp$NewResponse <- sh[N]
              makeUp$Responses_Levels <- "Missing Point"
              sub <- rbind(sub,makeUp)
              sub <- sub[order(NewResponse),]
            }
            
          }
          sub <- sub[order(NewResponse),]
        }
        Newtotal <- rbind(Newtotal,sub)
      }
      Newtotal <- Newtotal[2:dim(Newtotal)[1],]
      print(Newtotal)
      Newtotal$Responses_Levels <- ifelse(is.na(Newtotal$Responses_Levels),"Observed Levels of Responses","Missing Levels of Responses")
      Newtotal$Responses_Levels <- as.character(Newtotal$Responses_Levels)
      
      if(rating_better=="Higher"){
        plot2 = ggplot(Newtotal, aes(x=NewResponse,y = cumFreq) ) + 
          geom_line(aes(colour=Newtotal[[x]], group=Newtotal[[x]])) +
          geom_point(aes(colour=Newtotal[[x]],shape=Responses_Levels), size=3) +
          themeplot2 +
          xlab('Rating') +
          labs(colour = x) +
          scale_shape_manual(values=c("Observed Levels of Responses"=19,"Missing Levels of Responses"=1))+
          ggtitle(expression(atop("Cummulative Line Chart", atop(italic("Higher Rating is better: Line at the bottom is the best"), ""))))
        
        
      }
      if(rating_better=="Lower"){
        plot2 = ggplot(Newtotal, aes(x=NewResponse,y = cumFreq) ) + 
          geom_line(aes(colour=Newtotal[[x]], group=Newtotal[[x]])) +
          geom_point(aes(colour=Newtotal[[x]],shape=Responses_Levels), size=3) +
          themeplot2 +
          xlab('Rating') +
          labs(colour = x) +
          scale_shape_manual(values=c("Observed Levels of Responses"=19,"Missing Levels of Responses"=1))+
          ggtitle(expression(atop("Cummulative Line Chart", atop(italic("Lower Rating is better: Line at the top is the best"), ""))))
        
      }
      plot <- plot  + labs(caption = paste0("\nFMC Confidential     Reveal V1.1      ", Sys.Date()))
      print(plot)
      title(main=plot_title,xpd=T,line=2)
      
      
      plot_title2index=unique(c(loopvar))
      plot_title2index=prioritylist(defaultSort,plot_title2index)  
      if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])} 
      else {plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')}
      
      par(font.main=1,cex.main=1)
      title(main=plot_title2,xpd=NA,line=1)
      
      plot1 <- plot1  + labs(caption = paste0("\nFMC Confidential     Reveal V1.1      ", Sys.Date()))
      print(plot1)
      title(main=plot_title,xpd=T,line=2)      
      
      plot_title2index=unique(c(loopvar))
      plot_title2index=prioritylist(defaultSort,plot_title2index)  
      if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])} 
      else {plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')}
      
      par(font.main=1,cex.main=1)
      title(main=plot_title2,xpd=NA,line=1)
      
      #print(plot2)
      title(main=plot_title,xpd=T,line=2)

      plot_title2index=unique(c(loopvar))
      plot_title2index=prioritylist(defaultSort,plot_title2index)  
      if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])} 
      else {plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')}
      
      par(font.main=1,cex.main=1)
      title(main=plot_title2,xpd=NA,line=1)

      formular <- as.formula(paste0("NewResponse ~ 1+",colnames(ModelData)[colnames(ModelData)==x],
                                    "+(1|",colnames(ModelData)[colnames(ModelData)==trial],"/",colnames(ModelData)[colnames(ModelData)==rep],
                                    ") + (1|",colnames(ModelData)[colnames(ModelData)==trial],":",colnames(ModelData)[colnames(ModelData)==x],")",sep=""))
      assign("formular",formular, envir = .GlobalEnv)

      Mx = as.factor(ModelData[,x])
      My = as.factor(ModelData[,y])
      Mt = as.factor(ModelData[,trial])
      Mr = as.factor(ModelData[,rep])
      
      assign("Mx",Mx, envir = .GlobalEnv)
      assign("My",My, envir = .GlobalEnv)
      assign("Mt",Mt, envir = .GlobalEnv)
      assign("Mr",Mr, envir = .GlobalEnv)
      
      # assign("ModelData",ModelData, envir = .GlobalEnv)
      
      lower_comment <- "For example, if the difference between two treatments is significant (based on mean separation table on prior page)
                      and the odds ratio comparing them is 3.2, it means that the odds of getting better (lower) ratings in the first treatment
                      are 3.2 times the odds of getting better (lower) ratings in the second treatment.\n
                      If the odds ratio estimate is larger than 1, the first treatment is more likely to get better/lower ratings that the second treatment.
                      If the odds ratio is smaller than 1, then the second treatment is more likely to get better/lower ratings than the first treatment."
      
      higher_comment <- "For example, if the difference between two treatments is significant (based on mean separation table on prior page)
                      and the odds ratio comparing them is 3.2, it means that the odds of getting better (higher) ratings in the first treatment
                      are 3.2 times the odds of getting better (higher) ratings in the second treatment.\n
                      If the odds ratio estimate is larger than 1, the first treatment is more likely to get better/higher ratings that the second treatment.
                      If the odds ratio is smaller than 1, then the second treatment is more likely to get better/higher ratings than the first treatment."
      
      comment <- ifelse(rating_better=='Higher',higher_comment,lower_comment)
      note <- "NOTE: The model and results are being validated !"
      
      if(modeled){  
        LS <- tryCatch({
          
          ModelData$NewResponse <- factor(ModelData$NewResponse,levels=rev(levels(ModelData$NewResponse)))
          assign("ModelData",ModelData, envir = .GlobalEnv)
          cat('clmm begin ...')
          mm <- clmm(ModelData$NewResponse ~ 1+Mx+(1|Mt/Mr)+(1|Mt:Mx),
                     link = "logit", threshold = "flexible", Hess = TRUE)
          cat('clmm end ...')
          assign("mm",mm, envir = .GlobalEnv)          
          print(summary(mm))
          #                   #cof = coef(mm)
          #                   #confint(mm, type = "Wald")
          #                   ls = lsmeans(mm, ~ Mx,type="response")
          #                   pp = summary(pairs(lsmeans(mm, ~ Mx,type="response")))
          #                   
          
          require(lsmeans)
          cat('lsmeans begin ...')
          ls = lsmeans::lsmeans(mm, ~ Mx,type="response")
          cat('lsmeans end ...')
          
          cat('pairs begin ...')
          pp = summary(pairs(lsmeans::lsmeans(mm, ~ Mx,type="response")))
          cat('pairs end ...')
          
          cat('alphalevel start')
          Comp = lsmeans::cld(ls, level = alphalevel, 
                              Letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
          cat('alphalevel end')
          
          assign("ls",ls, envir = .GlobalEnv)
          assign("pp",pp, envir = .GlobalEnv)
          
          #Treatment = Comp$Prog_Alias
          Comp = as.data.frame(Comp)
          Comp = Comp[order(row.names(Comp)),]
          assign("Comp",Comp, envir = .GlobalEnv)
          assign("ModelData",ModelData, envir = .GlobalEnv)
          Nobs = aggregate(data = ModelData, as.formula(paste0("cbind(count = NewResponse)",paste0("~ ",colnames(ModelData)[colnames(ModelData)==x],sep=""),sep="")),FUN = function(x){NROW(x)})
          Table1 = aggregate(data=ModelData, as.formula(paste0(colnames(ModelData)[colnames(ModelData)==trial], "~", colnames(ModelData)[colnames(ModelData)==x],sep="")), function(x) length(unique(x)))
          Table1 = cbind(Table1, Nobs[,2])
          Table1 <- sqldf("select A.*, B.'.group' as MeanSep from Table1 A, Comp B where A.Prog_Alias=B.Mx")
          AvgResp = aggregate(data = ModelData, as.formula(paste0("cbind(count = NewResponse)", paste0("~ ",colnames(ModelData)[colnames(ModelData)==x],sep=""),sep="")),FUN = function(x){round(mean(x),2)})
	  Table1 = cbind(Table1, AvgResp[,2]-1)
	  Table1<-Table1[,c(1,5,4,2,3)]
          colnames(Table1) = c('Treatment', 'Avg\nResponse', 'MeanSep', 'No. of\nTrials', 'No. of\n Obs')
          Table1$Treatment <- as.factor(Table1$Treatment)
          try(levels(Table1$Treatment) <- sapply(levels(Table1$Treatment),"break_str",80))
          
          estimate = round(exp(-pp$estimate), digits = 2)
          Table2 = cbind(as.character(pp$contrast), estimate, round(pp$p.value,4))
          colnames(Table2) = c('Trt Comparison', 'Odds Ratio Estimate','p-value')
          Table2 = as.data.frame(Table2)
          
          Table2$`Trt Comparison` <- as.factor(gsub('-','vs',Table2$`Trt Comparison`))
          try(levels(Table2$`Trt Comparison`) <- sapply(levels(Table2$`Trt Comparison`),"break_str",80))
          
          assign("Table1",Table1, envir = .GlobalEnv)
          assign("Table2",Table2, envir = .GlobalEnv)
          
          grid.newpage()
          grid.table(Table1,rows=NULL)
          subtitle <- paste0("\n\nTreatment Summary and Tukey Means Comparison at ",100*(1-alphalevel)) 
          subtitle <- paste0(subtitle,"% Confidence Level.")
          plot_title1 <- paste(plot_title,subtitle,sep='\n')
          grid.text(plot_title1,gp=gpar(cex=1.1,font=2),y=0.95,x=0.5,just='center')
          
          plot_title2index=unique(c(loopvar))
          plot_title2index=prioritylist(defaultSort,plot_title2index)  
          if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])} 
          else {plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')}
          
          #par(font.main=1,cex.main=1)
          grid.text(plot_title2,gp=gpar(cex=1.1,font=2),y=0.88,x=0.5,just='center')
          add_footer(Confidential=Confidential) 
          
          grid.newpage()
          perpage=14      
          tableno=ceiling(dim(Table2)[1]/perpage)
          this_title <- plot_title
          for (i in 1:tableno) {
            if (i>1) grid.newpage()
            grid.table(Table2[((i-1)*perpage+1):min(i*perpage,dim(Table2)[1]),], rows=NULL)
            if (tableno!=1){
              this_title=paste0(plot_title,'(',i,')')
            }
            grid.text(this_title,gp=gpar(cex=1.2,font=2),y=0.95,x=0.5,just='center')
            grid.text(plot_title2,gp=gpar(cex=1.1,font=2),y=0.9,x=0.5,just='center')
            add_footer(Confidential=Confidential) 
          }
          assign("plot_title2",plot_title2, envir = .GlobalEnv)

          grid.newpage()
          grid.text(paste0("Instruction for Reading Odds Ratio Estimates : \n\n",comment),gp=gpar(cex=1,font=1.2),y=0.6,x=0.5,just='center')
          
          title(main=plot_title,xpd=T,line=2)
          
          plot_title2index=unique(c(loopvar))
          plot_title2index=prioritylist(defaultSort,plot_title2index)  
          if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])} 
          else {plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')}
          
          par(font.main=1,cex.main=1)
          title(main=plot_title2,xpd=NA,line=1) 
          add_footer(Confidential=Confidential) 
        },
        
        error=function(e){
          
          p <-"Hessian is not positive definite"
          
          if(grepl(p,e)){
            par(font.main=1,cex.main=1)
            title(main=plot_title2,xpd=NA,line=1)
            
            grid.newpage()
            grid.text(paste("Convergence did not meet, No Summary for mean separations and Odds Ratio Estimates"),gp=gpar(cex=1,font=1),y=0.55,x=0.5,just='center')
            title(main=plot_title,xpd=T,line=2)
            
            
            plot_title2index=unique(c(loopvar))
            plot_title2index=prioritylist(defaultSort,plot_title2index)  
            if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])} 
            else {plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')}
            par(font.main=1,cex.main=1)
            title(main=plot_title2,xpd=NA,line=1)
            add_footer(Confidential=Confidential) 
          }
        }
        )
      }
      
    }
    
    else if(nrow(data2)==0){
      plot.new()
      grid.text(paste("All treatments have only 1 Trial"),gp=gpar(cex=1,font=1),y=0.55,x=0.5,just='center')
      title(main=plot_title,xpd=T,line=2)
      plot_title2index=unique(c(loopvar))
      plot_title2index=prioritylist(defaultSort,plot_title2index)   
      if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])} 
      else {plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')}
      par(font.main=1,cex.main=1)
      title(main=plot_title2,xpd=NA,line=1)
      add_footer(Confidential=Confidential) 
    }
    
  }
  
}

multinomial <- function(data,loopvar,x,y,trial,rep, alphalevel,Key_Weed_Species,X_label,Y_label,plot_title,xticks,Confidential)
{
  defaultSort=c('Country','Crop','Pestid','SMBP','symptom','method','basis','part')
  loopvar=prioritylist(defaultSort,loopvar)  
  
  plotnum=dim(filter)[1]
  
  if (length(loopvar)==0){
    loopflag=FALSE
    plotnum=1
    datafilter=data
  } else {
    loopflag=TRUE
    filter=sqldf(paste("select distinct", paste(loopvar,collapse=','),'from data'))
    plotnum=dim(filter)[1]
  }

  for (ploti in 1:plotnum){
    if (loopflag){
      head=filter[ploti,]
      if (length(loopvar)>1) {
        datafilter=sqldf(paste("select *",'from data natural inner join head' ))
      }else{
        datafilter=data[data[loopvar]==head,]
      }
    }
    
    datafilter=as.data.frame(datafilter)
    #uniform variable name    
    datafilter$x=datafilter[,x]
    datafilter$y=datafilter[,y]
    datafilter$trial=datafilter[,trial]
    datafilter$rep=datafilter[,rep]    
    
    Trt<-as.factor(datafilter$x)
    Response<-datafilter$y
    Trial<-as.factor(datafilter$trial)
    Rep<-datafilter$rep

    data1 = datafilter
    
    #count the trial No in each treatment
    agg <- aggregate(x=data1[,trial], by=list(data1[,x]),function(x) length(unique(x)))
    #the treatment name list, whose trial No. is 1
    trt_name_list <- agg[agg[,2]==1,1]
    #remove treatment only appear in 1 trial
    data2<-data1[!data1[,x] %in% trt_name_list,]
    plot_title2index=unique(c(loopvar))
    plot_title2index=prioritylist(defaultSort,plot_title2index)  
    if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])} 
    else {plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')}
    if(nrow(data2)!=0){
      #Delete Trials with all 0 or 100
      trt_avg <- aggregate( y ~ x, data2, mean )
      trts_0 <- trt_avg[trt_avg$y==0,]$x
      trts_100 <- trt_avg[trt_avg$y==100,]$x
      	#trts_0 <- trt_avg[trt_avg$y<=53,]$x 	#TESTING ONLY
	#trts_100 <- trt_avg[trt_avg$y>68,]$x	#TESTING ONLY
      sep_trts <- data2[data2$x %in% c(trts_0,trts_100),]
      cntl_trts <- data2[!(data2$x %in% c(trts_0,trts_100)),]
      colnames(sep_trts)[2] <- "trt_avg"
      colnames(cntl_trts)[2] <- "trt_avg"
      ModelData<-as.data.frame(cntl_trts)
      PlotData<-as.data.frame(data2)
      assign("PlotData",PlotData, envir = .GlobalEnv)

      # cdata
      cdata <- plyr::count(PlotData,vars = c(x,'Rating'))
      cdata <- data.table(cdata)
      
      cumData <- cdata[,list(cumsum = cumsum(freq)), by=x]
      total1 <- cbind(cdata, cumData$cumsum)
      colnames(total1)[4] <- 'cumsum'

      sumtable <- aggregate(cdata$freq, by=list(Category=cdata[[x]]), FUN=sum)
      colnames(sumtable) <- c(x, 'totalCnt')
      total <- merge(total1,sumtable,by=x)
      total$cumFreq <- total$cumsum / total$totalCnt
      
      total$prop.freq <- total$freq/total$totalCnt
      SUMmean <- ddply(PlotData, x, summarise,  avg = mean(y), std = sd(y))
      colnames(SUMmean) <- c(x,'avg','std')
      try(total <- merge(total,SUMmean,by=x))
      #define theme for plot
      themeplot=theme(axis.text=element_text(size=8, face="bold"),                                                    
                       axis.title=element_text(size=10,face="bold"),
                       legend.text=element_text(size=8,face="bold"),
                       legend.title=element_text(size=8,face="bold"),
                       strip.text=element_text(size=8,face="bold"),
                       plot.background = element_rect(fill = 'white', colour = 'white'),
                       panel.background = element_rect(fill = 'white', colour = 'grey', size=2),
                       panel.grid.major = element_line(colour = "white", size = 0.1, linetype = "dotted"),
                       panel.grid.minor = element_blank(),
                       plot.title = element_text(size = 14, hjust = 0.5, face="bold"),
                       plot.subtitle = element_text(size = 12, hjust = 0.5, face="bold"),
                       legend.position="bottom", legend.box = "horizontal",
                       plot.margin = unit(c(3,2,1,1), "cm"))
      substitle <- paste(paste0("mean=",round(total$avg,2)),paste0("std=",round(total$std,2))) 
      total$z <- paste(total[,get(x)],substitle,sep='\n')
      #Show all bins on x even when no data
      if(dim(total)[1] > 0){
        try({
          df <-data.frame(Prog_Alias=total[,get(x)][1],
                      Rating=setdiff(xticks,total$Rating),
                      freq=0,cumsum=0,totalCnt=0,cumFreq=0,prop.freq=0,
                      avg=0,std=0, z=total$z[1]
                )
          total<-rbind(total,df)
        })
      }
      plot <- ggplot()+
        geom_bar(data=total,aes(x=gsub('_','_\n',gsub('100-100','100',gsub('%','',factor(Rating)))),
                                y=prop.freq,fill=Rating),colour='black',stat='identity')+
        geom_text(data=total,aes(x=gsub('_','_\n',gsub('100-100','100',gsub('%','',factor(Rating)))),
                                 y=(prop.freq+.1),label=gsub(' 0%','',paste(' ',percent(round(prop.freq,2))))),size=2.8)+
        scale_y_continuous(labels = percent) +
        facet_wrap(~z, ncol = 3)+
        themeplot +
        labs(title = "Bar Chart by Treatment", subtitle =plot_title2, x = 'Rating', y = 'Percent') 
      plot <- plot  + labs(caption = paste0("\nFMC Confidential     Reveal V1.1      ", Sys.Date()))
      print(plot)      
      
      formular <- as.formula(paste0("Rating ~ 1+",colnames(ModelData)[colnames(ModelData)==x],
                                    "+(1|",colnames(ModelData)[colnames(ModelData)==trial],"/",colnames(ModelData)[colnames(ModelData)==rep],
                                    ") + (1|",colnames(ModelData)[colnames(ModelData)==trial],":",colnames(ModelData)[colnames(ModelData)==x],")",sep=""))
      Mx = as.factor(ModelData[,x])
      My = as.factor(ModelData[,y])
      Mt = as.factor(ModelData[,trial])
      Mr = as.factor(ModelData[,rep])
      ModelData$Rating <- as.factor(ModelData$Rating)
      ModelData$R <- as.factor(substr(ModelData$Rating,1,1))
      response <- as.numeric(ModelData[,y])
 assign("ModelData",ModelData, envir = .GlobalEnv)
 assign("Mx",Mx, envir = .GlobalEnv)
 assign("My",My, envir = .GlobalEnv)
 assign("Mt",Mt, envir = .GlobalEnv)
 assign("Mr",Mr, envir = .GlobalEnv)
 assign("ModelData",ModelData, envir = .GlobalEnv)
 
      RS <- try({
        convergence <- 0
        if (convergence==0) {
          mm <- clmm(ModelData$R ~ 1+Mx+(1|Mt/Mr)+(1|Mt:Mx), link = "logit", threshold = "flexible", Hess = TRUE)
          convergence <- mm$optRes[["convergence"]]
        } else  convergence <- 1
          
        if (convergence==1) {
          mm <- clmm(ModelData$Rating ~ 1+Mx+Mt/Mr+(1|Mt:Mx), link = "logit", threshold = "flexible", Hess = TRUE)
          convergence <- mm$optRes[["convergence"]]
        }  
        
        require(lsmeans)
        cat('lsmeans begin ...')
	ls = lsmeans::lsmeans(mm, ~ Mx,type="response")
	cat('lsmeans end ...')
	         
	cat('alphalevel start')
	Comp = lsmeans::cld(ls, level = alphalevel, Letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        cat('alphalevel end')  
      })
      if(!is.null(RS)) {
        par(font.main=1,cex.main=1)
        title(main=plot_title2,xpd=NA,line=1)
        grid.newpage()
        grid.text(paste("Convergence did not meet, No Summary for mean separations."),gp=gpar(cex=1,font=1),y=0.55,x=0.5,just='center')
        title(main=plot_title,xpd=T,line=2)
        par(font.main=1,cex.main=1)
        title(main=plot_title2,xpd=NA,line=1)
        add_footer(Confidential=Confidential) 
      } else {
        pt <- plot_table(ModelData, mm, ls,Comp, y, alphalevel, plot_title2,sep_trts,trts_0,trts_100)
        PlotData$Rating <- as.factor(PlotData$Rating)
        BoxPlot(PlotData, pt$Table1, pt$Comp, plot_title2,X_label,Y_label)
      }
    }  
    else if(nrow(data2)==0){
      plot_table1(datafilter,plot_title2)
      plot.new()
      grid.text(paste("All treatments have only 1 Trial"),gp=gpar(cex=1,font=1),y=0.55,x=0.5,just='center')
      title(main=plot_title,xpd=T,line=2)
      plot_title2index=unique(c(loopvar))
      plot_title2index=prioritylist(defaultSort,plot_title2index)      
      if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])} 
      else {plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')}
      par(font.main=1,cex.main=1)
      title(main=plot_title2,xpd=NA,line=1)
      add_footer(Confidential=Confidential) 
    }   
  }  
}

plot_table <- function(ModelData, mm, ls,Comp, y, alphalevel,plot_title2,sep_trts,trts_0,trts_100)
{
  assign("ModelData",ModelData, envir = .GlobalEnv)
  assign("mm",mm, envir = .GlobalEnv)
  assign("ls",ls, envir = .GlobalEnv)
  assign("Comp",Comp, envir = .GlobalEnv)
  assign("y",y, envir = .GlobalEnv)
  assign("sep_trts",sep_trts, envir = .GlobalEnv)
  assign("trts_0",trts_0, envir = .GlobalEnv)
  assign("trts_100",trts_100, envir = .GlobalEnv)
  Nobs = aggregate(data = ModelData, as.formula(paste0("cbind(count = Rating)",paste0("~ ",colnames(ModelData)[colnames(ModelData)==x],sep=""),sep="")),FUN = function(x){NROW(x)})
  Table1 = aggregate(data=ModelData, as.formula(paste0(colnames(ModelData)[colnames(ModelData)==trial], "~", colnames(ModelData)[colnames(ModelData)==x],sep="")), function(x) length(unique(x)))
  Table1 = cbind(Table1, Nobs[,2])
  Comp <- as.data.frame(Comp)
  colnames(Comp)[colnames(Comp)==".group"]<-"MeanSep"
  Table1 <- sqldf("select A.*, B.MeanSep from Table1 A, Comp B where A.Prog_Alias=B.Mx")
  Avg <- aggregate(data=ModelData, as.formula(paste0(colnames(ModelData)[colnames(ModelData)==y], "~", colnames(ModelData)[colnames(ModelData)==x],sep="")), function(x) round(mean(x),2))
  Med <- aggregate(data=ModelData, as.formula(paste0(colnames(ModelData)[colnames(ModelData)==y], "~", colnames(ModelData)[colnames(ModelData)==x],sep="")), function(x) round(median(x),2))
  Table1 = cbind(Table1, Avg[,2], Med[,2])
  Sdev <- aggregate(data=ModelData, as.formula(paste0(colnames(ModelData)[colnames(ModelData)==y], "~", colnames(ModelData)[colnames(ModelData)==x],sep="")), function(x) round(sd(x),2))
  Table1 = cbind(Table1, round(Sdev[,2],1))
  colnames(Table1) = c('Treatment', 'No. of Trials', 'No. of Obs', 'MeanSep','Avg Response','Median Response','Std Deviation') 
  if(dim(sep_trts)[1]>0) {
    tcdata1 <- plyr::ddply(sep_trts,x, summarise, z = length(unique(trial)))
    tcdata2 <- plyr::ddply(sep_trts,x, summarise, w = length(trial))
    sep_trts0100 <- data.frame(tcdata1[,1],tcdata1$z,tcdata2$w,' ',0,0,0)
    colnames(sep_trts0100) = c('Treatment', 'No. of Trials', 'No. of Obs', 'MeanSep','Avg Response','Med Response','Std Deviation') 
    sep_trts0 <- sep_trts0100[sep_trts0100[,1] %in% trts_0,]
    sep_trts100 <- sep_trts0100[sep_trts0100[,1] %in% trts_100,]
    try(sep_trts100[,'Avg Response']<-100)
    try(sep_trts100[,'Med Response']<-100)
    Table1 <-sqldf("select * from sep_trts100 union select * from Table1 union select * from sep_trts0")
    Table1 <- Table1[c(1,5,6,7,4,2,3)]
    Table1 <- Table1[order(-Table1$"Avg Response"),] 
  }
  plot_title <- "Trt Comparison Based on Multinomial Model" 
  plot_title <- paste(plot_title,plot_title2,sep='\n')
  subtitle <- paste0("Method= laplace at Alpha level=",alphalevel) 
  plot_title <- paste(plot_title,subtitle,sep='\n')
  subtitle <- paste0("Response is:",y)
  plot_title <- paste(plot_title,subtitle,sep='\n')
  plot_title <- paste0('\n',plot_title)
  plot.new()
  grid.table(Table1,rows=NULL)
  title(main=plot_title)  
  add_footer(Confidential=Confidential) 
  return(list(Table1=Table1,Comp=Comp))
}


plot_table1 <- function(datafilter,plot_title2)
{
  assign("datafilter",datafilter, envir = .GlobalEnv)
  Nobs = aggregate(data = datafilter, as.formula(paste0("cbind(count = Rating)",paste0("~ ",colnames(datafilter)[colnames(datafilter)==x],sep=""),sep="")),FUN = function(x){NROW(x)})
  Table1 = aggregate(data=datafilter, as.formula(paste0(colnames(datafilter)[colnames(datafilter)==trial], "~", colnames(datafilter)[colnames(datafilter)==x],sep="")), function(x) length(unique(x)))
  Table1 = cbind(Table1, Nobs[,2])
  Avg <- aggregate(data=datafilter, as.formula(paste0(colnames(datafilter)[colnames(datafilter)==y], "~", colnames(datafilter)[colnames(datafilter)==x],sep="")), function(x) round(mean(x),2))
  Med <- aggregate(data=datafilter, as.formula(paste0(colnames(datafilter)[colnames(datafilter)==y], "~", colnames(datafilter)[colnames(datafilter)==x],sep="")), function(x) round(median(x),2))
  Table1 = cbind(Table1, Avg[,2], Med[,2])
  Sdev <- aggregate(data=datafilter, as.formula(paste0(colnames(datafilter)[colnames(datafilter)==y], "~", colnames(datafilter)[colnames(datafilter)==x],sep="")), function(x) round(sd(x),2))
  Table1 = cbind(Table1, round(Sdev[,2],1))
  colnames(Table1) = c('Treatment', 'No. of Trials', 'No. of Obs','Avg Response','Median','Std Deviation') 
  plot_title <- "Trt Comparison Based on Multinomial Model" 
  plot_title <- paste(plot_title,plot_title2,sep='\n')
  plot_title <- paste0('\n',plot_title)
  plot.new()
  grid.table(Table1,rows=NULL)
  title(main=plot_title)  
  add_footer(Confidential=Confidential) 
}

BoxPlot <- function(PlotData, Table1, Comp, plot_title2,X_label,Y_label)
{
assign("PlotData",PlotData, envir = .GlobalEnv)
assign("Table1",Table1, envir = .GlobalEnv)
assign("plot_title2",plot_title2, envir = .GlobalEnv)
assign("X_label",X_label, envir = .GlobalEnv)
assign("Y_label",Y_label, envir = .GlobalEnv)
assign("Comp",Comp, envir = .GlobalEnv)

  mixed2 <- mixed(y ~ x + (1 | trial) + (1 | trial:x), PlotData,type=2, method="LRT")
  modelEst <-lsmeans(mixed2$full_model[[1]], specs = c("x"))

  t1 <- data.frame(x=Table1[,"Treatment"],Num.Trials=Table1[,"No. of Trials"])
  try(t2 <-data.frame(x=modelEst$lsmeans.table$x,lsmean=round(modelEst$lsmeans.table$Estimate, digits = 1)))
  #try(t2 <-data.frame(x=as.data.frame(modelEst)$x,lsmean=round(as.data.frame(modelEst)$lsmean, digits = 1)))
  try(t2 <-data.frame(x=as.data.frame(modelEst$lsmeans.table)$x,lsmean=round(as.data.frame(modelEst$lsmeans.table)$Estimate, digits = 1)))

  t3 <-data.frame(x=PlotData$x,y=PlotData$y)
  t <- t1
  t <- merge(t,t2, by="x") 
  #t4 <- data.frame(x=Table1[,"Treatment"],Med.Response=Table1[,"Med Response"])
  t4 <- data.frame(x=Table1[,"Treatment"],Med.Response=Table1[,"Median Response"])
  t <- merge(t,t4, by="x") 
  newdata1 <- merge(t,t3, by="x")
  plot_title <- "Boxplot of Treatments"
  if(is.null(X_label)) X_label <- "Trt@Rate"
  if(is.null(Y_label)) Y_label <- "Response"
  outplot <- ggplot(newdata1, aes(x=x, y=y, fill=x))  + geom_boxplot(width=0.6) +
	 stat_summary(fun.y="mean", geom="point", size=5,
	 position=position_dodge(width=0.75),color="white") + coord_flip() +
	 background_grid(minor='none') +
	 labs(title = plot_title, subtitle =plot_title2, x=" ",y = Y_label) +
	 theme(legend.position="none") +
	 theme(plot.title = element_text(hjust = 0.5, size = 14))+
	 theme(plot.subtitle = element_text(hjust = 0.5, size = 12)) +   
	 theme(axis.text.y = element_text(size=11,hjust = 1)) +
	 theme(plot.margin = unit(c(1,.25,1,1), "cm")) +
     scale_x_discrete(labels = get_wraper(32))
  outplot.build = ggplot_build(outplot)
  xpos <- max(outplot.build$data[[1]]$xmax) 
  ypos <- 1.3*(layer_scales(outplot)$y$range$range[[2]]-layer_scales(outplot)$y$range$range[[1]])+layer_scales(outplot)$y$range$range[[1]]
  ygap <- .2*(layer_scales(outplot)$y$range$range[[2]]-layer_scales(outplot)$y$range$range[[1]])
  y1 <- floor(min(outplot.build$data[[1]]$ymin_final)/5)*5
  y6 <- ceiling(max(outplot.build$data[[1]]$ymax_final)/5)*5
  y2  <- y1+(y6-y1)/5
  y3  <- y1+(y6-y1)/5*2
  y4  <- y1+(y6-y1)/5*3
  y5  <- y1+(y6-y1)/5*4
  outplot <- outplot +
      #geom_text(aes(label = "Trials   Median   EstMean", x=xpos+.1, y = ypos), size = 4) +
      geom_text(aes(label = "Trials",x=xpos+.1,y = ypos), size = 4) +
      geom_text(aes(label = "Median",x=xpos+.1,y = ypos+ygap), size = 4) +
      geom_text(aes(label = "EstMean",x=xpos+.1,y = ypos+2*ygap), size = 4) +      
      geom_text(aes(label = paste0('\n',Num.Trials),y = ypos), size = 4) +
      geom_text(aes(label = paste0('\n',Med.Response),y = ypos+ygap), size = 4) +
      geom_text(aes(label = paste0('\n',round(lsmean,1)), y = ypos+2*ygap), size = 4) +
      theme(axis.text.x = element_blank())  +
      geom_text(aes(label = as.character(y1), x=.5, y =y1, size = 2.2)) +
      geom_text(aes(label = as.character(y2), x=.5, y =y2, size = 2.2)) +
      geom_text(aes(label = as.character(y3), x=.5, y =y3, size = 2.2)) +
      geom_text(aes(label = as.character(y4), x=.5, y =y4, size = 2.2)) +
      geom_text(aes(label = as.character(y5), x=.5, y =y5, size = 2.2)) +
      geom_text(aes(label = as.character(y6), x=.5, y =y6, size = 2.2)) 
  outplot <- outplot  + labs(caption = paste0("\nFMC Confidential     Reveal V1.1      ", Sys.Date()))
  
  print(outplot)
}