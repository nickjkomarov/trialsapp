############### pdfplotfunction.R
box.barchart.pdfplot=function(data,loopvar,x,y,Plot_Sort_Order, single_grouped, grouped_var, plot_type, outlier, 
                              ErrorBarType, Statistics, Orientation,OutputURL, pdf_plot_title, 
                              plot_title,colorby,subtitle,trt_label_font, ResponseLabel){ 
  #pdfset#
  pdfheight=8
  pdfwidth=10.5
  assign('data',data,envir=.GlobalEnv)
  assign('loopvar',loopvar,envir=.GlobalEnv)
  assign('x',x,envir=.GlobalEnv)
  assign('y',y,envir=.GlobalEnv)
  assign('Plot_Sort_Order',Plot_Sort_Order,envir=.GlobalEnv)
  assign('Orientation',Orientation,envir=.GlobalEnv)
  assign('OutputURL',OutputURL,envir=.GlobalEnv)
  assign('pdf_plot_title',pdf_plot_title,envir=.GlobalEnv)
  assign('plot_title',plot_title,envir=.GlobalEnv)
  assign('subtitle',subtitle,envir=.GlobalEnv)
  assign('ResponseLabel',ResponseLabel,envir=.GlobalEnv)
  
  pdfloopvar <-c()
  pdfdefaultSort=c('country','crop','pestid')
  pdfloopvar=prisort(pdfdefaultSort,pdfloopvar)  
  
  if(plot_type=="Bar Chart") {
    pdfname='Barchart' 
    title2 <- 'Graphical Summary - Bar Chart'
  } else {
    pdfname='Boxplot'
    title2 <- 'Graphical Summary - Box Plot'
  }

  pdfdatafilter=data
    
  OutputURLlast=substr(OutputURL, nchar(OutputURL), nchar(OutputURL))
  separator=ifelse(OutputURLlast!='/','/','')
  file.URL=paste0(OutputURL,separator,pdfname,format(Sys.time(), format='%Y%m%d%H%M%S'),'.pdf')
  pdf(onefile=TRUE,file=file.URL,paper="USr",width=pdfwidth,height=pdfheight)
    
  plot.new()
    
  title1index=unique(c(pdfloopvar))
  title1index=prioritylist( pdfdefaultSort, title1index) 
   
  if (length(unique(pdfdatafilter[,title1index]))==1) { title1=unique(pdfdatafilter[,title1index])[1]
  } else{
    title1=paste(unique(pdfdatafilter[,title1index])[1,],collapse = '-')
  }
  assign('subtitle',subtitle,envir=.GlobalEnv)
    
  if (!nchar(sub('\\s+','',subtitle))){
    grid.text(pdf_plot_title,gp=gpar(cex=3,font=1),y=0.7,x=0.5,just='center')
    grid.text(title1,gp=gpar(cex=1.6,font=1),y=0.55,x=0.5,just='center')
    grid.text(title2,gp=gpar(cex=1.6,font=1),y=0.45,x=0.5,just='center')
  }else{      
    grid.text(pdf_plot_title,gp=gpar(cex=3,font=1),y=0.75,x=0.5,just='center')
    grid.text(subtitle,gp=gpar(cex=1.6,font=1),y=0.65,x=0.5,just='center')
    grid.text(title1,gp=gpar(cex=1.6,font=1),y=0.55,x=0.5,just='center')
    grid.text(title2,gp=gpar(cex=1.6,font=1),y=0.45,x=0.5,just='center')      
  } 
  add_footer()     
  BoxplotBarchart(data=pdfdatafilter,loopvar=loopvar,x=x,y=y,Plot_Sort_Order=Plot_Sort_Order,single_grouped=single_grouped, grouped_var=grouped_var,
             plot_type=plot_type, outlier=outlier, ErrorBarType=ErrorBarType, Statistics=Statistics, colorby=colorby, Orientation=Orientation, 
             plot_title=plot_title, trt_label_font=trt_label_font, ResponseLabel=ResponseLabel)
      
  pdfid=dev.list()[names(dev.list())=='pdf']
  lapply(pdfid,function(x) {dev.off(which=x)})    
  return(list(url=file.URL,pdfname=pdfname))
}

############### plotfunction.R

BoxplotBarchart=function(data,loopvar,x,y, Plot_Sort_Order, single_grouped, grouped_var,
                         plot_type, outlier, ErrorBarType, Statistics, colorby, 
                         Orientation,plot_title, trt_label_font, ResponseLabel)
{ 
assign("data", data ,envir = .GlobalEnv)
assign("loopvar", loopvar ,envir = .GlobalEnv)
assign("Plot_Sort_Order", Plot_Sort_Order ,envir = .GlobalEnv)
assign("single_grouped", single_grouped ,envir = .GlobalEnv)
assign("grouped_var", grouped_var ,envir = .GlobalEnv)
assign("x", x ,envir = .GlobalEnv)
assign("plot_type", plot_type ,envir = .GlobalEnv)
assign("y", y ,envir = .GlobalEnv)
assign("colorby", colorby ,envir = .GlobalEnv)
assign("Orientation", Orientation ,envir = .GlobalEnv)
assign("plot_title", plot_title ,envir = .GlobalEnv)
  defaultSort=c('year','region','country','crop','pestid','SMBP','symptom','method','basis','part','trial_NO_')
  loopvar=prioritylist(defaultSort,loopvar)  

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
    datafilter=datafilter[!is.na(datafilter[,y]),]
    datafilter=datafilter[!is.na(datafilter[,x]),]
    #uniform variable name
    datafilter$x=as.character(datafilter[,x])
    datafilter$y=as.numeric(as.character(datafilter[,y]))

    assign("datafilter", datafilter ,envir = .GlobalEnv)
    
    datafilter$y=as.numeric(datafilter[,'y'])  
    
    Trt<-as.factor(datafilter$x)
    color<-as.factor(datafilter[,colorby])
    Response<-datafilter$y
    
    if (single_grouped=='Grouped') {
      GroupedVar<-as.factor(datafilter[,grouped_var])
      newdata<-data.frame(GroupedVar, Trt, Response,color)
      df <- summarySE(datafilter, measurevar="y", groupvars=c(grouped_var,x))
      df$Trt<-df[,x]
      df$GroupedVar<-df[,grouped_var]
      newdata <- sqldf("select A.*,B.se,B.ci,B.y as trtmean from newdata A, df B where A.Trt=B.Trt and A.GroupedVar=B.GroupedVar")
    } else {
      newdata<-data.frame(Trt, Response,color)
      df <- summarySE(datafilter, measurevar="y", groupvars=c(x))
      df$Trt<-df[,x]
      newdata <- sqldf("select A.*,B.se,B.ci,B.y as trtmean from newdata A, df B where A.Trt=B.Trt")
    }
    newdata$Trt <- as.character(newdata$Trt)
    newdata$colors <- ""
    if(dim(newdata)[1]!=0){
      TrtLevel <- levels(Trt)
      assign("newdata", newdata ,envir = .GlobalEnv)
      #define theme for plot
      themeplot=theme(axis.text=element_text(size=10, face="bold"),                                                    
	       axis.title=element_text(size=10,face="bold"),
	       legend.text=element_text(size=8,face="bold"),
	       legend.title=element_text(size=10,face="bold"),
	       strip.text=element_text(size=10,face="bold"),
	       plot.background = element_rect(fill = 'white', colour = 'white'),
	       panel.background = element_rect(fill = 'white', colour = 'grey', size=2),
	       panel.grid.major = element_line(colour = "white", size = 0.1, linetype = "dotted"),
	       panel.grid.minor = element_blank(),
	       plot.title = element_text(size = 10, face="bold"),
	       legend.position="bottom", legend.box = "horizontal",
	       plot.margin = unit(c(3,2,1,1), "cm")) +
	       theme(legend.position="none")
      tot_len <- 0  
      for (i in 1:length(TrtLevel)) {
        tot_len <- tot_len + str_length(TrtLevel[i])
      }
      #try(levels(newdata$Trt) <- sapply(levels(newdata$Trt),"break_str",32))
      if(!is.null(Plot_Sort_Order) ) {
        newdata$Trt <- factor(newdata$Trt, Plot_Sort_Order) 
      }
      if (tot_len >600) {
        sub_len <- 0
        for (i in 1:length(TrtLevel)) {
          sub_len <- sub_len + str_length(TrtLevel[i])
          if (sub_len>tot_len/2) break
        }
        newdata1 <- newdata[newdata$Trt %in% TrtLevel[1:i],]
        newdata2 <- newdata[!newdata$Trt %in% TrtLevel[1:i],]
        plot1 <- ggplot(aes(x=sapply(newdata1$Trt,break_str,32),y=newdata1$Response),data=newdata1) 
        plot2 <- ggplot(aes(x=sapply(newdata2$Trt,break_str,32),y=newdata2$Response),data=newdata2)  
        plot_title2index=unique(c(loopvar))
        plot_title2index=prioritylist(defaultSort,plot_title2index)  
        if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])} 
        else {plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')} 
        plot_title12 <- paste(plot_title2,'(1 of 2)')
        plot_title22 <- paste(plot_title2,'(2 of 2)')
        if(plot_type=="Box Plot") {
          #Box Plot  
          plot1 <- plot1 + geom_boxplot(aes(fill=Trt)) +
                   stat_summary(fun.y="mean", geom="point", size=3, position=position_dodge(width=0.75),color="white")
          if (!outlier) plot1 <- plot1 + 
                                stat_summary(fun.y = min, colour = trt_colors(newdata), geom = "point", size = 3) +
                                stat_summary(fun.y = max, colour = trt_colors(newdata), geom = "point", size = 3) 
        } else {
          #Bar Chart
          plot1 <- plot1 + geom_bar(aes(fill=Trt),stat = "summary", fun.y = Statistics,width=0.6) 
        }
        plot1 <- plot1 +
	           scale_fill_manual(name="Treatments Type",values=colors) +
	           themeplot + 
	           labs(x=' ', y = ResponseLabel)	           
        if (Orientation=='Horizontal') {
          plot1 <- plot1 + 
                   coord_flip() + 
                   theme(axis.text.y = element_text(size=trt_label_font))
        } else {
          plot1 <- plot1 + 
                   theme(axis.text.x = element_text(angle = 45, hjust = 1,size=trt_label_font)) 
        }
        print(plot1)  
        title(main=plot_title,xpd=T,line=2,font.main= 6)
        plot_title2index=unique(c(loopvar))
        plot_title2index=prioritylist(defaultSort,plot_title2index)  
        if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])} 
        else {plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')} 
        op <- par()
        par(font.main=1,cex.main=1)
        title(main=plot_title12,xpd=NA,line=1)   
        if(plot_type=="Box Plot") {
          #Box Plot  
          plot2 <- plot2 + geom_boxplot(aes(fill=Trt)) +
                   stat_summary(fun.y="mean", geom="point", size=3, position=position_dodge(width=0.75),color="white")
          if (!outlier) plot2 <- plot2 + 
                                stat_summary(fun.y = min, colour = trt_colors(newdata), geom = "point", size = 3) +
                                stat_summary(fun.y = max, colour = trt_colors(newdata), geom = "point", size = 3) 
        } else {
          #Bar Chart
          plot2 <- plot2 + geom_bar(aes(fill=Trt),stat = "summary", fun.y = Statistics,width=0.6) 
        }
        plot2 <- plot2 +
	           scale_fill_manual(name="Treatments Type",values=colors) +
                   labs(x=' ', y = ResponseLabel)+
	           themeplot 
        if (Orientation=='Horizontal') {
          plot2 <- plot2 + 
                   coord_flip() + 
                   theme(axis.text.y = element_text(size=trt_label_font))
        } else {
          plot2 <- plot1 + 
                   theme(axis.text.x = element_text(angle = 45, hjust = 1,size=trt_label_font))
        }
        print(plot2)  
        par(op)
        title(main=plot_title,xpd=T,line=2,font.main= 6)
        plot_title2index=unique(c(loopvar))
        plot_title2index=prioritylist(defaultSort,plot_title2index)  
        if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])} 
        else {plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')} 
        par(font.main=1,cex.main=1)
        title(main=plot_title22,xpd=NA,line=1)  
      } else {     
          plot <- ggplot(aes(x=Trt,y=Response),data=newdata )
        if(plot_type=="Box Plot") {
          #Box Plot  
          plot <- plot + geom_boxplot(aes(fill=Trt)) +
                   stat_summary(fun.y="mean", geom="point", size=3, position=position_dodge(width=0.75),color="white")
          if (!outlier) plot <- plot + 
                                stat_summary(fun.y = min, colour = trt_colors(newdata), geom = "point", size = 3) +
                                stat_summary(fun.y = max, colour = trt_colors(newdata), geom = "point", size = 3) 
        } else {
          #Bar Chart  
          plot <- plot + geom_bar(aes(fill=Trt),position="dodge",stat = "summary", fun.y = Statistics,width=0.6,show.legend = TRUE) 
          if (ErrorBarType=='Confidence Interval') errbar=newdata$ci
          else errbar=newdata$se
          plot <- plot + geom_errorbar(position="dodge", width=.25, aes(ymin=trtmean-errbar, ymax=trtmean+errbar)) 
        }   
        plot <- plot +     
	        #scale_fill_manual(name="Treatments Type", labels = colorbyLevel, values=colors, limits = colorbyLevel) +
	        #scale_colour_manual(values = c(colorbyLevel[1] = "red", colorbyLevel[2] = "blue") +
	        scale_fill_manual(values=trt_colors(newdata)) +
	        labs(x=' ', y = ResponseLabel)+
	        themeplot 
        if (Orientation=='Horizontal') {
          plot <- plot + 
                  coord_flip() + 
                  theme(axis.text.y = element_text(size=trt_label_font))
        } else {
          plot <- plot + 
                  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=trt_label_font))
        }
        if (single_grouped=='Grouped') plot <- plot + facet_wrap(~GroupedVar)
        print(plot)  
        title(main=plot_title,xpd=T,line=2)
        plot_title2index=unique(c(loopvar))
        plot_title2index=prioritylist(defaultSort,plot_title2index)  
        if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])} 
        else {plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')} 
        par(font.main=1,cex.main=1)
        title(main=plot_title2,xpd=NA,line=1)  
      }
    } else if(dim(newdata)[1]==0) {
      plot.new()
      grid.text("No Data",gp=gpar(cex=1.6,font=1),y=0.55,x=0.5,just='center')
      title(main=plot_title,xpd=T,line=2)

      plot_title2index=unique(c(loopvar))
      plot_title2index=prioritylist(defaultSort,plot_title2index)  
      if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])}  
      else {
        plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')
      }
      par(font.main=1,cex.main=1)
      title(main=plot_title2,xpd=NA,line=1)
    }
    add_footer()
  }
}