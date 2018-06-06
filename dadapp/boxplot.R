############### pdfplotfunction.R
box.pdfplot=function(data,pdfloopvar,loopvar,x,y,Orientation, OutputURL,
                 pdf_plot_title,plot_title,subtitle,Y_label){ 
  #pdfset#
  pdfheight=8
  pdfwidth=10.5
  
  pdfdefaultSort=c('country','crop','pestid')
  pdfloopvar=prisort(pdfdefaultSort,pdfloopvar)  
  
  if (length(pdfloopvar)==0){
    pdfloopflag=FALSE
    pdfnum=1
    pdfdatafilter=data
    pdfname='Boxplot'
  } else {
    pdfloopflag=TRUE
    pdffilter=sqldf(paste("select distinct", paste(pdfloopvar,collapse=','),'from data' ))
    pdfnum=dim(pdffilter)[1]
  }

  for (pdfi in 1:pdfnum){
      if (pdfloopflag){  
      pdfhead=pdffilter[pdfi,]
      if (length(pdfloopvar)>1) {
        pdfdatafilter=sqldf(paste("select *",'from data natural inner join pdfhead' ))
      }else{
        pdfdatafilter=data[data[pdfloopvar]==pdfhead,]
      }
      pdfname=paste(c('Boxplot',pdfhead),collapse='-')
      pdfname=chartr('%','pct',pdfname)
    }
    
    OUtputURLlast=substr(OutputURL, nchar(OutputURL), nchar(OutputURL))
    separator=ifelse(OUtputURLlast!='\\','\\','')
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
      grid.text('Graphical Summary-Boxplot',gp=gpar(cex=1.6,font=1),y=0.45,x=0.5,just='center')
    }else{      
      grid.text(pdf_plot_title,gp=gpar(cex=3,font=1),y=0.75,x=0.5,just='center')
      grid.text(subtitle,gp=gpar(cex=1.6,font=1),y=0.65,x=0.5,just='center')
      grid.text(title1,gp=gpar(cex=1.6,font=1),y=0.55,x=0.5,just='center')
      grid.text('Graphical Summary-Boxplot',gp=gpar(cex=1.6,font=1),y=0.45,x=0.5,just='center')      
    } 
    add_footer()     
    Boxplot(data=pdfdatafilter,loopvar=loopvar,x=x,y=y,Orientation=Orientation,   
               plot_title=plot_title, Y_label=Y_label)
        
    pdfid=dev.list()[names(dev.list())=='pdf']
    lapply(pdfid,function(x) {dev.off(which=x)})    
  }
  return(list(url=file.URL,pdfname=pdfname))
}

############### plotfunction.R

Boxplot=function(data,loopvar,x,y,Orientation,plot_title, Y_label)
{ 
assign("data", data ,envir = .GlobalEnv)
assign("loopvar", loopvar ,envir = .GlobalEnv)
assign("x", x ,envir = .GlobalEnv)
assign("y", y ,envir = .GlobalEnv)
assign("Orientation", Orientation ,envir = .GlobalEnv)
assign("plot_title", plot_title ,envir = .GlobalEnv)
  defaultSort=c('year','region','country','crop','pestid','SMBP','symptom','method','basis','part','trial_NO_')
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
    datafilter=datafilter[!is.na(datafilter[,y]),]
    datafilter=datafilter[!is.na(datafilter[,x]),]
    #uniform variable name
    datafilter$x=as.character(datafilter[,x])
    datafilter$y=as.numeric(as.character(datafilter[,y]))
    #datafilter$trial=datafilter[,trial]
    # datafilter$rep=datafilter[,rep]
    
    datafilter$y=as.numeric(datafilter[,'y'])  
    
    Trt<-as.factor(datafilter$x)
    Response<-datafilter$y
    Type<-as.factor(datafilter$Trt_Type)
    
    newdata<-data.frame(Trt, Response)
    newdata$Trt <- as.character(newdata$Trt)
    newdata$color <- 'Treatments'
    # newdata$color[Type=="Stdrd"] <- "green"
    newdata$color[Type=="untrt"] <- "Untreated"
    assign("newdata", newdata ,envir = .GlobalEnv)
    
    #create mean table by treatment#
    if(dim(newdata)[1]!=0){
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
	       plot.margin = unit(c(3,2,1,1), "cm"))

      tot_len <- 0
      TrtLevel <- levels(Trt)
      for (i in 1:length(TrtLevel)) {
        tot_len <- tot_len + str_length(TrtLevel[i])
      }
      if (tot_len >600) {
        sub_len <- 0
        TrtLevel <- levels(Trt)
        for (i in 1:length(TrtLevel)) {
          sub_len <- sub_len + str_length(TrtLevel[i])
          if (sub_len>tot_len/2) break
        }
        newdata1 <- newdata[newdata$Trt %in% TrtLevel[1:i],]
        newdata2 <- newdata[!newdata$Trt %in% TrtLevel[1:i],]
        Boxplot1 <- ggplot(aes(x=sapply(newdata1$Trt,break_str,32),y=newdata1$Response),data=newdata1) 
        Boxplot2 <- ggplot(aes(x=sapply(newdata2$Trt,break_str,32),y=newdata2$Response),data=newdata2)  
        plot_title2index=unique(c(loopvar))
        plot_title2index=prioritylist(defaultSort,plot_title2index)  
        if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])} 
        else {plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')} 
        plot_title12 <- paste(plot_title2,'(1 of 2)')
        plot_title22 <- paste(plot_title2,'(2 of 2)')
        Boxplot1 <- Boxplot1 +
                   geom_boxplot(aes(fill=color)) +
	           stat_summary(fun.y=mean, colour="darkred", geom="point",shape=18, size=3,show_guide = FALSE)+
	           #geom_text(data = means, aes(label = round(Response,2), y = Response + 2),size=2.5)+
	           scale_fill_manual(name="Treatments Type",values=c(Treatments='orange',Untreated="gray")) +
	           themeplot + 
	           labs(x=' ', y = Y_label)	           
        if (Orientation=='Horizontal') {
          Boxplot1 <- Boxplot1 + 
                      coord_flip() 
        } else {
          Boxplot1 <- Boxplot1 + 
                      theme(axis.text.x = element_text(angle = 90, hjust = 1,size=10)) 
        }
        print(Boxplot1)  
        title(main=plot_title,xpd=T,line=2,font.main= 6)
        plot_title2index=unique(c(loopvar))
        plot_title2index=prioritylist(defaultSort,plot_title2index)  
        if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])} 
        else {plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')} 
        op <- par()
        par(font.main=1,cex.main=1)
        title(main=plot_title12,xpd=NA,line=1)   
        Boxplot2 <- Boxplot2 +
                   geom_boxplot(aes(fill=color)) +
	           stat_summary(fun.y=mean, colour="darkred", geom="point",shape=18, size=3,show_guide = FALSE)+
	           #geom_text(data = means, aes(label = round(Response,2), y = Response + 2),size=2.5)+
	           scale_fill_manual(name="Treatments Type",values=c(Treatments='orange',Untreated="gray")) +
	           xlab(' ') +
	           ylab('Response') +
	           themeplot 
        if (Orientation=='Horizontal') {
          Boxplot2 <- Boxplot2 + 
                     coord_flip() 
        } else {
          Boxplot2 <- Boxplot1 + 
                     theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8))
        }
        print(Boxplot2)  
        par(op)
        title(main=plot_title,xpd=T,line=2,font.main= 6)
        plot_title2index=unique(c(loopvar))
        plot_title2index=prioritylist(defaultSort,plot_title2index)  
        if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])} 
        else {plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')} 
        par(font.main=1,cex.main=1)
        title(main=plot_title22,xpd=NA,line=1)  
      } else {
        Boxplot <- ggplot(aes(x=break_str(newdata$Trt,32),y=Response),data=newdata)
        Boxplot <- Boxplot +
                   geom_boxplot(aes(fill=color)) +
	           stat_summary(fun.y=mean, colour="darkred", geom="point",shape=18, size=3,show_guide = FALSE)+
	           #geom_text(data = means, aes(label = round(Response,2), y = Response + 2),size=2.5)+
	           scale_fill_manual(name="Treatments Type",values=c(Treatments='orange',Untreated="gray")) +
	           xlab(' ') +
	           ylab('Response') +
	           themeplot 
        if (Orientation=='Horizontal') {
          Boxplot <- Boxplot + 
                     coord_flip() 
        } else {
          Boxplot <- Boxplot + 
                     theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8))
        }
        print(Boxplot)  
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