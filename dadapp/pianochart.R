############### pdfplotfunction.R

piano.pdfplot=function(data,pdfloopvar,loopvar,x,y,trial,OutputURL,trt1,trt2,
                 pdf_plot_title,plot_title,subtitle,DataDate,det){
  
  
  #pdfset#
  pdfheight=8
  pdfwidth=10.5
  
  pdfdefaultSort=c('country','crop','pestid')
  pdfloopvar=prisort(pdfdefaultSort,pdfloopvar)  
  
  if (length(pdfloopvar)==0){
    pdfloopflag=FALSE
    pdfnum=1
    pdfdatafilter=data
    pdfname='Piano'
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
      pdfname=paste(c('Piano Chart',pdfhead),collapse='-')
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
    
    if (!nchar(sub('\\s+','',subtitle))){
      grid.text(pdf_plot_title,gp=gpar(cex=3,font=1),y=0.7,x=0.5,just='center')
      grid.text(title1,gp=gpar(cex=1.6,font=1),y=0.55,x=0.5,just='center')
      grid.text('Graphical Summary-Piano Chart',gp=gpar(cex=1.6,font=1),y=0.45,x=0.5,just='center')
    }else{
      
      grid.text(pdf_plot_title,gp=gpar(cex=3,font=1),y=0.75,x=0.5,just='center')
      grid.text(subtitle,gp=gpar(cex=1.6,font=1),y=0.65,x=0.5,just='center')
      grid.text(title1,gp=gpar(cex=1.6,font=1),y=0.55,x=0.5,just='center')
      grid.text('Graphical Summary-Piano Chart',gp=gpar(cex=1.6,font=1),y=0.45,x=0.5,just='center')
      
    }
    add_footer()
    pianochart(data=pdfdatafilter,loopvar=loopvar,x=x,y=y,trial=trial,trt1,trt2,
                 plot_title=plot_title,det=det)
    pdfid=dev.list()[names(dev.list())=='pdf']
    lapply(pdfid,function(x) {dev.off(which=x)})
    
    
  }
  return(list(url=file.URL,pdfname=pdfname))
}
############### plotfunction.R

pianochart=function(data,loopvar,x,y,trial,trt1,trt2,plot_title,det)
{
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
    datafilter$trial=datafilter[,trial]
    # datafilter$rep=datafilter[,rep]

    datafilter$y=as.numeric(datafilter[,'y'])

    Trt<-as.factor(datafilter$x)
    Response<-datafilter$y
    Trial<-as.factor(datafilter$trial)
    # Rep<-as.factor(datafilter$rep)
    
    assign("Trt", Trt ,envir = .GlobalEnv)
    assign("Response", Response ,envir = .GlobalEnv)
    assign("Trial", Trial ,envir = .GlobalEnv)
    
    newdata<-data.frame(Trial, Trt, Response)
    newdata$Trt <- as.character(newdata$Trt)
    newdata_sub <- subset(newdata,Trt==trt1|Trt==trt2)

    #create mean table by treatment#
    if(dim(newdata_sub)[1]!=0){
      newdata_sub <- aggregate(Response~Trial+Trt,newdata_sub,mean)
      newdata_sub1 <- subset(newdata_sub,Trt==trt1)
      newdata_sub2 <- subset(newdata_sub,Trt==trt2)
      
      plottable<-merge(newdata_sub1,newdata_sub2,by='Trial')
      mean_response_diff <- plottable$Response.x - plottable$Response.y
      plottable <- cbind(plottable,mean_response_diff)
      plottable <- plottable[order(plottable$mean_response_diff),]
      # colnames(plottable) <- c("Trial", "MeanResponse")
      plottable$mean_response_diff <- round(plottable$mean_response_diff, digits=2)
      
      if (length(unique(plottable$Trial))==1) {
        plot.new()
        grid.text("Only One Trial Found",gp=gpar(cex=1.6,font=1),y=0.55,x=0.5,just='center')
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
         else if (length(unique(plottable$Trial)!=1)){
           assign('plottable',plottable,envir=.GlobalEnv)

          # cat(colnames(plottable))
          # plot data
          #plottitle = paste('Diff in Avg Resp of', paste(as.matrix(head),collapse = "-"), sep = " ") 
          subtitle= paste(trt1,'vs.',trt2, sep=" ")
          Ylabel = paste(trt1, '-', trt2, sep =" ")
          avgDiff = mean(plottable$Response.x) - mean(plottable$Response.y)
          avgDiff =round(avgDiff,2)
          positionX = nrow(plottable)-1
          positionY = avgDiff + 0.05*(max(plottable$mean_response_diff)-min(plottable$mean_response_diff))
          AvgDiffText = paste('AvgDiff', '=', avgDiff, sep=" ")
          countNegative =  sum(plottable$mean_response_diff <0)/nrow(plottable)
          countEqual = sum(plottable$mean_response_diff ==0)/nrow(plottable)
          assign('countEqual',countEqual,envir=.GlobalEnv)
          
          countPositive =  1 - countNegative - countEqual
          assign('countPositive',countPositive,envir=.GlobalEnv)
          
          countNegative = paste(round(100*countNegative, 0), "%", sep="")
          assign('countNegative',countNegative,envir=.GlobalEnv)
          
          countPositive = paste(round(100*countPositive, 0), "%", sep="")
          countEqual = paste(round(100*countEqual, 0), "%", sep="")
          
          if (det=='Higher') {
            textNegative= paste(countNegative, 'of time', trt2, 'was better')
            textPositive = paste(countPositive, 'of time', trt1, 'was better')
          } else {
            textNegative= paste(countNegative, 'of time', trt1, 'was better')
            textPositive = paste(countPositive, 'of time', trt2, 'was better')
          }
          textEqual = paste(countEqual, 'of time', trt1, "and", trt2, 'were the same')
          
          plottable$colour <- NA
          plottable$colour[plottable$mean_response_diff > 0] <- "positive"
          plottable$colour[plottable$mean_response_diff < 0] <- "negative"
          plottable$colour[plottable$mean_response_diff == 0] <- "equal"

          plottable$hjust <- ifelse(plottable$mean_response_diff < 0, -0.3,1.3)
          
          assign('plottable',plottable,envir=.GlobalEnv)
          
          cat(plottable$mean_response_diff,'\n')
          cat(plottable$colour,'\n')
          
          themeplot=theme(axis.text=element_text(size=10, face="bold"),                                                    #define theme for plot
                          axis.title=element_text(size=13,face="bold"),
                          legend.text=element_text(size=12),
                          legend.title=element_text(size=10),
                          strip.text=element_text(size=14,face="bold"),
                          plot.background = element_rect(fill = 'white', colour = 'white'),
                          panel.background = element_rect(fill = 'white', colour = 'grey', size=2),
                          panel.grid.major = element_line(colour = "white", size = 0.1, linetype = "dotted"),
                          panel.grid.minor = element_blank(),
                          legend.margin = unit(0.2, "cm"),
                          plot.margin = unit(c(2,1,1,1), "cm"),
                          plot.title = element_text(size = 12, face="bold"),
                          #legend.position="top", legend.box = "vertical",
                          legend.key.size = unit(0.5, "cm"),
                          legend.justification = "center",
                          legend.position = c(0.5, 0.89),legend.box = "vertical", 
                          axis.text.x = element_text(size=13,angle = 45, hjust =1)) #legend position X Y needs to be changed according to text length, X, Y scale
          
          mini <- ifelse(any(plottable$mean_response_diff<0),min(plottable$mean_response_diff)*1.5,0)
          cat('mini:',mini)
          
          cat('DET:',det)
          assign('det',det,envir=.GlobalEnv)
          
          positive_color <- ifelse(det=='Higher',"green",'red')
          negative_color <- ifelse(det=='Higher',"red",'green')
          
          if(any(plottable$colour %in% "equal")){
               piano <- ggplot(plottable,aes(Trial,mean_response_diff,label="",hjust=hjust),colour="black")+
                          geom_bar(stat="identity",position="identity",aes(fill = colour),colour="black")+
                          scale_fill_manual(values=c(positive=positive_color,negative=negative_color,equal="white"), labels = c(equal=textEqual,negative=textNegative,positive=textPositive)) +
                          aes(x=reorder(Trial,mean_response_diff,sum),y=mean_response_diff) +
                          #scale_y_continuous(limits=c(min(plottable$mean_response_diff),(max(plottable$mean_response_diff)*10)))+
                          coord_cartesian(ylim = c(mini,(max(plottable$mean_response_diff)*1.5+10)))+
                          themeplot + 
                          labs(x =" ", y = Ylabel) +
                          ggtitle(paste0(subtitle,"\n")) +
                          geom_hline(yintercept=avgDiff, linetype = 'dotdash') + 
                          geom_hline(yintercept=0) + 
                          annotate("text", x = positionX, y = positionY, label = AvgDiffText, size = 4.5, fontface="bold") +
                          labs(fill="")
          
               print(piano)
               
               title(main=plot_title,xpd=T,line=2)

               plot_title2index=unique(c(loopvar))
               plot_title2index=prioritylist(defaultSort,plot_title2index)  
               if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])} 
               else {plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')}

               par(font.main=1,cex.main=1)
               title(main=plot_title2,xpd=NA,line=1)
          }
          
          else if(!any(plottable$colour %in% "equal")){
            piano <- ggplot(plottable,aes(Trial,mean_response_diff,label="",hjust=hjust),colour="black")+
              geom_bar(stat="identity",position="identity",aes(fill = colour),colour="black")+
              scale_fill_manual(values=c(positive=positive_color,negative=negative_color), labels = c(positive=textPositive,negative=textNegative)) +
              aes(x=reorder(Trial,mean_response_diff,sum),y=mean_response_diff) +
              coord_cartesian(ylim = c(mini,(max(plottable$mean_response_diff)*1.5+10)))+
              themeplot + 
              labs(x =" ", y = Ylabel) +
              ggtitle(paste0(subtitle,"\n")) +
              geom_hline(yintercept=avgDiff, linetype = 'dotdash') + 
              geom_hline(yintercept=0) + 
              annotate("text", x = positionX, y = positionY, label = AvgDiffText, size = 4.5, fontface="bold") +
              labs(fill="")
            
            print(piano)
            
            title(main=plot_title,xpd=T,line=2)
 
            plot_title2index=unique(c(loopvar))
            plot_title2index=prioritylist(defaultSort,plot_title2index)  
            if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])} 
            else {plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')}
            
            par(font.main=1,cex.main=1)
            title(main=plot_title2,xpd=NA,line=1)
          }
         }  
    } else if(dim(newdata_sub)[1]==0) {
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
    add_footer(footer1=paste0(nlevels(factor(plottable$Trial))," trials compared")) 
  }
}