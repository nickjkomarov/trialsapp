# pdfplotfunction.R
tworesponse.pdfplot=function(data,pdfloopvar,loopvar,y,DAF,sep,se1,se2,OutputURL,
                 X_label,Y_label1,Y_label2,pdf_plot_title,plot_title,subtitle){
  #pdfset#
  pdfheight=8
  pdfwidth=8
  
  pdfdefaultSort=c('country')
  pdfloopvar=prisort(pdfdefaultSort,pdfloopvar) 
  
  if (length(pdfloopvar)==0){
    pdfloopflag=FALSE
    pdfnum=1
    pdfdatafilter=data
    pdfname='Two-ResponsePlot'
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
      pdfname=paste(c('Two-ResponsePlot',pdfhead),collapse='-')
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
      grid.text('Graphical Summary-Two-Response Plot',gp=gpar(cex=1.6,font=1),y=0.45,x=0.5,just='center')
    }else{
      grid.text(pdf_plot_title,gp=gpar(cex=3,font=1),y=0.75,x=0.5,just='center')
      grid.text(subtitle,gp=gpar(cex=1.6,font=1),y=0.65,x=0.5,just='center')
      grid.text(title1,gp=gpar(cex=1.6,font=1),y=0.55,x=0.5,just='center')
      grid.text('Graphical Summary-Two-Response Plot',gp=gpar(cex=1.6,font=1),y=0.45,x=0.5,just='center')
    }
    add_footer()
    tworesponse(data=pdfdatafilter,loopvar=loopvar,y=y,DAF=DAF,sep=sep,se1=se1,se2=se2,
                 X_label=X_label,Y_label1=Y_label1,Y_label2=Y_label2,plot_title=plot_title)

    pdfid=dev.list()[names(dev.list())=='pdf']
    lapply(pdfid,function(x) {dev.off(which=x)})
    #if (pdfi<=4){
    #}
  }
  return(list(url=file.URL,pdfname=pdfname))
}

#plotfunction.R

tworesponse=function(data,loopvar,y,DAF,sep,se1,se2,X_label,Y_label1,Y_label2,plot_title)
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
    datafilter$Eval_Avg=datafilter[,y]
    datafilter$DAF=datafilter[,DAF]
    datafilter$sep=datafilter[,sep]

    datafilter$y=as.numeric(datafilter[,'Eval_Avg'])
    
    #subset column names
    data.f=datafilter[,c("Eval_Avg", "DAF", "sep")]
    data.fbs=data.frame(cbind(data.f$Eval_Avg,data.f$DAF,data.f$sep))
    colnames(data.fbs)<-c("Eval_Avg","DAF","Symptom")
    #select one treatment for plotting
    
    #splilt dataset into two, not really work for this case
    #data.yield=split(data.fbs, data.fbs$Symptom)
    #data.yield
    
    data.fbs$DAF<-as.numeric(as.character(data.fbs$DAF))
    data.fbs$Eval_Avg<-as.numeric(as.character(data.fbs$Eval_Avg))
    
    #create two new columns by splitting the Symptom
    
    data.fbs$Yield=ifelse(data.fbs$Symptom==se1, data.fbs$Eval_Avg, NA)
    #test
    #data.fbs[c(1520:1540),]
    data.fbs$Defoli=ifelse(data.fbs$Symptom==se2, data.fbs$Eval_Avg, NA)
    #data.fbs[c(1:10),]
    #start plotting

    if (    (length(unique(data.fbs[!is.na(data.fbs$Yield),]$DAF))==1) | (length(unique(data.fbs[!is.na(data.fbs$Defoli),]$DAF))==1)   ){
      plot.new()
      grid.text("Only 1 DAF in a certain separation for this loop \nis found.",gp=gpar(cex=1.3,font=1),y=0.55,x=0.5,just='center')
      title(main=plot_title,xpd=T,line=3)
      
      plot_title2index=unique(c(loopvar))
      plot_title2index=prioritylist(defaultSort,plot_title2index)  
      if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])
      } else{
        plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')
      }
      par(font.main=1,cex.main=1)
      title(main=plot_title2,xpd=NA,line=1)
    }  else if (  (all(is.na(unique(data.fbs$Yield)))) |  all((is.na(unique(data.fbs$Defoli))))  )  {
      plot.new()
      par(mar=c(5.7,4,4,4)+2)
      grid.text("No separation for this loop.",gp=gpar(cex=1.6,font=1),y=0.55,x=0.5,just='center')
      title(main=plot_title,xpd=T,line=3)
      
      plot_title2index=unique(c(loopvar))
      plot_title2index=prioritylist(defaultSort,plot_title2index)  
      if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])
      } else{
        plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')
      }
      par(font.main=1,cex.main=1)
      title(main=plot_title2,xpd=NA,line=1)  
    }   else{
    #change number of set number to change color themes
    colorscale = brewer.pal(5, "Set1")
    #display.brewer.pal(3,"Set2")
    
    par(mar=c(5.7,4,1,4)+2)
    
    #max and min for each column
    apply(data.fbs, MARGIN = 2, function(x) max(x, na.rm=TRUE))
    apply(data.fbs, MARGIN = 2, function(x) min(x, na.rm=TRUE))
    
    #calculate average by group
    avg.yield=aggregate(data.fbs$Yield, list(DAF=data.fbs$DAF), FUN=mean, na.action = na.pass, na.rm=TRUE) 
    avg.Defoli=aggregate(data.fbs$Defoli, list(DAF=data.fbs$DAF), FUN=mean, na.action = na.pass, na.rm=TRUE)
    
    #merge average and create new table
    avg=merge(avg.yield, avg.Defoli, by="DAF")
    names(avg)[2]<-"Yield"
    names(avg)[3]<-"Defoli"
    avg.yield<-data.frame(avg.yield)
    avg.Defoli<-data.frame(avg.Defoli)
    avg.Defoli$DAF<-as.numeric(as.character(avg.Defoli$DAF))
    avg.yield$DAF<-as.numeric(as.character(avg.yield$DAF))
    avg.Defoli<-avg.Defoli[order(as.numeric(avg.Defoli$DAF)),]
    avg.yield<-avg.yield[order(as.numeric(avg.yield$DAF)),]
    
    rownames(avg.yield)<-NULL
    rownames(avg.Defoli)<-NULL

    min1=min(avg.yield$x[!is.na(avg.yield$x)])
    max1=max(avg.yield$x[!is.na(avg.yield$x)])
    ymin<-0
    ymax<-max1
    ticknum=15
    ygap=(ymax-ymin)/ticknum
    digitnum=floor(log(ygap)/log(10))
    ygap1=ygap/(10^digitnum)
    ygap2=ceiling(ygap1/5)*5*10^digitnum
    ymin=floor(ymin/ygap2)*ygap2
    ymax=ceiling(ymax/ygap2)*ygap2
    ymin=0
    yaxisrange=c(ymin,ymax)
    yaxis=seq(from=ymin,to=ymax,by=ygap2)
    ylim<-yaxisrange
    ticknum=(ymax-ymin)/ygap2+1
 
    min1_2=min(avg.Defoli$x[!is.na(avg.Defoli$x)])
    max1_2=max(avg.Defoli$x[!is.na(avg.Defoli$x)])
    ymin_2<-min1_2
    ymax_2<-max1_2
    ymin_2<-0

    ticknum_2=ticknum
    ygap_2=(ymax_2-ymin_2)/ticknum_2
    digitnum_2=floor(log(ygap_2)/log(10))
    ygap1_2=ygap_2/(10^digitnum_2)
    ygap2_2=ceiling(ygap1_2/5)*5*10^digitnum_2
    ymax_2=ymin_2+ygap2_2*(ticknum_2-1)
    yaxisrange_2=c(ymin_2,ymax_2)
    yaxis_2=seq(from=ymin_2,to=ymax_2,by=ygap2_2)
    ylim_2<-yaxisrange_2
    
    if (max1_2>ymax_2){
      lag=ceiling((max1_2-ymax_2)/ygap2_2)
      ymax=ymax+ygap2*lag
      ticknum=ticknum+lag
      yaxis=seq(from=ymin,to=ymax,by=ygap2)
      yaxisrange=c(ymin,ymax)
      yaxis=seq(from=ymin,to=ymax,by=ygap2)
      ylim<-yaxisrange
      ymax_2=ymax_2+ygap2_2*lag
      yaxisrange_2=c(ymin_2,ymax_2)
      yaxis_2=seq(from=ymin_2,to=ymax_2,by=ygap2_2)
      ylim_2<-yaxisrange_2
    }

    if (  (length(unique(data.fbs[!is.na(data.fbs$Yield),]$DAF))==0)  |  (length(unique(data.fbs[!is.na(data.fbs$Defoli),]$DAF))==0)  ){
      plot.new()
    }
    
    # plot yield on the left
    plot(avg$DAF, avg$Yield, type="l", xlab="", ylab="", axes=F, ylim=ylim)
    
    #time axis
    axis(1,at=avg$DAF, labels=avg$DAF, las=1, lwd=1)

    #left axis for yield
    axis(2, at=yaxis, labels=rep("",ticknum), las=2, col=colorscale[1],col.ticks=colorscale[1], lwd=2)
    mtext(side=2, at=yaxis, text=yaxis, las=2, col=colorscale[1], line=1)
    mtext(side=2, at=ymax/2, text=Y_label1, las=0,  col=colorscale[1], line=4)
    lines(avg$DAF[ !is.na(avg$Yield) ], avg$Yield[ !is.na(avg$Yield) ], col=colorscale[1], lwd=3)
    
    #right axis for Defoli
    axis(4, at=seq(ymin,ymax, length=ticknum), labels=rep("",ticknum), las=2, col=colorscale[2], col.ticks=colorscale[2],lwd=2)
    mtext(side=4, at=seq(ymin,ymax, length=ticknum), text=yaxis_2, las=2,  col=colorscale[2], line=1)
    mtext(side=4, at=ymax/2, text=Y_label2, las=0, col=colorscale[2], line=3)
    lines(avg$DAF[ !is.na(avg$Defoli) ], (ymax/ymax_2)*avg$Defoli[ !is.na(avg$Defoli) ], col=colorscale[2], lwd=3)
    
    title(main=plot_title,xpd=T,line=2)
    
    plot_title2index=unique(c(loopvar))

    plot_title2index=prioritylist(defaultSort,plot_title2index)  

    colnames(datafilter)
    
    if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])
    } else{
      plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')
    }

    par(font.main=1,cex.main=1)
    title(main=plot_title2,xpd=NA,line=1)

    maxdaf<-max(max(avg.yield$DAF[!is.na(avg.yield$DAF)]),max(avg.Defoli$DAF[!is.na(avg.Defoli$DAF)]))
    mindaf<-min(min(avg.yield$DAF[!is.na(avg.yield$DAF)]),min(avg.Defoli$DAF[!is.na(avg.Defoli$DAF)]))
    text(x = (maxdaf-mindaf)/2+mindaf, par("usr")[3], labels =X_label,  pos = 1, offset = 3.3,xpd = T,cex=1)  
    }
  add_footer()  
  }
}
