#install package#
CheckInstallPackages(c("openxlsx","data.table","zoo","graphics","grid","sqldf","RColorBrewer"))
CheckInstallPackages(c("gdata","data.table","tcltk","grDevices","graphics","gtable","zoo","stringr"))
CheckInstallPackages(c("lsmeans","parallel","lme4","pbkrtest","multcomp","R.utils","plotrix"))

############### pdfplotfunction.R

relative.pdfplot=function(data,pdfloopvar,loopvar,x,y,trial,rep,colorby,control1,OutputURL,
                 X_label,Y_label,pdf_plot_title,plot_title,subtitle,DataDate){
  
  
  #pdfset#
  pdfheight=8
  pdfwidth=10.5
  
  pdfdefaultSort=c('country','crop','pestid')
  pdfloopvar=prisort(pdfdefaultSort,pdfloopvar)  
  
  if (length(pdfloopvar)==0){
    pdfloopflag=FALSE
    pdfnum=1
    pdfdatafilter=data
    pdfname='Relative'
  } else {
    pdfloopflag=TRUE
    pdffilter=sqldf(paste("select distinct", paste(pdfloopvar,collapse=','),'from data' ))
    pdfnum=dim(pdffilter)[1]
    
  }
  
  
  
  
  for (pdfi in 1:pdfnum){
    
    #svalue(runinfo_Sub21[3,1])=paste0(pdfi,'/',pdfnum)
    
    if (pdfloopflag){
      
      pdfhead=pdffilter[pdfi,]
      
      if (length(pdfloopvar)>1) {
        pdfdatafilter=sqldf(paste("select *",'from data natural inner join pdfhead' ))
      }else{
        
        pdfdatafilter=data[data[pdfloopvar]==pdfhead,]
      }
      pdfname=paste(c('Relativeplot',pdfhead),collapse='-')
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
      #grid.text('Graphical Summary-Relative to Control Plot',gp=gpar(cex=1.6,font=1),y=0.45,x=0.5,just='center')
    }else{
      
      grid.text(pdf_plot_title,gp=gpar(cex=3,font=1),y=0.75,x=0.5,just='center')
      grid.text(subtitle,gp=gpar(cex=1.6,font=1),y=0.65,x=0.5,just='center')
      grid.text(title1,gp=gpar(cex=1.6,font=1),y=0.55,x=0.5,just='center')
      #grid.text('Graphical Summary-Relative to Control Plot',gp=gpar(cex=1.6,font=1),y=0.45,x=0.5,just='center')
      
    }
    
    
    datadateinfo='Based on Data Extracted From\n Data Warehouse'
    
    if (nchar(sub('\\s+','',DataDate))){
      datadateinfo=paste(datadateinfo,'on',DataDate)
    }
    
    
    datadateinfo='Based on Data Extracted From\n Data Warehouse'
    
    if (nchar(sub('\\s+','',DataDate))){
      datadateinfo=paste(datadateinfo,'on',DataDate)
    }
    
    grid.text(datadateinfo,gp=gpar(cex=0.9,font=1),y=0.05,x=0.5,just='center')  
    
    
    relativeplot(data=pdfdatafilter,loopvar=loopvar,x=x,y=y,trial=trial,rep=rep,colorby=colorby,control1=control1,
                 X_label=X_label,Y_label=Y_label,plot_title=plot_title)
    
    
    pdfid=dev.list()[names(dev.list())=='pdf']
    lapply(pdfid,function(x) {dev.off(which=x)})

    
  }
  
  return(list(url=file.URL,pdfname=pdfname))
}



############### plotfunction.R

relativeplot=function(data,loopvar,x,y,trial,rep,colorby,control1,X_label,Y_label,plot_title)
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
    #svalue(runinfo_Sub21[5,1])=paste0(ploti,'/',plotnum)
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
    #colnames(datafilter)[match(c(x,y,trial,rep,colorby),colnames(datafilter))]=c('x','y','trial','rep','colvar')
    datafilter$x=as.character(datafilter[,x])
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
    color<-as.numeric(as.factor(datafilter$colvar))
    cat("color:",as.character(datafilter$colvar))
    
    assign("Trt", Trt ,envir = .GlobalEnv)
    assign("Response", Response ,envir = .GlobalEnv)
    assign("Trial", Trial ,envir = .GlobalEnv)
    assign("Rep", Rep ,envir = .GlobalEnv)
    assign("color", color ,envir = .GlobalEnv)
    
    
    newdata<-data.frame(Trial, Rep, Trt, Response, color)
    
    colortrt<-unique(newdata[c('Trt','color')])
    rownames(colortrt) <- NULL
    coloruni<-unique(colortrt$color)
    #create mean table by treatment#
    if(length(newdata$Response)!=0){
    plottable<-aggregate(newdata$Response, list(newdata$Trt), mean)
    colnames(plottable) <- c("Treatment", "MeanResponse")
    round(plottable$MeanResponse, digits=2)
    
    if (control1 %in% unique(plottable$Treatment)) {
      
      if (length(unique(plottable$Treatment))==1){
        plot.new()
        grid.text("Not enough data.",gp=gpar(cex=1.6,font=1),y=0.55,x=0.5,just='center')
        title(main=plot_title,xpd=T,line=2)
        
        plot_title2index=unique(c(loopvar))
        plot_title2index=prioritylist(defaultSort,plot_title2index)  
        if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])
        } else{
          plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')
        }
        
        par(font.main=1,cex.main=1)
        title(main=plot_title2,xpd=NA,line=1)
        
      } else{
          
        assign('plottable',plottable,envir=.GlobalEnv)
        
        #calculate relative response to control#
        kk=(plottable$Treatment==control1)
        if (plottable$MeanResponse[kk] != 0){
          plottable$RelResponse<-plottable$MeanResponse/plottable$MeanResponse[kk]
        } else{
          plottable$MeanResponse[kk] = 0.00001
          plottable$RelResponse<-plottable$MeanResponse/plottable$MeanResponse[kk]
        }
        
        plottable$ConResponse<-round(plottable$RelResponse*plottable$MeanResponse,digits=2)
        plottable$ConResponse1 <- sapply(plottable$MeanResponse,function(x) ifelse(x<plottable$MeanResponse[kk],x-abs(x-plottable$MeanResponse[kk]),x+abs(x-plottable$MeanResponse[kk])))
        plottable$MeanResponse<-round(plottable$MeanResponse,digits=2)
        
        finaltable=plottable
        colnames(colortrt)[1]<-"Treatment"
        finaltable<-merge(colortrt,finaltable,by="Treatment")
        names(finaltable)[names(finaltable)=="color"] <- "Trt_Type"
        
        #try(levels(finaltable$Treatment) <- sapply(levels(finaltable$Treatment),"break_str",32))
        
        finaltable$col<-0
        #colscale<-c("Red","Blue","seagreen","Purple","darkturquoise","orange","magenta","coral4","gray40","navy","#FFFF99","#B15928")
        color_list <- colors(distinct = TRUE)
        color_list <- unique(sapply(color_list, function(x) gsub('[0-9]','',x)))
        color_list <- color_list[!color_list %in% grep("white+",color_list,perl=TRUE,value=TRUE)]
        color_list <- color_list[!color_list %in% grep("yellow+",color_list,perl=TRUE,value=TRUE)]
        color_list <- color_list[1:120]
        color_list <- unique(c("Red","Blue","seagreen","Purple","darkturquoise","orange","magenta","coral4","gray40","navy","#FFFF99","#B15928",sort(color_list,decreasing = TRUE)))
        colscale <- color_list
        for (i in 1:length(coloruni)) {
          
          if (finaltable$Treatment!= control1) {
            filterno<-which(finaltable$Trt_Type==coloruni[i])
            finaltable$col[filterno]<-colscale[i]
          }
        }
         
        finaltable$col[which(finaltable$Treatment==control1)]<-"Black"
        assign('finaltable',finaltable,envir=.GlobalEnv)

        #start plot#
        n = dim(finaltable)[1]
        perplot=25
        nplot=floor(n/perplot)
        nplot=max(floor(n/perplot),ceiling(n/perplot))
        
        finaltable1=finaltable

        for (j in 1:nplot) {
        #plot.new()
          
        if(j<nplot){
          seq1=25*(j-1)+1
          seq2=25*j
        }else{
          seq1=25*(j-1)+1
          seq2=n
        }
          
        finaltable= finaltable1[seq(seq1,seq2),]
        assign('finaltable',finaltable,envir=.GlobalEnv)
        max=max(c(finaltable$MeanResponse,finaltable$ConResponse))
        min=min(c(finaltable$MeanResponse,finaltable$ConResponse))
        
        ###
     
        margin <- 0.075*(max-min)
        

        dotchart(finaltable$MeanResponse,  xlim=c(min-margin, max+margin), color='black', 
                 labels=paste0('\n',break_str(as.character(finaltable$Treatment),32)), xlab=X_label)

        points(y=seq(1,(seq2-seq1+1)),x=finaltable$MeanResponse,col=finaltable$col,type = "p",pch=1)
        
        for (i in 1:(seq2-seq1+1)  ) {
          if ( paste(100*round((finaltable$ConResponse[i] -finaltable$MeanResponse[i])/finaltable$MeanResponse[i], 2), "%", sep="")=="0%" ){
            finaltable$ConResponse[i]<- finaltable$MeanResponse[i]
          }
        }
 
        arrows(finaltable$MeanResponse, seq(1,seq2-seq1+1), finaltable$ConResponse1, seq(1,seq2-seq1+1), col=finaltable$col,length=0.08 ,angle=25,lwd=1.7)
       
        print(paste("Sep = ",  seq(1,seq2-seq1+1)))
        text((finaltable$MeanResponse+finaltable$ConResponse)/2, seq(1,seq2-seq1+1)-0.3, 
             paste(100*round((finaltable$ConResponse -finaltable$MeanResponse)/finaltable$MeanResponse, 2), "%", sep=""), col=finaltable$col, cex=0.9, font=2)
        
        assign("finaltable",finaltable, envir = .GlobalEnv)
        assign("seq1",seq1, envir = .GlobalEnv)
        assign("seq2",seq2, envir = .GlobalEnv)
        assign("min",min, envir = .GlobalEnv)
        assign("margin",margin, envir = .GlobalEnv)
        
        assign("finaltableMeanResponse",finaltable$MeanResponse, envir = .GlobalEnv)
        assign("seq1",seq(1,seq2-seq1+1), envir = .GlobalEnv)
        assign(" finaltableConResponse", finaltable$ConResponse, envir = .GlobalEnv)
        assign("finaltablecol",finaltable$col, envir = .GlobalEnv)
        print(paste0('ConResponse',finaltable$ConResponse))
        
        print(paste0("finaltableConResponse : ", finaltable$ConResponse, '\n'))
        print(paste0("finaltableMeanResponse : ", finaltableMeanResponse, '\n'))
        print(paste0("text : ", (finaltable$ConResponse -finaltable$MeanResponse)/finaltable$MeanResponse, '\n'))
        
        
        maxf1<-max(finaltable$ConResponse)
        maxf2<-max(finaltable$MeanResponse)
        minf1<-min(finaltable$ConResponse)
        minf2<-min(finaltable$MeanResponse)
        maxff<-max(maxf1,maxf2)
        minff<-max(minf1,minf2)
        
        
        position<- seq(1,seq2-seq1+1)

        for (i in 1:(seq2-seq1+1)   ) {
          if ( (100*as.numeric(round((finaltable$ConResponse[i] -finaltable$MeanResponse[i])/finaltable$MeanResponse[i], 2))>=0) |( (as.numeric(finaltable$MeanResponse[i])==0) & (as.numeric(finaltable$ConResponse[i])==0)) )
          {position[i]=finaltable$MeanResponse[i]-0.05*(maxff-minff)} else  
          {position[i]=finaltable$MeanResponse[i]+0.05*(maxff-minff)}
          
        }
        
        text(position, seq(1,(seq2-seq1+1)), paste(finaltable$MeanResponse), col=finaltable$col, cex=0.9, font=2)
        
        title(main=plot_title,xpd=T,line=2)
        
        
        plot_title2index=unique(c(loopvar))
        plot_title2index=prioritylist(defaultSort,plot_title2index)  
        
        if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])
        } else{
          plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')
          
        }
        
        if(nplot!=1){
          plotpagenum=paste(c("Page", j),collapse = '_')
          plot_title2=paste(c(plot_title2, plotpagenum),collapse = '|')
        }
        
        par(font.main=1,cex.main=1)
        title(main=plot_title2,xpd=NA,line=1)

      } 
      
    }
    
    }
    
    else {
      plot.new()
      grid.text("Control not found in this data.",gp=gpar(cex=1.6,font=1),y=0.55,x=0.5,just='center')
      title(main=plot_title,xpd=T,line=2)

      plot_title2index=unique(c(loopvar))
      plot_title2index=prioritylist(defaultSort,plot_title2index)  
      if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])
      } else{
        plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')
      }
      
      par(font.main=1,cex.main=1)
      title(main=plot_title2,xpd=NA,line=1)

    }
    
  }
}
}