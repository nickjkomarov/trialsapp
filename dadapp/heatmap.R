# pdfplotfunction.R
heatmap.pdfplot=function(data,loopvar,x,y,trial,rep,DAFRange,control1,OutputURL,
                 X_label,Y_label,pdf_plot_title,plot_title,subtitle){
  #pdfset#
  pdfheight=12 #9
  pdfwidth=11 #6
  pdfdatafilter=data 
  pdfname='HeatMap'
    
  OUtputURLlast=substr(OutputURL, nchar(OutputURL), nchar(OutputURL))
  separator=ifelse(OUtputURLlast!='\\','\\','')
  file.URL=paste0(OutputURL,separator,pdfname,format(Sys.time(), format='%Y%m%d%H%M%S'),'.pdf')
  assign("file.URL",file.URL, envir = .GlobalEnv)  
  pdf(onefile=TRUE,file=file.URL,paper="USr",width=pdfwidth,height=pdfheight)
  plot.new()   
    
  if (!nchar(sub('\\s+','',subtitle))){
    grid.text(pdf_plot_title,gp=gpar(cex=2,font=1),y=0.7,x=0.5,just='center')
    grid.text('Graphical Summary-Heat Map',gp=gpar(cex=1.6,font=1),y=0.45,x=0.5,just='center')
  }else{     
    grid.text(pdf_plot_title,gp=gpar(cex=2,font=1),y=0.75,x=0.5,just='center')
    grid.text(subtitle,gp=gpar(cex=1.6,font=1),y=0.65,x=0.5,just='center')
    grid.text('Graphical Summary-Heat Map',gp=gpar(cex=1.6,font=1),y=0.45,x=0.5,just='center')     
  } 
  add_footer()   
  heatmap(data=pdfdatafilter,loopvar=loopvar,x=x,y=y,trial=trial,rep=rep,DAFRange=DAFRange,control1=control1,
         X_label=X_label,Y_label=Y_label,plot_title=plot_title)
    
  pdfid=dev.list()[names(dev.list())=='pdf']
  lapply(pdfid,function(x) {dev.off(which=x)})
  return(list(url=file.URL,pdfname=pdfname))
}

# plotfunction.R
heatmap=function(data,loopvar,x,y,trial,rep,DAFRange, control1,X_label,Y_label,plot_title)
{
assign("data",data, envir = .GlobalEnv) 
assign("loopvar",loopvar, envir = .GlobalEnv) 
assign("x",x, envir = .GlobalEnv) 
assign("y",y, envir = .GlobalEnv) 
assign("trial",trial, envir = .GlobalEnv) 
assign("rep",rep, envir = .GlobalEnv) 
assign("DAFRange",DAFRange, envir = .GlobalEnv) 
assign("control1",control1, envir = .GlobalEnv) 
assign("X_label",X_label, envir = .GlobalEnv) 
assign("Y_label",Y_label, envir = .GlobalEnv) 
assign("plot_title",plot_title, envir = .GlobalEnv) 
  defaultSort=c('Country','Pestid','Trial_year')
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
    datafilter$x=datafilter[,x]
    datafilter$y=as.numeric(datafilter[,y])
    datafilter$trial=datafilter[,trial]
    datafilter$rep=datafilter[,rep]
    datafilter$DAFRange=datafilter[,DAFRange]
    newdata <- datafilter[which(datafilter$x==control1),]  
    plotdata=subset(newdata, select=c(trial,PestID,rep, x,y, DAFRange))    
    colnames(plotdata)=c("Trial.No._Orig","PestID","REP","Prog_Alias","Response.Eval.Avg","DAFRange")
    if (dim(plotdata)[1] !=0 ){
    if(all(!is.na(plotdata$Response.Eval.Avg))){
    Pestpressure<-aggregate(data=plotdata, Response.Eval.Avg~ Trial.No._Orig+DAFRange,  function(x) mean(x))
    castmatrix<-cast(Pestpressure, Trial.No._Orig ~ DAFRange)
    assign("newdata",newdata, envir = .GlobalEnv)
    assign("plotdata",plotdata, envir = .GlobalEnv)
    assign("Pestpressure",Pestpressure, envir = .GlobalEnv)
    assign("castmatrix",castmatrix, envir = .GlobalEnv)
    #Generate heatmap
    #par(mar=c(8,8,3,3)+0.1)
    par(mar=c(2,2,2,2))
    col <- Pestpressure$DAFRange
    sort_order <- order(col)
    if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) sort_order <- order(as.numeric(col))
    sorted_df <- Pestpressure[sort_order,]
    sorted_df_mx <-cast(Pestpressure, Trial.No._Orig ~ DAFRange)
    row.names(sorted_df_mx) <- sorted_df_mx$Trial.No._Orig
    sorted_df_mx <- sorted_df_mx[,2:ncol(sorted_df_mx)]
    col_name <- colnames(sorted_df_mx)
    if (!is.null(col_name)) {
      col_order <- order(col_name)
      if (suppressWarnings(all(!is.na(as.numeric(as.character(col_name)))))) col_order <- order(as.numeric(col_name))
      sorted_df_mx <- sorted_df_mx[,col_order]
    }
    sorted_df_matrix <- data.matrix(sorted_df_mx)
    plot_title2index=unique(c(loopvar))
    plot_title2index=prioritylist(defaultSort,plot_title2index)  
    if (length(plot_title2index)==1) { plot_title2=paste(unique(datafilter[,plot_title2index])[1])
    } else{
        plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')
        plot_title2 <- break_str1(plot_title2,80,'\\|') 
        plot_title2 <- paste0(plot_title2,"\nTreatment: ",control1)
      }
      # (optional) defines the color breaks manually for a "skewed" color transition
      lp <- levelplot(t(sorted_df_matrix), 
              #cuts = dim(sorted_df_matrix)[2],
              xlab=X_label , ylab=Y_label , main=paste(plot_title ,plot_title2,sep='\n'),
              #col.regions = heat.colors(100)[length(heat.colors(100)):1])
              col.regions = colorRampPalette(c("yellow", "orange", "red"))(1000))
      assign("lp",lp, envir = .GlobalEnv) 
      print(lp)
     }else {
          grid.text(paste("Plots cannot be generated,","\n","due to missing values in response."),gp=gpar(cex=1,font=1),y=0.55,x=0.5,just='center')
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
    add_footer()
  }
}