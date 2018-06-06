
radar.pdfplot=function(data,loopvar,ChartType,x,y,trial,treatment, treatments, SpeciesFont, OutputURL,pdf_plot_title,subtitle){
  #pdfset#
  pdfheight=12 #9
  pdfwidth=11 #6
  pdfname='Radar' 
  OUtputURLlast=substr(OutputURL, nchar(OutputURL), nchar(OutputURL))
  separator=ifelse(OUtputURLlast!='\\','\\','')
  file.URL=paste0(OutputURL,separator,pdfname,format(Sys.time(), format='%Y%m%d%H%M%S'),'.pdf')
  pdf(onefile=TRUE,file=file.URL,paper="USr",width=pdfwidth,height=pdfheight)
  plot.new()     
  grid.text(pdf_plot_title,gp=gpar(cex=2,font=1),y=0.75,x=0.5,just='center')
  grid.text(subtitle,gp=gpar(cex=1.6,font=1),y=0.65,x=0.5,just='center')   
  add_footer()
  radarplot(data=data,loopvar=loopvar,ChartType=ChartType,x=x,y=y,trial=trial,treatment,treatments, SpeciesFont=SpeciesFont)
    
  pdfid=dev.list()[names(dev.list())=='pdf']
  lapply(pdfid,function(x) {dev.off(which=x)})
  return(list(url=file.URL,pdfname=pdfname))
}

mround <- function(x,base){ 
        base*round(x/base) 
} 

radarplot=function(data,loopvar,ChartType,x,y,trial,treatment, treatments, SpeciesFont)
{
assign("data",data,envir = .GlobalEnv)
assign("loopvar",loopvar,envir = .GlobalEnv)
assign("ChartType",ChartType,envir = .GlobalEnv)
assign("x",x,envir = .GlobalEnv)
assign("y",y,envir = .GlobalEnv)
assign("trial",trial,envir = .GlobalEnv)
assign("treatment",treatment,envir = .GlobalEnv)
assign("treatments",treatments,envir = .GlobalEnv)
assign("SpeciesFont",SpeciesFont,envir = .GlobalEnv)
  if (ChartType=='Compare Treatments') loopvar <- setdiff(loopvar,treatment)
  if (dim(data)[1]==0) {
    plot.new()
    grid.text(paste("Plots cannot be generated,","\n","due to no data selected."),gp=gpar(cex=1,font=1),y=0.55,x=0.5,just='center')
  }else {
    filter=sqldf(paste(paste("select distinct", paste(loopvar,collapse=','),'from data order by '),paste(loopvar,collapse = ',')))
    if (length(loopvar)==0){
      plotnum=1
      datafilter=data
    } else {
      plotnum=dim(filter)[1]
    }
  for (ploti in 1:plotnum){
    head=filter[ploti,]
    if (length(loopvar)>1) {datafilter=sqldf(paste("select *",'from data natural inner join head' ))
    }else { datafilter=data[data[loopvar]==head,]}
    plot_titleindex=unique(setdiff(loopvar,treatment))
    treatmentval <- unique(datafilter[,treatment])
    if (length(plot_titleindex)==1) {plot_title=unique(datafilter[,plot_titleindex])
    }else {plot_title=paste(unique(datafilter[,plot_titleindex])[1,],collapse = '|')}
    if (ChartType=='Plot for Each Treatment') {plot_title <- paste(plot_title,treatmentval,sep='\n')
    } else { plot_title <- paste(plot_title,treatment,sep='\n')}
    datafilter=as.data.frame(datafilter)
    try({
    datafilter$x=datafilter[,x] 
    datafilter$y=as.numeric(datafilter[,y]) 
    })
    if (ChartType=='Plot for Each Treatment' ) {
      #Individual Radar Chart
      plotdata=subset(datafilter, select=c(x,y))   
      if (dim(plotdata)[1] !=0 ){
        if(all(!is.na(plotdata$y))){
          df1<-aggregate(data=plotdata, y ~ x,  function(x) mean(x))
          #df1c<-sqldf("select x, count(*) as y from plotdata group by x")
          df1c<-sqldf(paste("select x, count(distinct ", trial,") as y from datafilter group by x"))
          if (dim(df1)[1]<3) {
            plot.new()
            grid.text(paste("Plots cannot be generated,","\n","because the number of spokes must be 3 or more."),gp=gpar(cex=1,font=1),y=0.6,x=0.5,just='center')
            par(mar=c(2,4,4,4))
            title(main=plot_title,xpd=NA,line=1)
          } else {
            df2 <- dcast(df1,'x' ~ x, y = 'value')[,-1]
            df2c <- dcast(df1c,'x' ~ x, y = 'value')[,-1]
            df2 <- rbind(100,0,df2)    
            df2 <- round(df2,2)
            colnames(df2) <- paste0(colnames(df2)," (",df2c,") ")
            colnames(df2) <- gsub(' (1) ', '', colnames(df2))
            colnames(df2) <- gsub(' () ', '', colnames(df2))
            par(mar=c(2,4,6,4))
            plot_title <- paste(plot_title,"(# of trials in parentheses)",'\n',sep='\n')
            assign("df2",df2,envir = .GlobalEnv)
            assign("plot_title",plot_title,envir = .GlobalEnv)
            colors_border=c(rgb(9,174,237,maxColorValue = 255))
            radarchart(df2, axistype=1 , title=plot_title,
              #custom polygon
              pcol=colors_border , plwd=4 , plty=1,  
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8, centerzero=TRUE, 
              seg=5, caxislabels=seq(0,100,20),
              #custom labels
              vlcex=SpeciesFont/10)
          }
        }else {
          plot.new()
          grid.text(paste0("Plots cannot be generated,","\n","due to missing values in ", y,"."),gp=gpar(cex=1,font=1),y=0.55,x=0.5,just='center')
          par(mar=c(2,4,4,4))
          title(main=plot_title,xpd=NA,line=1)
        }
      }  
      add_footer()
    } else {
        #Radar Chart comparing treatments
        #Build data sets
        assign("datafilter",datafilter,envir = .GlobalEnv)
        datafilters <- subset(datafilter,get(treatment) %in% treatments)
        plotdata=subset(datafilters, select=c(get(treatment),x,y))  
        colnames(plotdata) <- c("treatment","x","y")
        plotdatac<-sqldf(paste("select ",treatment, " as treatment,x, count(distinct ", trial,") as y from datafilter group by 1,2"))
        dfc <- cast(plotdatac, treatment ~ x, mean,na.rm=TRUE)
        df <- cast(plotdata, treatment ~ x, mean,na.rm=TRUE)
        if (dim(df)[1] > 1  & dim(df)[2] > 3) {
          #Remove columns with NaN
          n<-vector()
          for (i in 2:dim(df)[2]) {
            if(TRUE %in% is.na(df[,i])) {
              n <-c(n,i)
            }
          }   
          if (length(n)>0) {
            dat <- df[,-n]
          } else {
            dat <- df
          }
          n<-vector()
          for (i in 2:dim(dfc)[2]) {
            if(TRUE %in% is.na(dfc[,i])) {
              n <-c(n,i)
            }  
          }
          if (length(n)>0) {
            datc <- dfc[,-n]
          } else {
            datc <- dfc
          }
          dat <-as.data.frame(dat)
          if (dim(dat)[2]<4) {
            plot.new()
            grid.text(paste("Plots cannot be generated,","\n","because the number of spokes must be 3 or more."),gp=gpar(cex=1,font=1),y=0.6,x=0.5,just='center')
            par(mar=c(2,4,4,4))
            title(main=plot_title,xpd=NA,line=1)
          } else {
            rownames(dat) <-dat[,1]
            colorpalette=fmc_colors()
            colors_border <- colorpalette[1:dim(df)[1]]
            dat <- dat[,-1]
            datc <- datc[,-1]
            maxmin <- rbind(apply(dat,2,max)*0+100,0)
            colnames(maxmin) <- colnames(dat)
            dat <- rbind(maxmin,dat)
            dat <- round(dat,2)
            colnames(dat)  <- paste0(colnames(dat),'(', apply(datc,2,'min'),')')
            #Create plot
            par(mar=c(2,4,4,4))
            radarchart(dat, axistype=1, seg=5, caxislabels=seq(0,100,20),
            cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8, centerzero=TRUE, 
            pcol=colors_border, plwd=4 , plty=1, title=plot_title, vlcex=SpeciesFont/10)
            legend("bottomleft", legend =treatments, bty = "n", pch=20 , col=colors_border , cex=.8, pt.cex=1) 
          }
        } else {
          plot.new()
          grid.text("Insifficient data to generate plot.",gp=gpar(cex=1,font=1),y=0.55,x=0.5,just='center')
          par(mar=c(2,4,4,4))
          title(main=plot_title,xpd=NA,line=1)
        }
        add_footer()
      }
    }
  } 
}