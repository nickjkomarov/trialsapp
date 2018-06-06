############### pdfplotfunction.R

single.pdfplot=function(data,adafa,loopvar,adafa_varname,data_varname,x,y,groupby,colorby,trt_type_name,trt_type,OutputURL,
                 X_label,Y_label,plot_title,maintitle,subtitle,ShowADAFA){
 
  assign("data",data,envir = .GlobalEnv)
  assign("adafa",adafa,envir = .GlobalEnv)
  assign("loopvar",loopvar,envir = .GlobalEnv)
  assign("adafa_varname",adafa_varname,envir = .GlobalEnv)
  assign("data_varname",data_varname,envir = .GlobalEnv)
  assign("x",x,envir = .GlobalEnv)
  assign("y",y,envir = .GlobalEnv)  
  assign("groupby",groupby,envir = .GlobalEnv)
  assign("colorby",colorby,envir = .GlobalEnv)
  assign("trt_type_name",trt_type_name,envir = .GlobalEnv)
  assign("trt_type",trt_type,envir = .GlobalEnv)
  assign("OutputURL",OutputURL,envir = .GlobalEnv)
  assign("X_label",X_label,envir = .GlobalEnv)
  assign("Y_label",Y_label,envir = .GlobalEnv)
  assign("plot_title",plot_title,envir = .GlobalEnv)
  assign("maintitle",maintitle,envir = .GlobalEnv)
  assign("subtitle",subtitle,envir = .GlobalEnv)
  assign("ShowADAFA",ShowADAFA,envir = .GlobalEnv)
  
  data[,trt_type_name]=as.character(data[,trt_type_name])
  #pdfset#
  pdfheight=8
  pdfwidth=10.5
  
  pdfdefaultSort=c('year','region','country','crop','pest','SMBP','symptom','method','basis','part','trial_NO_')
 
  adafa=adafa[,adafa_varname]
  colnames(adafa)=c(data_varname,'ADAFA')

  adafa=merge(x=adafa,y=data,by=data_varname,x.all=TRUE,y.all=FALSE)
  adafa=unique(adafa[,c(adafa_varname[-2],trt_type_name)])

  pdfname='SingleTrialPlot'
  OUtputURLlast=substr(OutputURL, nchar(OutputURL), nchar(OutputURL))

  separator=ifelse(OUtputURLlast!='\\','\\','')
  file.URL=paste0(OutputURL,separator,pdfname,format(Sys.time(), format='%Y%m%d%H%M%S'),'.pdf')
  try(pdf(onefile=TRUE,file=file.URL,paper="USr",width=pdfwidth,height=pdfheight))

  plot.new()

  if (!nchar(sub('\\s+','',subtitle))){
  grid.text(maintitle,gp=gpar(cex=3,font=1),y=0.7,x=0.5,just='center')
  grid.text('Graphical Summary-Lineplots',gp=gpar(cex=1.6,font=1),y=0.45,x=0.5,just='center')
  }else{
    grid.text(maintitle,gp=gpar(cex=3,font=1),y=0.75,x=0.5,just='center')
    grid.text(subtitle,gp=gpar(cex=1.6,font=1),y=0.65,x=0.5,just='center')
    grid.text('Graphical Summary-Lineplots',gp=gpar(cex=1.6,font=1),y=0.45,x=0.5,just='center')
  }
  add_footer() 
  byvari=unique(c(loopvar,x))
  content=unique(data[,byvari])
  contentsummary=data.table(content)

  byvari2=unique(loopvar)
  contentsummary=contentsummary[,list(Evals.No=.N),by=byvari2]
  contentsummary=as.data.frame(contentsummary)

  perpage=20
  totaltrialnum=dim(contentsummary)[1]
  tableno=max(1,ceiling(totaltrialnum/perpage))
  
  for (i in 1:tableno) {
    plot.new()
    grid.table(contentsummary[((i-1)*perpage+1):min(i*perpage,dim(contentsummary)[1]),], rows=NULL)
    tabletitle='Table of Contents'
    if (tableno!=1){
      tabletitle=paste0(tabletitle,'(',i,')')
    }
    grid.text(tabletitle,gp=gpar(cex=1.2,font=2),y=0.9,x=0.5,just='center')
    add_footer() 
  }

  singleline(data=data,
                 adafa=adafa,
                 loopvar=loopvar,
                 x=x,
                 y=y,
                 groupby=groupby,
                 colorby=colorby,
                 trt_type=trt_type,
                 trt_type_name=trt_type_name,
                 X_label=X_label,
                 Y_label=Y_label,
                 plot_title=plot_title,
                 ShowADAFA=ShowADAFA)
  pdfid=dev.list()[names(dev.list())=='pdf']
  lapply(pdfid,function(x) {dev.off(which=x)})
  return(list(url=file.URL,pdfname=pdfname))
}

singleline=function(data,loopvar,adafa,x,y,groupby,colorby,trt_type,trt_type_name,
              X_label,Y_label,plot_title,ShowADAFA){

  defaultSort=c('year','region','country','crop','pest','SMBP','symptom','method','basis','part','trial_NO_')
  loopvar=prioritylist(defaultSort,loopvar)  

  data[,x]=as.numeric(data[,x])
  data[,y]=as.numeric(data[,y])

  filter <- unique(data[,loopvar])
  plotnum=dim(filter)[1]

  for (ploti in 1:plotnum){
    head=filter[ploti,]
    datafilter=sqldf(paste("select *",'from data natural inner join head' ))
    
    datafilter=datafilter[!is.na(datafilter[,y]),]
    datafilter=datafilter[!is.na(datafilter[,x]),]

    if (nrow(datafilter)!=0){
      #chanprioritylist=c('Opt')
      #chanvariablelist=prisort(chanprioritylist,colnames(datafilter))
      #assign("chanvariablelist", chanvariablelist ,envir = .GlobalEnv)
      #chan=chanvariablelist[1]
      #if (  (max(nchar(unique(datafilter[,groupby])))>30) & grepl('Opt', chan) ) {
      #  for (i in 1:nrow(datafilter)){
      #    if (nchar(datafilter[i,groupby])>30){
      #      datafilter[i,groupby]<-datafilter[i,chan]
      #    }
      #  } 
      #}
       
      datafilter<-datafilter[order(nchar(datafilter[,groupby])),]

      if (ShowADAFA){
        Trial_No=unique(datafilter[,colnames(adafa)[1]])

        ADAFA=adafa[which(adafa[,1]%in%Trial_No),c(2,3)]

        ADAFA_trt=ADAFA
        ADAFA_trt=table(ADAFA_trt)
        ADAFA_trt=(ADAFA_trt>=1) 
        Productindex=integer(0)
        STDindex=integer(0)
 
        if (trt_type[3]!='NONE'&any(trt_type[3]%in%colnames(ADAFA_trt))){
          Productindex=ifelse(is.na(ADAFA_trt[,trt_type[3]]),rep(0,dim(ADAFA_trt)[1]),ADAFA_trt[,trt_type[3]])
        }
        if (trt_type[1]!='NONE'&any(trt_type[1]%in%colnames(ADAFA_trt))){
          STDindex=ifelse(is.na(ADAFA_trt[,trt_type[1]]),rep(0,dim(AadDAFA_trt)[1]),ADAFA_trt[,trt_type[1]])
        }
        overlayindex=Productindex*STDindex

        Product=as.numeric(rownames(ADAFA_trt)[which(Productindex!=0)])
        STD=as.numeric(rownames(ADAFA_trt)[which(STDindex!=0)])
        overlay=as.numeric(rownames(ADAFA_trt)[which(overlayindex!=0)])
        referencetick=as.numeric(rownames(ADAFA_trt))
      }

      datafiltersummary=datafilter[,c(x,y,groupby)]
      colnames( datafiltersummary)=c('x','y','by')
      datafiltersummary=data.table(datafiltersummary)
      datafiltersummary=datafiltersummary[,list(y=mean(y)),by=c('x','by')]
      datafiltersummary=as.data.frame(datafiltersummary)
    
      datafilterby=unique(datafilter[,c(groupby,colorby,trt_type_name)])
      datafilterby=as.data.frame(datafilterby)
      colnames(datafilterby)=c('by','colvar','Trt_Type')

      #axis set#
      #xaxis set#
      xaxis=unique(datafiltersummary$x)
      xaxisrange=c(0,max(xaxis))
    
      ticknum=5
      ymax=max(datafiltersummary$y[which(!is.na(datafiltersummary$y))])
      ymin=min(datafiltersummary$y[which(!is.na(datafiltersummary$y))])
      ymin=ymin-(ymax-ymin)*0.2
    
      if (ymax!=ymin) {
        ygap=(ymax-ymin)/ticknum
        digitnum=floor(log(ygap)/log(10))
        ygap1=ygap/(10^digitnum)
        ygap2=ceiling(ygap1/5)*5*10^digitnum
        ymin=floor(ymin/ygap2)*ygap2
        ymax=ceiling(ymax/ygap2)*ygap2
        yaxisrange=c(ymin-ygap2/2,ymax+ygap2/2)
        yaxis=seq(from=ymin,to=ymax,by=ygap2)
      }else{
        yaxisrange=c(ymin-1,ymax+1)
        yaxis=c(ymin)
      }
    
      #palette and lty#
	  #########trt_type has 3 types untrt, stdrd, product##########
      untrt=which(datafilterby$Trt_Type==trt_type[2])
      untrtnum=length(untrt)
    
      stdrd=which(datafilterby$Trt_Type==trt_type[1])
      stdrdnum=length(stdrd)
    
      product=which(datafilterby$Trt_Type==trt_type[3])
      productnum=length(product)

      colorvar=rep('white',dim(datafilterby)[1])
      colorpalette=c('darkgreen','red','blue','deeppink','cyan','chartreuse','violetred4','tomato',brewer.pal(12,"Paired"),'slateblue1')
      colorvar=colorpalette[match(datafilterby$colvar,unique(datafilterby$colvar))]
      colorvar[untrt]='black' 
      palette(colorvar)

      ltyvar=rep(1,dim(datafilterby)[1])
      ltyvar[untrt]=6
      ltyvar[stdrd]=5
    
      pchvar=rep(1,dim(datafilterby)[1])
    
      pchvar[product]=1:11
      pchvar[untrt]=c(20,19,16)
      pchvar[stdrd]=c(15,16,18)
    
      pcexvar=rep(0.5,dim(datafilterby)[1])
      pcexvar[stdrd]=0.6

      plot.new()
      par(fig=c(0,1,0.2,0.8), new=TRUE,mar=c(4,4,0,4))

      graphics::plot(x=c(),y=c(),ylim=yaxisrange,xlim=xaxisrange,axes=FALSE,ylab =Y_label,xlab='',family='Times')
    
      #border#
      rect(grconvertX(0, from='npc'), grconvertY(0, from='npc'),
           grconvertX(1, from='npc'), grconvertY(1, from='npc'), 
           col=NA,lwd=1)
    
      #yaxis left#
      axis(2,at=yaxis,family="Times",label=yaxis,tick=TRUE,tcl=0.5,lwd=1)

      #xaxis below#
      axis(1,at=NULL,family="Times",tick=TRUE,tcl=0.5,lwd=1,padj=-1.5)
      title(main = NULL, sub = NULL, xlab = X_label, ylab = NULL,
            line = 1.8, outer = FALSE,family="Times") 
    
      #xaxis upper#
      axisupper=unique(c(xaxis))
      if (ShowADAFA){
        axisupper=unique(c(xaxis,referencetick))
        #reference line
        axis(3,at=axisupper,label=axisupper,family="Times",tick=FALSE,tcl=0.5,lwd=1,padj=1.5)
        axis(3,at=overlay,tck=1,col.tick='burlywood3',lty=1,lwd.ticks=5,labels = FALSE)
        axis(3,at=Product,tck = 1,col.ticks='burlywood3',lty=5,lwd.ticks=2,labels = FALSE)
        axis(3,at=STD,tck = 1,col.ticks='aquamarine4',lty=5,lwd.ticks=2,labels = FALSE)
      }
    
      referencecol=c('aquamarine4','burlywood3')
      referencelty=c(5,5)
    
      #####title#####

      par(font.main=1,cex.main=1.3,family="Times")
    
      plot_title2index=unique(c(loopvar))
      plot_title2index=prioritylist(defaultSort,plot_title2index)  
      if (length(unique(datafilter[,plot_title2index]))==1) {
  	    plot_title2=unique(datafilter[,plot_title2index])[1]
      } else{
        plot_title2=paste(unique(datafilter[,plot_title2index])[1,],collapse = '|')
      } 

      title(main=plot_title,xpd=NA,line=2.5)
      title(main=plot_title2,xpd=NA,line=1.5)

      add_footer() 
	  
      #points and lines
      for (ploti in 1:(dim(datafilterby)[1])){
        datafilterfilter=datafilterby$by[ploti]
        datafilterplot=datafiltersummary[which(datafiltersummary$by==datafilterfilter),]
        datafilterplot=datafilterplot[order(datafilterplot$x),]
        lines(x=datafilterplot$x,y=datafilterplot$y,col=ploti,lty=ltyvar[ploti],lwd=3.5)
        points(x=datafilterplot$x,y=datafilterplot$y,col=ploti,pch=pchvar[ploti],cex=pcexvar[ploti])
        text(x=datafilterplot$x,y=datafilterplot$y,labels=round(datafilterplot$y,digits=2),col=ploti,cex=0.85,adj=c(0,1.2))
      }
    
      if (ShowADAFA){
        #legend("bottomright",legend=c(paste(c(': ',trt_type[3],' ADAFA'),collapse = ''),': Standard ADAFA'),col=referencecol,lty=referencelty,lwd=1.5,cex=1,xpd = TRUE,bty="n ",ncol=1,text.col=referencecol,seg.len=4)
        legend("bottomright",legend=c(': FMC ADAFA',': Std ADAFA'),col=referencecol,lty=referencelty,lwd=1.5,cex=1,xpd = TRUE,bty="n ",ncol=1,text.col=referencecol,seg.len=4)
      }
	  
      #legend for prog_alias
   
      par(fig=c(0,1,0,0.21),new=TRUE,mar=c(0,0,0,0),family="Times")

      if (  length(unique(datafilter[,"Prog_Alias"]))>14 ) {
        colnum=3
	  } else{
        colnum=2
      }

      #colnum=3
      textcex=1
    
      assign('legend',datafilterby$by,envir=.GlobalEnv)
      assign('ltyvar',ltyvar,envir=.GlobalEnv)
      #datafilterby$by=gsub(' ','',datafilterby$by)
      text=datafilterby$by
	  text=trimws(text, "l")
      textwidth=strwidth(text,units='inch',cex=textcex)
      textwidth=textwidth/(par('din')[2])
    
      legendnum=length(textwidth)
      rownum=ceiling(length(textwidth)/colnum)
      length(textwidth)= rownum*colnum
   
      textwidth[is.na(textwidth)]=0
      textwidth1=matrix(textwidth,ncol=colnum)
    
      if (dim(textwidth1)[1]>1){
        textwidth1=apply(textwidth1,2,max)
      }
    
      legendwidth=sum(textwidth1)+(colnum-1)*0.03
 
      xini=grconvertX(0.5-legendwidth/2, from =  "ndc", to ='user')
      legendposition=legend('top',legend='a',plot=FALSE)
      yini= legendposition$rect$top
      assign('xini',xini,envir=.GlobalEnv)

      for (coli in 1:colnum){
        legendtext= text[((coli-1)*rownum+1):min(coli*rownum,legendnum)]
        pchvari= pchvar[((coli-1)*rownum+1):min(coli*rownum,legendnum)]
        ltyvari=ltyvar[((coli-1)*rownum+1):min(coli*rownum,legendnum)]
        colorvari=colorvar[((coli-1)*rownum+1):min(coli*rownum,legendnum)]
        if (coli==1){
          legendinfo=legend(x=xini,y=yini,xjust=0,seg.len=3,pch=pchvari,legend=legendtext,lty=ltyvari,col=colorvari,lwd=1.5,cex=textcex,xpd = NA,bty='n')
        }else{
          legendinfo=legend(x=(legendinfo$rect$left+legendinfo$rect$w),y=yini,xjust=0,seg.len=3,pch=pchvari,legend=legendtext,lty=ltyvari,col=colorvari,lwd=1.5,cex=textcex,xpd = NA,bty='n')
        }      
      }  
    }     
  }
}