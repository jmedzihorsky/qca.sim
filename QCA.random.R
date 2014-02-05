QCA.random<-function(n, type, data, outcome, conditions, min.incl.cut, max.incl.cut, min.n.cut, max.n.cut, reps, plot, plot.legend, ...){
  
  library(ggplot2)
  
  if(!is.numeric(n)){
    stop("'n' must be of class 'numeric'")
  }
  
  if(!(type %in% c("binary", "uniform"))){
    stop("'type' must be either 'binary' or 'uniform'")
  }
  
  if(type=="binary"){
    data.list<-replicate(n=n, simplify=F, expr={
      data$random.variable<-sample(x=c(0,1), size=nrow(data), replace=T)
      data
    })
  }
  
  if(type=="uniform"){
    data.list<-replicate(n=n, simplify=F, expr={
      data$random.variable<-runif(n=nrow(data), min=0, max=1)
      data
    })
  }
  
  out<-lapply(X=data.list, FUN=function(y, conditions){
    if(!missing(conditions)){
      # Apply QCA.sim.inclcut across different n.cut values
      out<-lapply(X=min.n.cut:max.n.cut, FUN=function(x){
        as.data.frame(QCA.sim.inclcut(data=y, outcome=outcome, conditions=conditions, min.incl.cut=min.incl.cut, max.incl.cut=max.incl.cut, n.cut=x, reps=reps, ...))
      })
    }else {
      # Apply QCA.sim.inclcut across different n.cut values
      out<-lapply(X=min.n.cut:max.n.cut, FUN=function(x){
        as.data.frame(QCA.sim.inclcut(data=y, outcome=outcome, min.incl.cut=min.incl.cut, max.incl.cut=max.incl.cut, n.cut=x, reps=reps, ...))
      })
    }
  })
  
  # Bind results
  out<-unlist(x=out, recursive=F)
  results<-rbind.fill(out)
  
  # Parse result strings
  config<-do.call(paste, as.data.frame(results[,4:ncol(results)], stringsAsFactors=FALSE))
  config<-gsub(pattern=" NA", replacement="", x=config, )
  config<-gsub(pattern=" ", replacement=" + \n  ", x=config)
  
  # Clean results
  results<-cbind(results[,1:2], paste("Frequency Threshold = ", results[,3], sep=""), config, stringsAsFactors=F)
  colnames(results)[3]<-"n.cut"
  results<-arrange(df=results, desc(incl.cut1), desc(incl.cut0), n.cut)
  results$config<-ifelse(test=results$config=="NA", yes="No Solution", no=results$config)
  results$config.id<-factor(x=results$config, labels=1:length(unique(results$config)))
  
  # Create a legend of solutions
  legend<-lapply(X=1:length(unique(as.numeric(results$config.id))), function(x){
    list("config.id"=unique(as.numeric(results$config.id))[x], "config"=unique(as.character(results$config))[x])
  })
  
  # Plots
  if(missing(plot)){
    plot<-F
  }
  if(missing(plot.legend)){
    plot.legend<-"solutions"
  }
  if(plot){
    if(plot.legend=="solutions"){
      plot<-ggplot(data=results, aes(x=incl.cut0, y=incl.cut1))+
        geom_point(aes(color=config))+scale_color_hue(c=150, l=60, name="Configuration")+
        xlim(0,1)+ylim(0,1)+
        xlab(label="Maximum Sufficiency Inclusion Score")+
        ylab(label="Minimum Sufficiency Inclusion Score")+
        guides(col=guide_legend(keyheight=2))+facet_wrap(~n.cut, ncol=2)
    }else if(plot.legend=="ids"){
      plot<-ggplot(data=results, aes(x=incl.cut0, y=incl.cut1))+
        geom_point(aes(color=config.id))+scale_color_hue(c=150, l=60, name="Configuration ID")+
        xlim(0,1)+ylim(0,1)+
        xlab(label="Maximum Sufficiency Inclusion Score")+
        ylab(label="Minimum Sufficiency Inclusion Score")+
        guides(col=guide_legend(keyheight=2))+facet_wrap(~n.cut, ncol=2)
    }else if(plot.legend=="none"){
      plot<-ggplot(data=results, aes(x=incl.cut0, y=incl.cut1))+
        geom_point(aes(color=config.id))+scale_color_hue(c=150, l=60, name="Configuration ID")+
        xlim(0,1)+ylim(0,1)+
        xlab(label="Maximum Sufficiency Inclusion Score")+
        ylab(label="Minimum Sufficiency Inclusion Score")+
        guides(col=F)+facet_wrap(~n.cut, ncol=2)   
    }
    return(list("plot"=plot, "results"=results[,-4], "legend"=legend))
  } else{
    return(list("results"=results[,-4], "legend"=legend))  
  }
}