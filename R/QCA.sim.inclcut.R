QCA.sim.inclcut<-function(data, outcome, conditions, min.incl.cut, max.incl.cut, n.cut, reps, ...){
  
  library(QCA)
  library(plyr)
  
  # Check argument classes
  if(!is.data.frame(data)){
    stop("'data' object must be of class 'data.frame")
  }
  if(!is.character(outcome) & !is.numeric(outcome)){
    stop("'outcome' argument must be either a character vector or a vector of column indices")
  }
  if(!is.numeric(min.incl.cut) | !is.numeric(max.incl.cut) | !is.numeric(n.cut) | !is.numeric(reps)){
    stop("'min.incl.cut', 'max.incl.cut', 'n.cut', and 'reps' arguments must be of class 'numeric'")
  }
  if(min.incl.cut<0 | min.incl.cut>1 | max.incl.cut<0 | max.incl.cut>1){
    stop("'min.incl.cut' and 'max.incl.cut' arguments must be between 0 and 1")
  }
  if(n.cut<1){
    stop("'n.cut' argument must be greater than or equal to 1")
  }
  if(reps<1){
    stop("'reps' argument must be greater than or equal to 1")
  }
  
  # Create incl.cut value pairs
  incl.cut1.val<-runif(n=3*reps, min=min.incl.cut, max=max.incl.cut)
  incl.cut0.val<-runif(n=3*reps, min=0, max=max.incl.cut)
  n.cut.val<-rep(x=n.cut, times=3*reps)
  vals<-cbind(incl.cut1.val, incl.cut0.val, n.cut.val)
  vals<-vals[incl.cut1.val>=incl.cut0.val,]
  vals<-vals[sample(x=1:nrow(vals), size=reps, replace=FALSE),]
  vals<-cbind(vals, progress=1:reps/reps*100)
  
  # Prepare data object for analysis
  if(is.numeric(outcome)){
    outcome<-colnames(test)[outcome]
  }
  if(!missing(conditions)){
    if(!is.character(conditions) & !is.numeric(conditions)){
      stop("'conditions' argument must be either a character vector or a vector of column indices")
    }
    if(is.numeric(conditions)){
      conditions<-colnames(test)[conditions]
    }
    data<-data[,colnames(data) %in% c(outcome, conditions)]
  }
  
  # Apply the error handling function
  out<-apply(X=vals, MARGIN=1, FUN=function(x){
    cat("Percent complete: n.cut = ", x[3], ", ", x[4], "%\n", sep="")
    out<-tryCatch(expr=eqmcc(data=data,outcome=outcome,incl.cut1=x[1],incl.cut0=x[2],n.cut=x[3], ...)$solution, error=function(e) NA)
    as.data.frame(t(do.call("rbind", lapply(X=out, paste, collapse="+"))))   
  })
  
  # Combine and clean results (removing progress indicator)
  results<-data.frame(vals[, -4], rbind.fill(out))
  colnames(results)[1:3]<-c("incl.cut1", "incl.cut0", "n.cut")
  colnames(results)[4:length(colnames(results))]<-paste("Solution", 1:length(4:length(colnames(results))))
  
  return(results)
}