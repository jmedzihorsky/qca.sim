\name{QCA.random}
\alias{QCA.random}
\title{
QCA Sufficiency Inclusion Score & Minimum Frequency Threshold Simulation with Random Variables
}
\description{
Returns QCA results for a range of minimum frequency thresholds across an arbitrarily large set of sufficiency inclusion scores when a random variable is repeatedly added to the dataset
}
\usage{
QCA.random(n, type, data, outcome, conditions=NULL, 
    min.incl.cut, max.incl.cut, min.n.cut, 
    max.n.cut, reps, plot, plot.legend, ...)
}
\arguments{
  \item{n}{number of times to simulate random variable inclusion}
  \item{type}{type of random variabe to be included; either "binary", which samples from 0 and 1, or "uniform", which draws from a uniform distribution bound by 0 and 1}
  \item{data}{an object of class 'data.frame'}
  \item{outcome}{a character string or column index indicating the outcome variable}
  \item{conditions}{optional character vector or vector of column indices indicating explanatory variables}
  \item{min.incl.cut}{numeric lower bound for sampling of sufficiency inclusion scores}
  \item{max.incl.cut}{numeric upper bound for sampling of sufficiency inclusion scores}
  \item{min.n.cut}{numeric lower bound for minimum frequency thresholds}
  \item{max.n.cut}{numeric upper bound for minimum frequency thresholds}  
  \item{reps}{number of sufficiency inclusion score pairs to be sampled}
  \item{plot}{if TRUE, plot solutions}
  \item{plot.legend}{"solutions" indicates plot legend should contain actual unique solutions; "ids" indicates plot should contain numeric identifiers for unique solutions; "none" indicates plot should not contain a legend}
  \item{...}{optional arguments passed to \code{eqmcc()}}
}
\value{
\item{plot}{plot of QCA results for given sufficiency inclusion score pairs and minimum frequency thresholds}
\item{results}{data frame containing combinations of sufficiency inclusion scores and QCA solutions}
\item{legend}{list containing unique solutions (config) and their numeric identifiers (config.id)}
}
\author{Chris Krogslund, http://ckro.gs; ckrogslund@berkeley.edu}

\examples{
protest.data<-read.csv(file="http://philhoward.org/wp-content/
    uploads/2012/11/International-Studies-Review-Replication-Data.csv")
protest.data<-protest.data[,!colnames(protest.data) %in% "Country"]

QCA.random(n=3, type="uniform", data=protest.data, outcome="success", 
    min.incl.cut=0, max.incl.cut=1, min.n.cut=1, 
    max.n.cut=4, reps=100, plot=TRUE, plot.legend="ids")
    
QCA.random(n=5, type="uniform", data=protest.data, outcome="success", 
    conditions=c("mobile", "fuel"), min.incl.cut=0, 
    max.incl.cut=1, min.n.cut=1, max.n.cut=4, reps=100, 
    plot=TRUE, plot.legend="ids")
}
