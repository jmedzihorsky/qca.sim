\name{QCA.sim.inclcut}
\alias{QCA.sim.inclcut}
\title{
QCA Sufficiency Inclusion Score Simulation
}
\description{
Returns QCA results for a given minimum frequency threshold across an arbitrarily large set of sufficiency inclusion scores
}
\usage{
QCA.sim.inclcut(data, outcome, conditions=NULL, 
    min.incl.cut, max.incl.cut, n.cut, reps, ...)
}
\arguments{
  \item{data}{an object of class 'data.frame'}
  \item{outcome}{a character string or column index indicating the outcome variable}
  \item{conditions}{optional character vector or vector of column indices indicating explanatory variables}
  \item{min.incl.cut}{numeric lower bound for sampling of sufficiency inclusion scores}
  \item{max.incl.cut}{numeric upper bound for sampling of sufficiency inclusion scores}
  \item{n.cut}{numeric minimum frequency threshold}
  \item{reps}{number of sufficiency inclusion score pairs to be sampled}
  \item{...}{optional arguments passed to \code{eqmcc()}}
}
\value{
A data frame containing combinations of sufficiency inclusion scores and QCA solutions
}
\author{Chris Krogslund, http://ckro.gs; ckrogslund@berkeley.edu}

\examples{
protest.data<-read.csv(file="http://philhoward.org/wp-content/
    uploads/2012/11/International-Studies-Review-Replication-Data.csv")
protest.data<-protest.data[,!colnames(protest.data) %in% "Country"]

QCA.sim.inclcut(data=protest.data, outcome="success", 
    min.incl.cut=0, max.incl.cut=1, n.cut=1, reps=100)

QCA.sim.inclcut(data=protest.data, outcome="success", 
    conditions=c("mobile", "fuel"), min.incl.cut=0, 
    max.incl.cut=1, n.cut=1, reps=100)

}
