\name{get.qca.datasets}
\alias{get.qca.datasets}
\title{
Get QCA Datasets
}
\description{
Automatically downloads raw and calibrated QCA datasets hosted on the COMPASSS.org server.  Can select between csQCA, mvQCA, and fsQCA.  Returns datasets as a list, along with hyperlink selections indicating the author name, year, and dataset type (raw versus calibrated).
}
\usage{
get.qca.datasets(csQCA = FALSE, mvQCA = FALSE, 
    fsQCA = FALSE)
}
\arguments{
  \item{csQCA}{
if TRUE, get csQCA datasets
}
\item{mvQCA}{
if TRUE, get mvQCA datasets
}
\item{fsQCA}{
if TRUE, get fsQCA datasets
}
}

\value{eturns a named list with up to three nested elements, one for each of csQCA, mvQCA, and fsQCA (corresponding to argument values supplied by the user).  Within each nest, there is a list whose items have one sub-item containing a hyperlink stub and one sub-item containing the corresponding dataset.  
}

\author{
Chris Krogslund (http://ckro.gs; ckrogslund@berkeley.edu)
}

\examples{
# Get all csQCA datasets
get.qca.datasets(csQCA=TRUE)

# Get all csQCA and mvQCA datasets
get.qca.datasets(csQCA=TRUE, mvQCA=TRUE)
}

# Get all csQCA, mvQCA, and fsQCA datasets
get.qca.datasets(csQCA=TRUE, mvQCA=TRUE, fsQCA=TRUE)
}

\keyword{ ~QCA }
