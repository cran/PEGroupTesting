\name{p.mle}
\alias{p.mle}
\title{
Maximum likelihood estimate for population proportion by group testing
}
\description{
This function estimates the population proportion by group testing using maximum likelihood method. It is for both equal and unequal group size.
}
\usage{
p.mle(obs)
}

\arguments{
  \item{obs}{
A three-column matrix containing all the data information. Column 1 is the number of groups. Column 2 is group size. Column 3 is number of positives. Different rows corresponds to different group sizes. 
}
}

\value{
The maximum likelihood estimate for population proportion by group testing
}

\references{
Ayung, J. (2003) Tutorial on maximum likelihood estimation. Journal of Mathematical Psychology. 47(1).
<http://www.sciencedirect.com/science/article/pii/S0022249602000287>
 
}
\author{
Qingyang Zhang, Yanchuan Li
}


\examples{
library(PEGroupTesting)
mydata=matrix(c(50,10,40,40,8,37),nrow=2,byrow=TRUE)
p.mle(mydata)
}
\keyword{Maximum likelihood estimate}
