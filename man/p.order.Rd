\name{p.order}
\alias{p.order}
\title{
Order statistics estimate for population proportion using group testing
}
\description{
This function estimates the population proportion using order statistics method (Ding and Xiong 2016). It is for equal group size only.
}
\usage{
p.order(obs)
}

\arguments{
  \item{obs}{
A 1 by 3 matrix containing the data information. Column 1 is the number of groups. Column 2 is group size. Column 3 is number of positives.

}
}

\value{
Order statistics estimate for population proportion in group testing
}
\references{
Ding, J. and Xiong, W. (2016) A new estimator for a population proportion using group testing. Communication in Statistics-Simulation and Computation. 45(101-114) 
<http://dx.doi.org/10.1080/03610918.2013.854909>
}
\author{
Qingyang Zhang, Yanchuan Li
}


\examples{
library(PEGroupTesting)
mydata=matrix(c(50,10,45),nrow=1,byrow=TRUE)
p.order(mydata)
}

\keyword{group testing}
\keyword{order statistics}
