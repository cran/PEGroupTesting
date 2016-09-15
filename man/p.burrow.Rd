\name{p.burrow}
\alias{p.burrow}
\title{
Burrow's estimate for population proportion using group testing
}
\description{
This function estimates population proportion by group testing using Burrow's method. It is for equal group size only.
}
\usage{
p.burrow(obs)
}

\arguments{
  \item{obs}{
A 1 by 3 matrix containing the data information. Column 1 is the number of groups. Column 2 is group size. Column 3 is number of positives.
}
}

\value{
Burrow's estimate for population proportion by group testing.
}
\references{
Burrows, P. (1987) Improved estimation of pathogen transmission rates by group testing. Phytopathology. 77(363-365) 
<https://www.apsnet.org/publications/phytopathology/backissues/Documents/1987Articles>
}
\author{
Qingyang Zhang, Yanchuan Li
}

\examples{
library(PEGroupTesting)
mydata=matrix(c(50,10,45),nrow=1,byrow=TRUE)
p.burrow(mydata)
}

\keyword{Burrow's estimate}
