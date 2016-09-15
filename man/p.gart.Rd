\name{p.gart}
\alias{p.gart}
\title{
Gart's estimate for population proportion by group testing
}
\description{
The function estimates the population proportion by group testing using Gart's method. It is for both equal and unequal group size.
}
\usage{
p.gart(obs)
}

\arguments{
  \item{obs}{
A three-column matrix containing all the data information. Column 1 is the number of groups. Column 2 is group size. Column 3 is number of positives. Different rows corresponds to different group sizes. 
}
}

\value{
Gart's estimate for population proportion by group testing
}
\references{
Gary, J. (1991) An application of score methodology: confidence intervals and tests of fit for one-hit curves. Handbook of Statistics, 8(395-406), Amsterdam Elsevier. <https://www.elsevier.com/books/book-series/handbook-of-statistics>
}
\author{
Qingyang Zhang, Yanchuan Li
}


\examples{
library(PEGroupTesting)
mydata=matrix(c(50,10,40,40,8,37),nrow=2,byrow=TRUE)
p.gart(mydata)
}

\keyword{Gart's estimate}