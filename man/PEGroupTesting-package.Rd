\name{PEGroupTesting-package}
\alias{PEGroupTesting-package}
\alias{PEGroupTesting}
\docType{package}
\title{
\packageTitle{PEGroupTesting}
}
\description{
\packageDescription{PEGroupTesting}
}

\author{
\packageAuthor{PEGroupTesting}

Maintainer: \packageMaintainer{PEGroupTesting}
}
\references{
Ayung, J. (2003) Tutorial on maximum likelihood estimation. Journal of Mathematical Psychology. 47(1).
<http://www.sciencedirect.com/science/article/pii/S0022249602000287>

Ding, J. and Xiong, W. (2016) A new estimator for a population proportion using group testing. Communication in Statistics-Simulation and Computation. 45(101-114) 
<http://dx.doi.org/10.1080/03610918.2013.854909>

Gart, J. (1991) An application of score methodology: confidence intervals and tests of fit for one-hit curves. Handbook of Statistics, 8(395-406), Amsterdam Elsevier. <https://www.elsevier.com/books/book-series/handbook-of-statistics>

Burrows, P. (1987) Improved estimation of pathogen transmission rates by group testing. Phytopathology. 77(363-365) 
<https://www.apsnet.org/publications/phytopathology/backissues/Documents/1987Articles>
}

\keyword{Group testing, maximum likelihood estimate, order statistics, Burrow's method, Gart's method}

\examples{
library(PEGroupTesting)
mydata=matrix(c(50,10,45),nrow=1,ncol=3,byrow=TRUE)
p.mle(mydata)
p.burrow(mydata)
p.gart(mydata)
p.order(mydata)
}
