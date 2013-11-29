\docType{package}
\name{applyBy-package}
\alias{applyBy-package}
\title{Subset-wise apply functions to aggregate rows or columns of matrix-like objects}
\description{
  This package defines a function applyBy that enables
  applying a function along margins of matrix-like objects
  separately within groups of columns/rows defined by
  factor(s), e.g., computing aggregated summary statistics.
  It is essentially a wrapper around functions from the the
  matrixStats package, which provide very fast alternative
  implementation to the base aggregate function.
}
\details{
  \tabular{ll}{ Package: \tab applyBy\cr Type: \tab
  Package\cr Version: \tab 1.0\cr Date: \tab 2013-11-29\cr
  License: \tab GPL (>= 2)\cr LazyLoad: \tab yes\cr }
}
\examples{
x <- matrix(runif(32), ncol = 4)
g.row <- gl(2, 4)
g.col <- gl(2, 2)

rowMeans(x)
rowMeansBy(x, g.col)
colMeansBy(x, g.row)
}
\author{
  Renaud Gaujoux \email{renaud@tx.technion.ac.il}

  Maintainer: Renaud Gaujoux
  \email{renaud@tx.technion.ac.il}
}
\seealso{
  \code{\link{matrixStats-package}},
  \code{\link{colAvgsPerRowSet}},
  \code{\link{rowAvgsPerColSet}} , \code{\link{applyBy}}
}
\keyword{package}

