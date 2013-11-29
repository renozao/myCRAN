# Project: applyBy
# 
# Author: Renaud Gaujoux
# Created: Nov 29, 2013
###############################################################################


#' Subset-wise apply functions to aggregate rows or columns of matrix-like objects
#' 
#' This package defines a function applyBy that enables applying a 
#' function along margins of matrix-like objects separately within groups of
#' columns/rows defined by factor(s), e.g., computing aggregated summary
#' statistics. It is essentially a wrapper around functions from the the
#' matrixStats package, which provide very fast alternative implementation to 
#' the base aggregate function.
#' 
#' \tabular{ll}{
#' Package: \tab applyBy\cr
#' Type: \tab Package\cr
#' Version: \tab 1.0\cr
#' Date: \tab 2013-11-29\cr
#' License: \tab GPL (>= 2)\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' @author
#' Renaud Gaujoux \email{renaud@@tx.technion.ac.il}
#'
#' Maintainer: Renaud Gaujoux \email{renaud@@tx.technion.ac.il}
#' @name applyBy-package
#' @docType package
#' @keywords package
#' 
#' @seealso \code{\link{matrixStats-package}}, \code{\link{colAvgsPerRowSet}}, \code{\link{rowAvgsPerColSet}}
#' , \code{\link{applyBy}}
#' 
#' @examples
#' x <- matrix(runif(32), ncol = 4)
#' g.row <- gl(2, 4)
#' g.col <- gl(2, 2) 
#' 
#' rowMeans(x)
#' rowMeansBy(x, g.col)
#' colMeansBy(x, g.row) 
NULL

