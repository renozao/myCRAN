\name{applyBy}
\alias{applyBy}
\alias{applyBy.ExpressionSet}
\alias{applyBy.matrix}
\alias{applyBy.numeric}
\alias{colApplyBy}
\alias{colMaxsBy}
\alias{colMeansBy}
\alias{colMediansBy}
\alias{colMinsBy}
\alias{colSumsBy}
\alias{rowApplyBy}
\alias{rowMaxsBy}
\alias{rowMeansBy}
\alias{rowMediansBy}
\alias{rowMinsBy}
\alias{rowSumsBy}
\title{Group Apply}
\usage{
  applyBy(x, ...)

  \method{applyBy}{matrix} (x, BY, MARGIN, FUN, W = NULL,
    ..., DROP = FALSE)

  \method{applyBy}{ExpressionSet} (x, BY, MARGIN, ...,
    ANNOTATIONS = TRUE)

  \method{applyBy}{numeric} (x, BY, MARGIN, ...)

  rowApplyBy(x, BY, FUN, ...)

  colApplyBy(x, BY, FUN, ...)

  colSumsBy(x, BY, ...)

  rowSumsBy(x, BY, ...)

  rowMeansBy(x, BY, ...)

  colMeansBy(x, BY, ...)

  rowMediansBy(x, BY, ...)

  colMediansBy(x, BY, ...)

  rowMaxsBy(x, BY, ...)

  colMaxsBy(x, BY, ...)

  rowMinsBy(x, BY, ...)

  colMinsBy(x, BY, ...)
}
\arguments{
  \item{x}{matrix-like object on which \code{\link{apply}}
  can be called.}

  \item{BY}{factor or object coerced to a factor, that
  defines the groups within which the function \code{FUN}
  is applied.

  If \code{x} is an ExpressionSet object, then \code{BY}
  can be the names of a sample (resp. feature) annotation
  variable if \code{MARGIN=1} (resp. \code{MARGIN=2L}) (see
  examples).}

  \item{MARGIN}{margin along which the function \code{FUN}
  is applied: 1L for rows, 2L for columns.}

  \item{FUN}{function to apply to each sub-matrix that
  contains the rows/columns defined by each level of
  argument \code{BY}. It must be a function that takes a
  matrix as its first argument and returns a vector of
  length the dimension of margin \code{MARGIN} of
  \code{x}.}

  \item{...}{extra parameters passed to \code{FUN}.}

  \item{DROP}{logical that indicates if absent levels
  should be removed from the result matrix, or appear as
  0-filled rows/columns.

  If \code{BY} is a list of character indexes, then
  \code{DROP=FALSE} will complete the list with any missing
  index, so that the corresponding rows/columns form
  singletons. This means that these data will be passed as
  row/column vectors to the aggregation function
  \code{FUN}.}

  \item{ANNOTATIONS}{logical that indicates if
  samples/feature annotations should be kept, when the
  input data is an \code{\link[Biobase]{ExpressionSet}}
  object. Currently, if \code{TRUE}: \itemize{ \item if
  code{MARGIN=1L}, then feature annotations are kept
  unchanged, and phenotypic sample annotations are
  discarded. \item if code{MARGIN=2L}, then phenotypic
  sample annotations are kept unchanged, and feature
  annotations are discarded. }

  In any case, the value of slot \code{annotation} (i.e.
  the annotation package), is passed on to the result
  object.}

  \item{W}{An optional \code{\link[base]{numeric}} NxM
  \code{\link[base]{matrix}} of weights.}
}
\value{
  The result is a matrix or an \code{ExpressionSet} object
  whose margin's dimension \code{MARGIN} is equal the same
  margin's dimension in \code{x}, and the other to the
  number of levels in \code{BY}.
}
\description{
  \code{appplyBy} is an S3 generic function that applies a
  given function to sub-matrices of a matrix-like object,
  which are generated according to a factor that defines
  groups rows or columns.

  \code{applyBy.matrix} is a wrapper around
  \code{\link[matrixStats]{colAvgsPerRowSet}}, which make
  the computation really fast, but requires somehow
  cumbersome matrix specifications for the groups of
  columns or rows. The wrapper builds the arguments for the
  particular case where the groups are defined by a factor.

  A method is provided for
  \code{\link[Biobase]{ExpressionSet}} objects, which
  preserve sample and feature annotations. Moreover it
  allows directly passing names of feature/sample
  annotation -- factor -- variables in argument \code{BY}
  (see examples).

  \code{rowApplyBy} applies a function to rows of
  sub-matrices whose columns are defined by a factor.

  \code{rowApplyBy} applies a function to columns of
  sub-matrices whose rows are defined by a factor.

  \code{col<STAT>By} computes for each column a given
  statistic within separate groups of rows, which are
  defined by a factor.

  \code{row<STAT>By} computes for each row a given
  statistic within separate groups of columns, which are
  defined by a factor.
}
\examples{
#----------
# applyBy.matrix
#----------
# random data matrix
x <- matrix(runif(12 * 6), 12, 6)

# by groups of columns
fc <- gl(2, 3)
b <- applyBy(x, fc, 1L, rowSums)
b
# or
balt <- rowApplyBy(x, fc, rowSums)
identical(b, balt)

# by groups of rows
fr <- gl(3, 4)
b <- applyBy(x, fr, 2L, colSums)
# or
balt <- colApplyBy(x, fr, colSums)
identical(b, balt)

#----------
# applyBy.ExpressionSet
#----------
## Method for apply directly to ExpressionSet objects

library(Biobase)
x <- ExpressionSet(x, annotation='abcd.db')
y <- rowMinsBy(x, fc)
y <- colMinsBy(x, fr)

## annotations are conserved/collapsed
pData(x) <- data.frame(Group=fc, Sample=letters[1:ncol(x)])
pData(x)
fData(x) <- data.frame(ENTREZID=fr, Gene=letters[nrow(x):1])
fData(x)

# keep feature annotations, collapse sample annotations
y <- rowMinsBy(x, 'Group')
pData(y)
fData(y)

# keep sample annotations, collapse feature annotations
y <- colMinsBy(x, 'ENTREZID')
pData(y)
fData(y)

#----------
# applyBy.numeric
#----------
x <- runif(12)
# 3 groups of 4 elements
g <- gl(3, 4)
g

# row means ~> row matrix with 3 columns
a <- rowMeansBy(x, g)
a
# col means ~> column matrix with 3 rows
b <- colMeansBy(x, g)
b

# values are identical
stopifnot( identical(as.numeric(a), as.numeric(b)) )
}

