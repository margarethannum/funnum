% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xtab_rowcol.R
\name{xtab_rowcol}
\alias{xtab_rowcol}
\title{Calculate row AND column crosstabulatd percent}
\usage{
xtab_rowcol(x, rowvar, colvar)
}
\arguments{
\item{x}{crosstabulated matrix (e.g. cluster assignments)}

\item{rowvar}{name of row variables}

\item{colvar}{name of column variables}
}
\description{
Useful in comparing cluster assignments
}
\examples{

x <- cbind( x1 = c(1, 0, 0, 4), x2 = c(0, 1, 5, 0), x3 = c(6, 3, 0, 0))
xtab_rowcol(x, "a", "b")
}
\author{
Margaret L. Hannum
}
