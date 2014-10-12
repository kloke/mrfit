\name{print.mrfit}
\alias{print.mrfit}
\alias{print.summary.mrfit}
\title{mrfit Internal Print Functions}
\description{
These functions print the output in a user-friendly manner using the internal R function \code{print}.
}
\usage{
\method{print}{mrfit}(x, ...)
\method{print}{summary.mrfit}(x, digits = max(5, .Options$digits - 2), ...)
}
\arguments{
  \item{x}{ An object to be printed }
  \item{digits}{number of digits to display}
  \item{\dots}{  additional arguments to be passed to \code{print} }
}
\author{ John Kloke \email{kloke@biostat.wisc.edu}}

\seealso{
\code{\link{mrfit}}, \code{\link{summary.mrfit}}
}
