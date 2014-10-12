\name{mrwilcoxontau}
\alias{mrwilcoxontau}
\title{
MR estimate of tau.
}
\description{
MR estimate of tau.
}
\usage{
mrwilcoxontau(x, y, center, beta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{n by p design matrix}
  \item{y}{n by 1 response vector}
  \item{center}{n by 1 vector denoting block (cluster) membership}
  \item{beta}{ p by 1 vector }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
\references{
Rashid, M.M., McKean, J.W., Kloke, J.D. (2011).  R Estimates and Associated Inferences for Mixed Models with Covariates in a Multi-Center Clinical Trial.  \emph{Statistics in Biopharmaceutical Research}.
}
\author{
John Kloke \email{kloke@biostat.wisc.edu}
}

\seealso{
\code{\link{mrdisp}}
}
\examples{
### Internal function ###
## The function is currently defined as
function (x, y, center, beta) 
{
    x <- as.matrix(x)
    a <- unique(center)
    nc <- length(a)
    p <- ncol(x)
    tauhats <- rep(0, nc)
    for (i in 1:nc) {
        x1 <- as.matrix(x[center == a[i], ])
        y1 <- y[center == a[i]]
        tauhats[i] <- rashidtau(y1 - x1 \%*\% beta, p)
    }
    mean(tauhats)
  }
}
