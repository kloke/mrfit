\name{interaction.test.mrfit}
\alias{interaction.test.mrfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Robust rank-based test of interaction between treatment and center.
}
\description{
Robust rank-based test of interaction between treatment and center.
}
\usage{
interaction.test.mrfit(x, y, block)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{n by p design matrix }
  \item{y}{n by 1 response vector }
  \item{block}{n by 1 vector denoting block membership}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\item{Fstat}{F test statistic}
\item{Fpval}{p-value associated with F statistic}
\item{Tstat}{Chi-sq test statistic}
\item{Tpval}{p-value associated with Chi-sq statistic}
}
\references{
Rashid, M.M., McKean, J.W., Kloke, J.D. (2011).  R Estimates and Associated Inferences for Mixed Models with Covariates in a Multi-Center Clinical Trial.  \emph{Statistics in Biopharmaceutical Research}.
}
\author{
John Kloke \email{kloke@biostat.wisc.edu}
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, y, block) 
{
    ub <- unique(block)
    m <- length(ub)
    p <- ncol(x)
    bigbetahat <- vector(mode = "numeric", length = m * p)
    tauhat <- vector(mode = "numeric", length = m)
    one <- rep(1, m - 1)
    Ip <- diag(rep(1, p))
    H1 <- one \%x\% Ip
    H2 <- -diag(1 * one) \%x\% Ip
    H <- cbind(H1, H2)
    V <- matrix(0, nrow = m * p, ncol = m * p)
    min.n <- sum(block == ub[1])
    for (i in 1:m) {
        yi <- y[block == ub[i]]
        xi <- x[block == ub[i], ]
        xi <- xi - outer(rep(1, nrow(xi)), apply(xi, 2, mean))
        if (length(yi) < min.n) {
            min.n <- length(yi)
        }
        fit <- rfit(xi, yi)
        bigbetahat[((i - 1) * p + 1):(i * p)] <- fit$coef
        V[((i - 1) * p + 1):(i * p), ((i - 1) * p + 1):(i * p)] <- solve(crossprod(xi))
    }
    tauhat <- mrfit(x, y, block)$tauhat
    q <- nrow(H)
    HB <- H \%*\% bigbetahat
    F <- t(HB) \%*\% solve(H \%*\% V \%*\% t(H)) \%*\% HB/(q * tauhat^2)
    T <- q * F
    list(Fstat = F, Fpval = 1 - pf(F, q, min.n - p), Tstat = T, 
        Tpval = 1 - pchisq(T, q))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{robust}
\keyword{nonparametric }
