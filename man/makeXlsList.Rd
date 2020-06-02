\name{makeXlsList}
\alias{makeXlsList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Wrapper around 'centerMatrix' to extract numeric data from input tables before transforming
}
\description{
  Internal function not to be called by the user directly.
}
\usage{
makeXlsList(dat, nsheets = 1, startcol = 2, symbolcol = 1, medianCenter = NULL, log = FALSE, do.log = FALSE, log.base = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{nsheets}{
%%     ~~Describe \code{nsheets} here~~
}
  \item{startcol}{
%%     ~~Describe \code{startcol} here~~
}
  \item{symbolcol}{
%%     ~~Describe \code{symbolcol} here~~
}
  \item{medianCenter}{
%%     ~~Describe \code{medianCenter} here~~
}
  \item{log}{
%%     ~~Describe \code{log} here~~
}
  \item{do.log}{
%%     ~~Describe \code{do.log} here~~
}
  \item{log.base}{
%%     ~~Describe \code{log.base} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dat, nsheets = 1, startcol = 2, symbolcol = 1, medianCenter = NULL,
    log = FALSE, do.log = FALSE, log.base = 2)
{
    xlslist <- lapply(dat, function(x) {
        endcol <- ncol(x)
        y <- as.matrix(x[, startcol:endcol])
        rownames(y) <- make.names(x[, symbolcol])
        colnames(y) <- make.names(colnames(y))
        if (any(is.na(y))) {
            rm.r <- which(apply(y, 1, function(z) all(is.na(z))))
            if (length(rm.r) > 0)
                y <- y[-rm.r, ]
        }
        centerMatrix(y, medianCenter = medianCenter, log = log,
            do.log = do.log, log.base = log.base)
    })
    return(xlslist)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
