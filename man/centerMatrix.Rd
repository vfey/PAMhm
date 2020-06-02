\name{centerMatrix}
\alias{centerMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Perform median centring
}
\description{
  Internal function not to be called by the user directly.
}
\usage{
centerMatrix(xls, medianCenter = NULL, log = FALSE, do.log = FALSE, log.base = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xls}{
%%     ~~Describe \code{xls} here~~
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
function (xls, medianCenter = NULL, log = FALSE, do.log = FALSE,
    log.base = 2)
{
    if (missing(xls))
        stop("No data input")
    if (is.character(log.base)) {
        if (!is.na(as.integer(log.base))) {
            log.base <- as.integer(log.base)
        }
        else if (log.base == "e") {
            log.base <- exp(1)
        }
        else {
            stop("Inappropriate log-base...")
        }
    }
    if (is.null(medianCenter)) {
        if (do.log) {
            if (!log) {
                xls <- log(xls, base = log.base)
            }
            else {
                xls <- log.base^xls
            }
        }
    }
    else if (medianCenter == "grand") {
        if (log) {
            xls <- xls - median(xls, na.rm = TRUE)
        }
        else {
            xls <- log(xls/median(xls, na.rm = TRUE), base = log.base)
        }
    }
    else if (medianCenter == "row") {
        if (log) {
            mr <- apply(xls, 1, median, na.rm = TRUE)
            xls <- sweep(xls, 1, mr)
        }
        else {
            mr <- apply(log(xls, base = log.base), 1, median,
                na.rm = TRUE)
            xls <- sweep(log(xls, base = log.base), 1, mr)
        }
    }
    else if (medianCenter == "column") {
        if (log) {
            mr <- apply(xls, 2, median, na.rm = TRUE)
            xls <- sweep(xls, 2, mr)
        }
        else {
            mr <- apply(log(xls, base = log.base), 2, median,
                na.rm = TRUE)
            xls <- sweep(log(xls, base = log.base), 2, mr)
        }
    }
    return(xls)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
