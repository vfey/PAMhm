\name{clustNum}
\alias{clustNum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Convert character input giving the desired number of clusters to object of class numeric
}
\description{
  Internal function not to be called by the user directly.
}
\usage{
clustNum(cluster.number)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cluster.number}{
%%     ~~Describe \code{cluster.number} here~~
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
function (cluster.number)
{
    num1 <- unlist(strsplit(cluster.number, ","))
    clusternum <- c(lapply(num1, function(x) {
        y <- unlist(strsplit(x, "-"))
        if (length(y) == 2) {
            y <- y[1]:y[2]
        }
        y <- as.numeric(y)
        y
    }), recursive = T)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
