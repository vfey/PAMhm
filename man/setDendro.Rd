\name{setDendro}
\alias{setDendro}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Set dendrogram style for use in 'heatmap.n'
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
setDendro(dendrograms)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dendrograms}{
%%     ~~Describe \code{dendrograms} here~~
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
function (dendrograms)
{
    if (dendrograms == "Both") {
        reorder <- c(TRUE, TRUE)
    }
    else if (dendrograms == "Vertical") {
        reorder <- c(TRUE, FALSE)
    }
    else if (dendrograms == "Horizontal") {
        reorder <- c(FALSE, TRUE)
    }
    else if (dendrograms == "None") {
        reorder <- c(FALSE, FALSE)
    }
    return(reorder)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
