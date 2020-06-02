\name{setCol}
\alias{setCol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Set heatmap color scheme for use in 'heatmap.n'
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
setCol(cols)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cols}{
%%     ~~Describe \code{cols} here~~
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
function (cols)
{
    if (cols == "BlueWhiteRed") {
        colrs <- colorRampPalette(c("darkblue", "white", "red"))(30)
    }
    if (cols == "RedBlackGreen") {
        colrs <- hmcols()
    }
    if (cols == "PurpleWhiteGreen") {
        colrs <- brewer.pal(11, "PRGn")
    }
    if (cols == "OrangeWhitePurple") {
        colrs <- brewer.pal(11, "PuOr")
    }
    if (cols == "RedWhiteBlue") {
        colrs <- brewer.pal(11, "RdBu")
    }
    if (cols == "BrownWhiteTurquoise") {
        colrs <- brewer.pal(11, "BrBG")
    }
    return(colrs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
