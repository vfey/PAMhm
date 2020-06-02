\name{plotAdjust}
\alias{plotAdjust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Automatically calculate various plotting parameters to adjust the pdf/image size and magnification of the label text for
  a well readable output.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotAdjust(dm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dm}{
%%     ~~Describe \code{dm} here~~
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
function (dm)
{
    adj.l <- vector(mode = "list")
    adj.l$r.cex <- round(1.05/log(sqrt(nrow(dm))), 1)
    if (nrow(dm) <= 10) {
        adj.l$r.cex <- 0.8
    }
    adj.l$c.cex <- round(1.4/log(sqrt(ncol(dm))), 1)
    if (ncol(dm) <= 10) {
        adj.l$c.cex <- 0.8
    }
    adj.l$pdf.width <- ceiling(sqrt(log(ncol(dm))) * ceiling(sqrt(ncol(dm)))) *
        1.6
    adj.l$pdf.height <- ceiling(sqrt(nrow(dm)))
    if (adj.l$pdf.height < 5) {
        adj.l$pdf.height <- 5
    }
    mcc <- max(nchar(colnames(dm)))
    adj.l$labelheight <- round(mcc/100/1.15, 2)/ceiling(adj.l$pdf.height/9)
    mc <- max(nchar(rownames(dm)))
    adj.l$labelwidth <- round(log10(sqrt(mc))/(sqrt(mc) * mc^(-mc/100)),
        2) * 1.2 * adj.l$r.cex
    return(adj.l)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
