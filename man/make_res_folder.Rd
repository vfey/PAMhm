\name{make_res_folder}
\alias{make_res_folder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Generate unique results folder
}
\description{
  Internal function not to be called by the user directly.
}
\usage{
make_res_folder(folder.name, makeFolder = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{folder.name}{
%%     ~~Describe \code{folder.name} here~~
}
  \item{makeFolder}{
%%     ~~Describe \code{makeFolder} here~~
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
function (folder.name, makeFolder = TRUE)
{
    if (makeFolder) {
        res.dir <- paste("Results", gsub("-", "", unlist(strsplit(as.character(Sys.time()),
            " "))[1]), gsub(":", "", unlist(strsplit(as.character(Sys.time()),
            " "))[2]), sep = "_")
        dir.create(path = file.path(folder.name, res.dir, sep = ""))
        resultsFolder <- file.path(folder.name, res.dir, sep = "")
    }
    else {
        resultsFolder <- folder.name
    }
    return(resultsFolder)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
