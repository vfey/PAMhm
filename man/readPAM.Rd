\name{readPAM}
\alias{readPAM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Read various input file types into a list of data.frames similar to 'read2list'
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
readPAM(file.name, path = ".", nsheets = 1, dec = ".", header = T, na.strings = "NA")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file.name}{
%%     ~~Describe \code{file.name} here~~
}
  \item{path}{
%%     ~~Describe \code{path} here~~
}
  \item{nsheets}{
%%     ~~Describe \code{nsheets} here~~
}
  \item{dec}{
%%     ~~Describe \code{dec} here~~
}
  \item{header}{
%%     ~~Describe \code{header} here~~
}
  \item{na.strings}{
%%     ~~Describe \code{na.strings} here~~
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
function (file.name, path = ".", nsheets = 1, dec = ".", header = T,
    na.strings = "NA")
{
    fl <- file.path(path, file.name)
    if (!file.exists(fl))
        stop("File not found!")
    f.ext <- sub(".+(.)([[:alpha:]]{3})", "\\1\\2", file.name)
    if (length(grep("xls", f.ext)) > 0) {
        require(gdata)
        xls <- vector(mode = "list", length = nsheets)
        for (i in 1:nsheets) {
            xls[[i]] <- read.xls(fl, sheet = i, header = header,
                dec = dec, stringsAsFactor = F, comment.char = "",
                na.strings = c(na.strings, paste(na.strings,
                  " ", sep = ""), "#N/A!", "#N/A! "))
        }
    }
    else if (length(grep("txt|tsv|csv", f.ext)) > 0) {
        nsheets <- 1
        ve <- c(",", ";", "\t")
        se <- which(lapply(ve, function(x) grep(x, readLines(fl,
            n = 1))) == 1)
        sep <- ve[se]
        xls <- list(read.table(fl, header = header, dec = dec,
            stringsAsFactor = FALSE, sep = sep, comment.char = "",
            na.strings = na.strings))
    }
    else if (length(grep("xdr", f.ext)) > 0) {
        require(R.utils)
        xls <- loadObject(fl)
    }
    else if (length(grep("RData", f.ext)) > 0) {
        l <- ls()
        load(fl)
        nl <- ls()
        if (length(nl) == length(l) + 2) {
            xls <- get(nl[-c(which(nl == "l"), match(l, nl))])
        }
        else {
            stop("Image file must contain only one (1) object!")
        }
    }
    else {
        stop("Please provide either an Excel document or a tab-, comma- or semi-colon-delimited text file or an R image file")
    }
    return(xls)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
