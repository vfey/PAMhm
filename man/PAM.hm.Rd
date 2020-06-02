\name{PAM.hm}
\alias{PAM.hm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Old top-level wrapper function including the 'readPAM' function for use at the console
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PAM.hm(x, folder.name = ".", nsheets = 1, dec = ".", header = TRUE, symbolcol = 1, startcol = 2, sample.names = NULL, cluster.number = as.character(4), trim = -1, cols = "bwr", dendrograms = "Both", autoadj = TRUE, pdf.height = 10, pdf.width = 10, labelheight = 0.25, labelwidth = 0.2, r.cex = 0.5, c.cex = 1, medianCenter = NULL, log = FALSE, do.log = FALSE, log.base = 2, metric = "manhatten", na.strings = "NA", makeFolder = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{folder.name}{
%%     ~~Describe \code{folder.name} here~~
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
  \item{symbolcol}{
%%     ~~Describe \code{symbolcol} here~~
}
  \item{startcol}{
%%     ~~Describe \code{startcol} here~~
}
  \item{sample.names}{
%%     ~~Describe \code{sample.names} here~~
}
  \item{cluster.number}{
%%     ~~Describe \code{cluster.number} here~~
}
  \item{trim}{
%%     ~~Describe \code{trim} here~~
}
  \item{cols}{
%%     ~~Describe \code{cols} here~~
}
  \item{dendrograms}{
%%     ~~Describe \code{dendrograms} here~~
}
  \item{autoadj}{
%%     ~~Describe \code{autoadj} here~~
}
  \item{pdf.height}{
%%     ~~Describe \code{pdf.height} here~~
}
  \item{pdf.width}{
%%     ~~Describe \code{pdf.width} here~~
}
  \item{labelheight}{
%%     ~~Describe \code{labelheight} here~~
}
  \item{labelwidth}{
%%     ~~Describe \code{labelwidth} here~~
}
  \item{r.cex}{
%%     ~~Describe \code{r.cex} here~~
}
  \item{c.cex}{
%%     ~~Describe \code{c.cex} here~~
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
  \item{metric}{
%%     ~~Describe \code{metric} here~~
}
  \item{na.strings}{
%%     ~~Describe \code{na.strings} here~~
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
function (x, folder.name = ".", nsheets = 1, dec = ".", header = TRUE,
    symbolcol = 1, startcol = 2, sample.names = NULL, cluster.number = as.character(4),
    trim = -1, cols = "bwr", dendrograms = "Both", autoadj = TRUE,
    pdf.height = 10, pdf.width = 10, labelheight = 0.25, labelwidth = 0.2,
    r.cex = 0.5, c.cex = 1, medianCenter = NULL, log = FALSE,
    do.log = FALSE, log.base = 2, metric = "manhatten", na.strings = "NA",
    makeFolder = TRUE)
{
    resultsFolder <- make_res_folder(folder.name, makeFolder)
    if (is.null(sample.names)) {
        sample.names <- paste("Sample", LETTERS[1:nsheets])
    }
    plotCol <- setCol(cols)
    reorder <- setDendro(dendrograms)
    clusternum <- clustNum(cluster.number)
    if (is.character(x)) {
        dat <- readPAM(x, folder.name, nsheets = nsheets, dec = dec,
            header = header, na.strings = na.strings)
    }
    else {
        dat <- x
    }
    dat <- lapply(dat, rm.empty.cols)
    xlslist <- makeXlsList(dat, nsheets = nsheets, startcol = startcol,
        symbolcol = symbolcol, medianCenter = medianCenter, log = log,
        do.log = do.log, log.base = log.base)
    names(xlslist) <- make.names(sample.names)
    clustlist <- makeClustList(xlslist, clusternum, metric)
    lapply(names(clustlist), function(x) {
        plot.PAM(clustlist[[x]], x, res.folder = resultsFolder,
            col = plotCol, trim = trim, autoadj = autoadj, pdf.width = pdf.width,
            pdf.height = pdf.height, labelwidth = labelwidth,
            labelheight = labelheight, reorder = reorder, r.cex = r.cex,
            c.cex = c.cex)
    })
    saveObject(clustlist, file = "PAM clustering - complete object.xdr",
        path = resultsFolder)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
