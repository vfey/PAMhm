\name{doPAMhm}
\alias{doPAMhm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Top-level wrapper to perform the clustering, draw the heatmap and save the graphic files
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
doPAMhm(dat, folder.name = ".", nsheets = 1, symbolcol = 1, startcol = 2, sample.names = NULL, cluster.number = as.character(4), trim = -1, cols = "BlueWhiteRed", dendrograms = "Both", autoadj = TRUE, pdf.height = 10, pdf.width = 10, labelheight = 0.25, labelwidth = 0.2, r.cex = 0.5, c.cex = 1, medianCenter = NULL, log = FALSE, do.log = FALSE, log.base = 2, metric = "manhatten", makeFolder = TRUE, PDF = TRUE, PNG = TRUE, main = NULL, file = main, saveRData = FALSE, shiny = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{folder.name}{
%%     ~~Describe \code{folder.name} here~~
}
  \item{nsheets}{
%%     ~~Describe \code{nsheets} here~~
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
  \item{makeFolder}{
%%     ~~Describe \code{makeFolder} here~~
}
  \item{PDF}{
%%     ~~Describe \code{PDF} here~~
}
  \item{PNG}{
%%     ~~Describe \code{PNG} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{saveRData}{
%%     ~~Describe \code{saveRData} here~~
}
  \item{shiny}{
%%     ~~Describe \code{shiny} here~~
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
function (dat, folder.name = ".", nsheets = 1, symbolcol = 1,
    startcol = 2, sample.names = NULL, cluster.number = as.character(4),
    trim = -1, cols = "BlueWhiteRed", dendrograms = "Both", autoadj = TRUE,
    pdf.height = 10, pdf.width = 10, labelheight = 0.25, labelwidth = 0.2,
    r.cex = 0.5, c.cex = 1, medianCenter = NULL, log = FALSE,
    do.log = FALSE, log.base = 2, metric = "manhatten", makeFolder = TRUE,
    PDF = TRUE, PNG = TRUE, main = NULL, file = main, saveRData = FALSE,
    shiny = FALSE)
{
    resultsFolder <- make_res_folder(folder.name, makeFolder)
    if (length(sample.names) == 0) {
        sample.names <- paste("Sample", LETTERS[1:nsheets])
    }
    plotCol <- setCol(cols)
    reorder <- setDendro(dendrograms)
    clusternum <- clustNum(cluster.number)
    xlslist <- makeXlsList(dat, nsheets = nsheets, startcol = startcol,
        symbolcol = symbolcol, medianCenter = medianCenter, log = log,
        do.log = do.log, log.base = log.base)
    names(xlslist) <- make.names(sample.names)
    clustlist <- makeClustList(xlslist, clusternum, metric)
    if (saveRData) {
        saveObject(clustlist, file = "PAM clustering - complete object.xdr",
            path = resultsFolder)
    }
    if (shiny) {
        plot.PAM(clustlist[[1]], names(clustlist)[1], res.folder = resultsFolder,
            col = plotCol, trim = trim, autoadj = autoadj, pdf.width = pdf.width,
            pdf.height = pdf.height, labelwidth = labelwidth,
            labelheight = labelheight, reorder = reorder, r.cex = r.cex,
            c.cex = c.cex, PDF = PDF, PNG = PNG, main = main,
            file = file, shiny = TRUE)
    }
    else {
        pfln <- lapply(names(clustlist), function(x) {
            plot.PAM(clustlist[[x]], x, res.folder = resultsFolder,
                col = plotCol, trim = trim, autoadj = autoadj,
                pdf.width = pdf.width, pdf.height = pdf.height,
                labelwidth = labelwidth, labelheight = labelheight,
                reorder = reorder, r.cex = r.cex, c.cex = c.cex,
                PDF = PDF, PNG = PNG, main = main, file = file)
        })
        if (is.list(pfln)) {
            return(unlist(pfln))
        }
        else {
            return(pfln)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
