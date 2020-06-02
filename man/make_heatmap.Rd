\name{make_heatmap}
\alias{make_heatmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Internal wrapper around heatmap.n
}
\description{
  Internal function not to be called by the user directly.
}
\usage{
make_heatmap(clust, what, cols = "bwr", trim = -1, pdf.width = 13, pdf.height = 10, labelwidth = 0.6, labelheight = 0.25, reorder = c(TRUE, TRUE), r.cex = 0.5, c.cex = 1, folder.name = ".", PNG = FALSE, main = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clust}{
%%     ~~Describe \code{clust} here~~
}
  \item{what}{
%%     ~~Describe \code{what} here~~
}
  \item{cols}{
%%     ~~Describe \code{cols} here~~
}
  \item{trim}{
%%     ~~Describe \code{trim} here~~
}
  \item{pdf.width}{
%%     ~~Describe \code{pdf.width} here~~
}
  \item{pdf.height}{
%%     ~~Describe \code{pdf.height} here~~
}
  \item{labelwidth}{
%%     ~~Describe \code{labelwidth} here~~
}
  \item{labelheight}{
%%     ~~Describe \code{labelheight} here~~
}
  \item{reorder}{
%%     ~~Describe \code{reorder} here~~
}
  \item{r.cex}{
%%     ~~Describe \code{r.cex} here~~
}
  \item{c.cex}{
%%     ~~Describe \code{c.cex} here~~
}
  \item{folder.name}{
%%     ~~Describe \code{folder.name} here~~
}
  \item{PNG}{
%%     ~~Describe \code{PNG} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
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
function (clust, what, cols = "bwr", trim = -1, pdf.width = 13,
    pdf.height = 10, labelwidth = 0.6, labelheight = 0.25, reorder = c(TRUE,
        TRUE), r.cex = 0.5, c.cex = 1, folder.name = ".", PNG = FALSE,
    main = NULL)
{
    nl <- list()
    png.name <- lapply(clust$PAM, function(x) {
        if (PNG) {
            png.name <- file.path(folder.name, paste("PAM clustering of",
                what, x$clusternum, "clusters.png"))
            png(png.name, width = pdf.width, height = pdf.height,
                units = "in", res = 300)
        }
        par(mar = par("mar") + c(5, 5, 0, 5))
        grps <- x$PAM$clustering
        m <- x$PAM$data
        if (min(m, na.rm = TRUE) > 0) {
            trim <- NULL
        }
        else if (!is.null(trim) && trim < 0) {
            trim <- abs(min(ceiling(c(min(m, na.rm = TRUE), max(m,
                na.rm = TRUE))), na.rm = TRUE))
        }
        if (is.null(main)) {
            main <- paste(what, ", ", x$clusternum, " clusters",
                sep = "")
        }
        else {
            main <- paste(main, ", ", x$clusternum, " clusters",
                sep = "")
        }
        heatmap.n(m, col = cols, trim = trim, main = main, rowMembers = grps,
            labRow = rownames(clust$dat), labelwidth = labelwidth,
            labelheight = labelheight, reorder = reorder, r.cex = r.cex,
            c.cex = c.cex)
        if (PNG) {
            dev.off()
            nl[[paste(what, x$clusternum, "clust", sep = "")]] <- png.name
        }
        else {
            nl[[paste(what, x$clusternum, "clust", sep = "")]] <- NULL
        }
        nl
    })
    return(png.name)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
