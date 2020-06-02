\name{plot.PAM}
\alias{plot.PAM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Main wrapper function to generate the plot
}
\description{
  The function calls 'make_heatmap' to generate the heatmap and can produce PDF and/or PNG files if desired. This is
  more a helper function and should generally not be called by the user.
}
\usage{
plot.PAM(clust, what, res.folder = ".", cols = "bwr", trim = -1, autoadj = TRUE, pdf.width = 13, pdf.height = 10, labelwidth = 0.6, labelheight = 0.25, reorder = c(TRUE, TRUE), r.cex = 0.5, c.cex = 1, PDF = TRUE, PNG = FALSE, main = NULL, file = main, shiny = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clust}{
%%     ~~Describe \code{clust} here~~
}
  \item{what}{
%%     ~~Describe \code{what} here~~
}
  \item{res.folder}{
%%     ~~Describe \code{res.folder} here~~
}
  \item{cols}{
%%     ~~Describe \code{cols} here~~
}
  \item{trim}{
%%     ~~Describe \code{trim} here~~
}
  \item{autoadj}{
%%     ~~Describe \code{autoadj} here~~
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
function (clust, what, res.folder = ".", cols = "bwr", trim = -1,
    autoadj = TRUE, pdf.width = 13, pdf.height = 10, labelwidth = 0.6,
    labelheight = 0.25, reorder = c(TRUE, TRUE), r.cex = 0.5,
    c.cex = 1, PDF = TRUE, PNG = FALSE, main = NULL, file = main,
    shiny = FALSE)
{
    if (autoadj) {
        adj.l <- plotAdjust(clust$dat)
    }
    else {
        adj.l <- list(pdf.width = pdf.width, pdf.height = pdf.height,
            labelwidth = labelwidth, labelheight = labelheight,
            r.cex = r.cex, c.cex = c.cex)
    }
    if (is.null(file)) {
        filename.pam <- paste("PAM clustering of", what)
    }
    else {
        filename.pam <- file
    }
    if (shiny) {
        make_heatmap(clust, what, cols = cols, trim = trim, pdf.width = adj.l$pdf.width,
            pdf.height = adj.l$pdf.height, labelwidth = adj.l$labelwidth,
            labelheight = adj.l$labelheight, reorder = reorder,
            r.cex = adj.l$r.cex, c.cex = adj.l$c.cex, folder.name = res.folder,
            main = main)
    }
    else {
        if (PDF) {
            pdf.name <- file.path(res.folder, paste(filename.pam,
                ".pdf", sep = ""))
            pdf(pdf.name, width = adj.l$pdf.width, height = adj.l$pdf.height)
            invisible(make_heatmap(clust, what, cols = cols,
                trim = trim, labelwidth = adj.l$labelwidth, labelheight = adj.l$labelheight,
                reorder = reorder, r.cex = adj.l$r.cex, c.cex = adj.l$c.cex,
                main = main))
            dev.off()
        }
        if (PNG) {
            png.name <- file.path(res.folder, paste(filename.pam,
                ".png", sep = ""))
            png(png.name, width = adj.l$pdf.width, height = adj.l$pdf.height,
                units = "in")
            make_heatmap(clust, what, cols = cols, trim = trim,
                pdf.width = adj.l$pdf.width, pdf.height = adj.l$pdf.height,
                labelwidth = adj.l$labelwidth, labelheight = adj.l$labelheight,
                reorder = reorder, r.cex = adj.l$r.cex, c.cex = adj.l$c.cex,
                folder.name = res.folder, PNG = TRUE, main = main)
            if (all(unlist(lapply(png.name, is.null)))) {
                png.name <- NULL
            }
            if (!PDF) {
                return(png.name)
            }
        }
        if (!PDF && !PNG) {
            make_heatmap(clust, what, cols = cols, trim = trim,
                pdf.width = adj.l$pdf.width, pdf.height = adj.l$pdf.height,
                labelwidth = adj.l$labelwidth, labelheight = adj.l$labelheight,
                reorder = reorder, r.cex = adj.l$r.cex, c.cex = adj.l$c.cex,
                folder.name = res.folder, main = main)
            return()
        }
        if (PDF) {
            return(pdf.name)
        }
        else {
            return(NULL)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
