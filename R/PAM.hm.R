#' @title Generate Heatmaps Based on Partioning Around Medoids (PAM)
#' @docType package
#' @name PAMhm
#' @description Data are partitioned (clustered) into k clusters "around medoids", which is
#' a more robust version of K-means implemented in the function pam() in the 'cluster' package.
#' The PAM algorithm is described in Kaufman and Rousseeuw (1990) <doi:10.1002/9780470316801>.
#' Please refer to the pam() function documentation for more references.
#' Clustered data is plotted as a split heatmap allowing visualisation of representative
#' "group-clusters" (medoids) in the data as separated fractions of the graph while those
#' "sub-clusters" are visualised as a traditional heatmap based on hierarchical clustering.
#' @author Vidal Fey <vidal.fey@gmail.com>, Henri Sara <henri.sara@gmail.com>
#' Maintainer: Vidal Fey <vidal.fey@gmail.com>
#' @details \tabular{ll}{
#' Package: \tab PAMhm\cr
#' Type: \tab Package\cr
#' Initial version: \tab 0.1-0\cr
#' Created: \tab 2011-01-07\cr
#' License: \tab GPL-3\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' @keywords package
#' @import RColorBrewer
#' @import readxl
#' @import cluster
#' @import heatmapFlex
#' @import stats
#' @import grDevices
#' @import graphics
#' @importFrom robustHD winsorize
#' @importFrom readmoRe rm.empty.cols
#' @importFrom utils packageVersion read.delim read.table
#' @importFrom R.utils loadObject saveObject
NULL
#'
#' Main function to produce a heatmap using PAM clustering.
#' @description This is the main wrapper function to be called by end users. It accepts a numeric matrix
#'     (or an object that can be coerced to a numeric matrix) or a number of data file formats and produces one or
#'     more PDFs with the plots.
#' @param x (\code{character}, \code{data.frame}, \code{numeric}). The name(s) of the input files(s) (character vector)
#'     or a data object such as a \code{data.frame} or \code{numeric matrix}. See 'Details'.
#' @param project.folder (\code{character}). Name of the root folder inside which the results will be created if any files are to be saved. See 'Details'.
#' @param nsheets (\code{integer}). Number of sheets to be read if file is of type ".xls" or ".xlsx". All sheets starting from 1 up to the
#'     given number in the respective data file will be read. If more than one file is read this must be be an integer vector with the
#'     numbers of sheets in exactly the same order as the files.
#' @param dec (\code{character}). The decimal separator for numbers.
#' @param header (\code{logical}). Does the input file have a header row?
#' @param symbolcol (\code{character}). The name of the column with identifiers used as labels.
#' @param sample.names (\code{character}). A vector of names used for plot titles and output files.
#' @param cluster.number (\code{character} or \code{integer}). A vector of numbers used for PAM clustering (corresponds to argument
#'     \code{k} in \code{\link[cluster]{pam}}). If a character vector, this is broken down to a numeric vector accepting
#'     comma-separated strings in the form of, e.g, "4" and "2-5". The clustering algorithm then iterates through all given numbers.
#'     See 'Details'.
#' @param trim (\code{numeric}). Value to "cut off" data distribution. Values at both ends of the distribution,
#'     larger or smaller, respectively, will be made equal to \code{+/-trim}, i.e., data will be symmetrical around 0.
#'     \code{NULL} means no trimming which is the default. If \code{trim} is \code{-1} (or any negative value) and
#'     \code{winsorize.mat} is \code{TRUE} the matrix will be \emph{winsorized} and then the smaller of the two largest
#'     absolute values at both ends of the distribution rounded to three digits will be used. If \code{winsorize.mat}
#'     is \code{FALSE} the largest possible absolute integer, i.e., the smaller of the to extreme integers is used.
#'     Trimming is disabled for only positive or only negative values.
#' @param winsorize.mat (\code{logical}). Should the matrix be \emph{winsorized} (cleaned of outliers) before plotting?
#'     Defaults to \code{TRUE}. See 'Details'.
#' @param cols (\code{character}). Name of the colour palette.
#' @param dendrograms (\code{character}). Which dendrograms are to be plotted? One of "Vertical", "Horizontal",
#'     "None" or "Both". Defaults to "Both".
#' @param autoadj (\code{logical}). Should label sizes and pdf dimensions be adjusted automatically? See 'Details'.
#' @param pdf.height (\code{numeric}). Heigth of the PDF device.
#' @param pdf.width (\code{numeric}). Width of the PDF device.
#' @param labelheight (\code{numeric} or \code{lcm(numeric)}). Relative or absolute height (using \code{lcm}, see \code{layout}) of the labels.
#' @param labelwidth (\code{numeric} or \code{lcm(numeric)}). Relative or absolute width (using \code{lcm}, see \code{layout}) of the labels.
#' @param r.cex (\code{numeric}). Font size for row labels.
#' @param c.cex (\code{numeric}). Font size for column labels.
#' @param medianCenter (\code{character}). If not \code{NULL}, how should data be median-centered? One of "grand",
#'     "row" or "column". Defaults to \code{NULL}, no median-centering.
#' @param log (\code{logical}). Is the data on log-scale. (The log-base is given in argument \code{log.base}).
#' @param do.log (\code{logical}). Should data be log-transformed? (The log-base is given in argument \code{log.base}).
#' @param log.base (\code{numeric}). The log-base used for \code{log} and \code{do.log}.
#' @param metric (\code{character}). The metric metric to be used for calculating dissimilarities between observations.
#'     The currently available options are "euclidean" and "manhattan". Euclidean distances are root sum-of-squares of differences,
#'     and manhattan distances are the sum of absolute differences. Defaults to "manhattan".
#' @param na.strings (\code{character}). Character vector of strings to interpret as missing values when reading data files
#'     with \code{read.table} or \code{readxlread_excel}. By default, \code{readxl} treats blank cells as missing data.
#' @param makeFolder (\code{logical}). Should the results folder be created?
#' @param do.pdf (\code{logical}). Should images be saved to PDFs?
#' @param do.png (\code{logical}). Should images be saved to PNGs?
#' @param save.objects (\code{logical}). Should R objects be save to disk?
#'
#' @details Argument \code{x} can be a \code{data.frame} or numeric matrix to be used directly for plotting the heatmap.
#'     If it is a \code{data.frame} argument \code{symbolcol} sets the respective columns for symbols to be used as
#'     labels and the column where the numeric data starts.
#'
#'     Matrices will be coerced to data frames.
#'
#'     The read function accepts txt, tsv, csv and xls files.
#'
#'     If PDF, PNG or R object files are to be saved, i.e., if the corresponding arguments are \code{TRUE}, a results
#'     folder will be created using time and date to create a unique name. The folder will be created in the directory
#'     set by argument \code{project.folder}. The reasoning behind that behaviour is that during development the
#'     heatmap was used as data analysis tool testing various \code{cluster.number} values with numerous files and
#'     comparing the results.
#'
#'     The \code{cluster.number} argument defines the numbers of clusters when doing PAM. After processing it is passed
#'     one-by-one to argument \code{k} in \code{\link[cluster]{pam}}. The numbers can be defined in the form
#'     \code{c("2","4-7", "9")}, for example, depending on the experimental setup. An integer vector is coerced to
#'     character.
#'
#'     If \code{autoadj} is \code{TRUE} character expansion (cex) for rows annd columns, pdf width and height and
#'     label width and height are adjusted automatically based on the dimensions of the data matrix and length
#'     (number of characters) of the labels.
#'
#'     The default behavior regarding outliers is to \emph{winsorize} the matrix before plotting, i.e., shrink outliers
#'     to the unscattered part of the data by replacing extreme values at both ends of the distribution with less
#'     extreme values. This is done for the same reason as trimming but the data will not be symmetrical around 0.
#' @references
#'     Kaufman, L., & Rousseeuw, P. J. (Eds.). (1990). \emph{Finding Groups in Data: An Introduction to Cluster Analysis.}
#'     John Wiley & Sons, Inc. \doi{10.1002/9780470316801}
#' @seealso \code{\link[utils]{read.delim}}
#' @seealso \code{\link[readxl]{read_excel}}
#' @seealso \code{\link[cluster]{pam}}
#' @return A list: Invisibly returns the results object from the PAM clustering.
#' @examples
#' # Generate a random 10x10 matrix and plot it using default values
#' set.seed(1234)                                  # for reproducibility
#' mat <- matrix(rnorm(120), nrow = 20)            # standard normal
#' PAM.hm(mat, cluster.number = 3)
#'
#' ## Plot with more than one cluster number
#' PAM.hm(mat, cluster.number = 2:4)               # integer vector
#' PAM.hm(mat, cluster.number = c("2", "4-5"))     # character vector
#'
#' # Using the 'trim' argument
#' ## Introduce outlier to the matrix and plot w/o trimming or winsorization
#' mat[1] <- mat[1] * 10
#' PAM.hm(mat, cluster.number = 3, trim = NULL, winsorize = FALSE)
#'
#' ## calculate a trim value by getting the largest possible absolute integer and
#' ## plot again
#' tr <- min(abs(ceiling(c(min(mat, na.rm = TRUE), max(mat, na.rm = TRUE)))),
#'     na.rm = TRUE)
#' PAM.hm(mat, cluster.number = 3, trim = tr, winsorize = FALSE)
#' ## Note that the outlier is still visible but since it is less extreme
#' ## it does not distort the colour scheme.
#'
#' # An example reading data from an Excel file
#' # The function readxl::read_excel is used internally to read Excel files.
#' # The example uses their example data.
#' readxl_datasets <- readxl::readxl_example("datasets.xlsx")
#' PAM.hm(readxl_datasets, cluster.number = 4, symbolcol = 5)
#'
#' @export
PAM.hm <-
  function(x, project.folder = ".", nsheets = 1, dec = ".",
           header = TRUE, symbolcol = 1, sample.names = NULL,
           cluster.number = 4, trim = NULL, winsorize.mat = TRUE,
           cols = "BlueWhiteRed", dendrograms = "Both", autoadj = TRUE,
           pdf.height = 10, pdf.width = 10, labelheight = 0.25, labelwidth = 0.2,
           r.cex = 0.5, c.cex = 1, medianCenter = NULL, log = FALSE,
           do.log = FALSE, log.base = 2, metric = "manhattan",
           na.strings = "NA", makeFolder = TRUE, do.pdf = FALSE, do.png = FALSE,
           save.objects = FALSE)

  {

    # create results folder
    if (any(c(do.pdf, do.png, save.objects))) {
      resultsFolder <- make_res_folder(project.folder, makeFolder)
    }

    # create dummy sample names for output files and plot titles
    if (is.null(sample.names)) {
      # create arbitrary sample names
      sample.names <- paste("Sample", LETTERS[1 : nsheets])
    }

    # colour settings
    plotCol <- setCol(cols)

    # set dendrogram plotting
    reorder <- setDendro(dendrograms)

    # extracting the desired (sequence of) number(s) of clusters
    clusternum <- clustNum(cluster.number)

    # reading data
    if (is.character(x)) {
      dat <- readPAM(x, project.folder, nsheets = nsheets, dec = dec, header = header,
                     na.strings = na.strings)
    } else if (is.matrix(x)) {
      if (!is.numeric(x))
        stop("Data must be a numeric matrix or a data frame.")
      dat <- as.data.frame(x)
      dat$sym <- 1:nrow(dat)
      dat <- dat[, c(ncol(dat), (1:ncol(dat)-1))]
      dat <- list(dat)
    }



    # remove empty columns introduced by Excel from data frame
    dat <- plyr::llply(dat, readmoRe::rm.empty.cols)

    # create objects to be used throughout the plotting
    # lists allow for more complicated scenarios:
    # several sheets can be plotted using several different numbers of clusters with
    # the first loop
    xlslist <- makeXlsList(dat, symbolcol = symbolcol, medianCenter = medianCenter,
                           log = log, do.log = do.log, log.base = log.base)
    names(xlslist) <- make.names(sample.names)
    clustlist <- makeClustList(xlslist, clusternum, metric)

    # plotting every sheet to a separate PDF, different cluster numbers are plotted to
    # the same file
    plyr::llply(names(clustlist), function(x) {
      plot.PAM(clustlist[[x]], x, res.folder = resultsFolder, cols = plotCol,
               trim = trim, winsorize.mat = winsorize.mat, autoadj = autoadj,
               pdf.width = pdf.width, pdf.height = pdf.height, labelwidth = labelwidth,
               labelheight = labelheight, reorder = reorder, r.cex = r.cex,
               c.cex = c.cex, PDF = do.pdf, PNG = do.png)
    })
    if (save.objects) {
      R.utils::saveObject(clustlist, file = "PAM clustering - complete object.xdr", path = resultsFolder)
    }
    invisible(clustlist)

  }
