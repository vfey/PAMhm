PAM.hm <-
function(x, folder.name = ".", nsheets = 1, dec = ".",
         header = TRUE, symbolcol = 1, startcol = 2, sample.names = NULL,
         cluster.number = as.character(4), trim = -1, cols = "bwr",
         dendrograms = "Both", autoadj = TRUE, pdf.height = 10,
         pdf.width = 10, labelheight = 0.25, labelwidth = 0.2,
				 r.cex = 0.5, c.cex = 1, medianCenter = NULL, log = FALSE,
				 do.log = FALSE, log.base = 2, metric = "manhatten",
         na.strings = "NA", makeFolder = TRUE)

{

# create results folder
resultsFolder <- make_res_folder(folder.name, makeFolder)

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
  dat <- readPAM(x, folder.name, nsheets = nsheets, dec = dec, header = header,
                 na.strings = na.strings)
} else {
  dat <- x
}



# remove empty columns introduced by Excel from data frame
dat <- lapply(dat, rm.empty.cols)

# create objects to be used throughout the plotting
# lists allow for more complicated scenarios:
 # several sheets can be plotted using several different numbers of clusters with
 # the first loop
  xlslist <- makeXlsList(dat, nsheets = nsheets, startcol = startcol,
                         symbolcol = symbolcol, medianCenter = medianCenter,
                         log = log, do.log = do.log, log.base = log.base)
  names(xlslist) <- make.names(sample.names)
  clustlist <- makeClustList(xlslist, clusternum, metric)

# plotting every sheet to a separate PDF, different cluster numbers are plotted to
# the same file
lapply(names(clustlist), function(x) {
       plot.PAM(clustlist[[x]], x, res.folder = resultsFolder, col = plotCol,
                trim = trim, autoadj = autoadj, pdf.width = pdf.width,
                pdf.height = pdf.height, labelwidth = labelwidth,
                labelheight = labelheight, reorder = reorder, r.cex = r.cex,
                c.cex = c.cex)
       })
saveObject(clustlist, file = "PAM clustering - complete object.xdr", path = resultsFolder)

}
