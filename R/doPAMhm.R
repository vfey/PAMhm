#' @noRd
doPAMhm <-
		function(dat, project.folder = ".", nsheets = 1, symbolcol = 1,
				sample.names = NULL, cluster.number = 4, trim = -1,
				cols = "BlueWhiteRed", dendrograms = "Both", autoadj = TRUE, pdf.height = 10,
				pdf.width = 10, labelheight = 0.25, labelwidth = 0.2, r.cex = 0.5,
				c.cex = 1, medianCenter = NULL, log = FALSE, do.log = FALSE,
				log.base = 2, metric = "manhattan", makeFolder = TRUE, PDF = TRUE,
				PNG = TRUE, main = NULL, file = main, saveRData=FALSE, shiny=FALSE)
{

# create results folder
	resultsFolder <- make_res_folder(project.folder, makeFolder)

	if (length(sample.names) == 0) {
		# create arbitrary sample names
		sample.names <- paste("Sample", LETTERS[1 : nsheets])
	}

# colour settings
	plotCol <- setCol(cols)

# set dendrogram plotting
	reorder <- setDendro(dendrograms)

# extracting the desired (sequence of) number(s) of clusters
	clusternum <- clustNum(cluster.number)

# create objects to be used throughout the plotting
# lists allow for more complicated scenarios:
	# several sheets can be plotted using several different numbers of clusters with
	# the first loop
	xlslist <- makeXlsList(dat, symbolcol = symbolcol, medianCenter = medianCenter,
			log = log, do.log = do.log, log.base = log.base)
	names(xlslist) <- make.names(sample.names)
	clustlist <- makeClustList(xlslist, clusternum, metric)
	if (saveRData) {
		saveObject(clustlist, file = "PAM clustering - complete object.xdr", path = resultsFolder)
	}

# plotting every sheet to a separate PDF, different cluster numbers are plotted to
# the same file
	if (shiny) {
		plot.PAM(clustlist[[1]], names(clustlist)[1], res.folder = resultsFolder, cols = plotCol,
				trim = trim, autoadj = autoadj, pdf.width = pdf.width,
				pdf.height = pdf.height, labelwidth = labelwidth,
				labelheight = labelheight, reorder = reorder, r.cex = r.cex,
				c.cex = c.cex, PDF = PDF, PNG = PNG, main = main, file = file, shiny = TRUE)
	} else {
		pfln <- plyr::llply(names(clustlist), function(x) {
					plot.PAM(clustlist[[x]], x, res.folder = resultsFolder, cols = plotCol,
							trim = trim, autoadj = autoadj, pdf.width = pdf.width,
							pdf.height = pdf.height, labelwidth = labelwidth,
							labelheight = labelheight, reorder = reorder, r.cex = r.cex,
							c.cex = c.cex, PDF = PDF, PNG = PNG, main = main, file = file)
				})
		if (is.list(pfln)) {
			return(unlist(pfln))
		} else {
			return(pfln)
		}
	}

}
