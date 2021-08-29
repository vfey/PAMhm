#' @noRd
plot.PAM <-
		function (clust, what, res.folder = ".", cols = "bwr", trim = NULL,
		          winsorize.mat = TRUE, autoadj = TRUE, pdf.width = 13,
		          pdf.height = 10, labelwidth = 0.6, labelheight = 0.25,
		          reorder = c(TRUE, TRUE), r.cex = 0.5, c.cex = 1, PDF = TRUE,
		          PNG = FALSE, main = NULL, file = main, shiny = FALSE)
{

	if (autoadj) {
		adj.l <- plotAdjust(clust$dat)
	} else {
		adj.l <- list(pdf.width = pdf.width, pdf.height = pdf.height, labelwidth = labelwidth,
				labelheight = labelheight, r.cex = r.cex, c.cex = c.cex)
	}
	if (is.null(file)) {
		filename.pam <- paste("PAM clustering of", what)
	} else {
		filename.pam <- file
	}
	if (shiny) {
		make_heatmap(clust, what, cols = cols, trim = trim,
		             winsorize.mat = winsorize.mat, pdf.width = adj.l$pdf.width,
		             pdf.height = adj.l$pdf.height, labelwidth=adj.l$labelwidth,
		             labelheight=adj.l$labelheight, reorder=reorder,
		             r.cex=adj.l$r.cex, c.cex=adj.l$c.cex,
		             project.folder = res.folder, main=main)
	} else {
		if (PDF) {
			pdf.name <- file.path(res.folder, paste(filename.pam, ".pdf", sep=""))
			pdf(pdf.name, width=adj.l$pdf.width, height=adj.l$pdf.height)
			invisible(make_heatmap(clust, what, cols = cols, trim = trim,
			                       winsorize.mat = winsorize.mat,
			                       labelwidth=adj.l$labelwidth,
			                       labelheight=adj.l$labelheight, reorder=reorder,
			                       r.cex=adj.l$r.cex, c.cex=adj.l$c.cex, main=main))
			dev.off()
		}
		if (PNG) {
			png.name <- file.path(res.folder, paste(filename.pam, ".png", sep=""))
			png(png.name, width=adj.l$pdf.width, height=adj.l$pdf.height, units="in")
			make_heatmap(clust, what, cols = cols, trim = trim,
			             winsorize.mat = winsorize.mat, pdf.width = adj.l$pdf.width,
			             pdf.height = adj.l$pdf.height, labelwidth=adj.l$labelwidth,
			             labelheight=adj.l$labelheight, reorder=reorder,
			             r.cex=adj.l$r.cex, c.cex=adj.l$c.cex,
			             project.folder = res.folder, PNG = TRUE, main=main)
			if (all(unlist(plyr::llply(png.name, is.null)))) { png.name <- NULL }
			if (!PDF) {
				return(png.name)
			}
		}
		if (!PDF && !PNG) {
			make_heatmap(clust, what, cols = cols, trim = trim,
			             winsorize.mat = winsorize.mat, pdf.width = adj.l$pdf.width,
			             pdf.height = adj.l$pdf.height, labelwidth=adj.l$labelwidth,
			             labelheight=adj.l$labelheight, reorder=reorder,
			             r.cex=adj.l$r.cex, c.cex=adj.l$c.cex,
					project.folder = res.folder, main=main)
			return()
		}
		if (PDF) {
			return(pdf.name)
		} else {
			return(NULL)
		}
	}

}
