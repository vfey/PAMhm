make_heatmap <-
function (clust, what, cols = "bwr", trim = -1, pdf.width = 13,
          pdf.height = 10,  labelwidth = 0.6, labelheight = 0.25,
          reorder = c(TRUE, TRUE), r.cex = 0.5, c.cex = 1, folder.name = ".",
          PNG = FALSE, main = NULL)
{

nl <- list()
png.name <- lapply(clust$PAM, function(x) {
                   if (PNG) {
                     png.name <- file.path(folder.name, paste("PAM clustering of",
                                           what, x$clusternum, "clusters.png"))
                     png(png.name, width = pdf.width, height = pdf.height,
                         units = "in", res = 300)
                   }
                   par(mar=par("mar")+c(5,5,0,5))
                   grps <- x$PAM$clustering
                   m <- x$PAM$data
                   if (min(m, na.rm = TRUE) > 0) {
                     trim <- NULL
                   } else if (!is.null(trim) && trim < 0) {
                     trim <- abs(min(ceiling(c(min(m, na.rm = TRUE),
                                 max(m, na.rm = TRUE))), na.rm = TRUE))
                   }
				   if (is.null(main)) {
					   main <- paste(what, ", ",
							   x$clusternum, " clusters", sep="")
				   } else {
					   main <- paste(main, ", ",
							   x$clusternum, " clusters", sep="")
				   }
                   heatmap.n(m, col=cols, trim=trim, main=main, rowMembers=grps,
                             labRow=rownames(clust$dat), labelwidth=labelwidth,
                             labelheight=labelheight, reorder=reorder,
                             r.cex=r.cex, c.cex=c.cex)
                   if (PNG) {
                     dev.off()
                     nl[[paste(what, x$clusternum, "clust", sep = "")]] <- png.name
                   } else {
                     nl[[paste(what, x$clusternum, "clust", sep = "")]] <- NULL
                   }
                   nl
                   })
return(png.name)

}
