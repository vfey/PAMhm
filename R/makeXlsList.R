#' @noRd
makeXlsList <-
function (dat, symbolcol = 1, medianCenter = NULL,
          log = FALSE, do.log = FALSE, log.base = 2)
{

  xlslist <- plyr::llply(dat, function(x) {
                    colrange <- 1 : ncol(x)
                    colrange <- colrange[-(grep(symbolcol, colrange))]
                    y <- as.matrix(x[, colrange])
                    rownames(y) <- make.names(x[, symbolcol])
                    colnames(y) <- make.names(colnames(y))
                    # remove NA rows from matrix
                    if (any(is.na(y))) {
                      rm.r <- which(apply(y, 1, function(z) all(is.na(z))))
                      if (length(rm.r) > 0) y <- y[-rm.r, ]
                    }
                    # apply different median centering methods
                    ## over the entire matrix
                    centerMatrix(y, medianCenter = medianCenter, log = log,
                                 do.log = do.log, log.base = log.base)
                    })
  return(xlslist)

}
