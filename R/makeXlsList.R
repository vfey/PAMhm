makeXlsList <-
function (dat, nsheets = 1, startcol = 2, symbolcol = 1, medianCenter = NULL,
          log = FALSE, do.log = FALSE, log.base = 2)
{

  xlslist <- lapply(dat, function(x) {
                    endcol <- ncol(x)
                    y <- as.matrix(x[, startcol : endcol])
                    rownames(y) <- make.names(x[, symbolcol])
                    colnames(y) <- make.names(colnames(y))
                    # remove NA rows from matrix
                    if (any(is.na(y))) {
                      rm.r <- which(apply(y, 1, function(z) all(is.na(z))))
                      if (length(rm.r) > 0) y <- y[-rm.r, ]
                    }
                    # apply different median centring methods
                    ## over the entire matrix
                    centerMatrix(y, medianCenter = medianCenter, log = log,
                                 do.log = do.log, log.base = log.base)
                    })
  return(xlslist)

}
