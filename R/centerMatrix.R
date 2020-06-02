centerMatrix <-
function (xls, medianCenter = NULL, log = FALSE, do.log = FALSE, log.base = 2)
{

  if (missing(xls)) stop("No data input")
  if (is.character(log.base)) {
    if (!is.na(as.integer(log.base))) {
      log.base <- as.integer(log.base)
    } else if (log.base == "e") {
      log.base <- exp(1)
    } else {
      stop("Inappropriate log-base...")
    }
  }
  if (is.null(medianCenter)) {
    if (do.log) {
      if (!log) {
        xls <- log(xls, base = log.base)
      } else {
        xls <- log.base ^ xls
      }
    }
  } else if (medianCenter == "grand") {
  	if (log) {
      xls <- xls - median(xls, na.rm = TRUE)
    } else {
      xls <- log(xls / median(xls, na.rm = TRUE), base = log.base)
    }
  ## row-wise (centres a gene over all samples -> emphas. gene expression between samples;
  ## similarly regulated genes will cluster)
  } else if (medianCenter == "row") {
  	if (log) {
      mr <- apply(xls, 1, median, na.rm = TRUE)
  	  xls <- sweep(xls, 1, mr)
    } else {
  		mr <- apply(log(xls, base = log.base), 1, median, na.rm = TRUE)
  		xls <- sweep(log(xls, base = log.base), 1, mr)
    }
  ## column-wise (centres a sample over all genes -> emphas. gene expression per one sample;
  ## similarly behaving samples will cluster)
  } else if (medianCenter == "column") {
  	if (log) {
      mr <- apply(xls, 2, median, na.rm = TRUE)
  	  xls <- sweep(xls, 2, mr)
    } else {
  		mr <- apply(log(xls, base = log.base), 2, median, na.rm = TRUE)
  		xls <- sweep(log(xls, base = log.base), 2, mr)
    }
  }
  return(xls)

}
