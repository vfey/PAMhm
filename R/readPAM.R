readPAM <-
function (file.name, path = ".", nsheets = 1, dec = ".", header = T, na.strings = "NA")
{

  ## checking file extension and then reading accordingly into lists (sheet-wise)
  ## if image file return object
  fl <- file.path(path, file.name)
  if (!file.exists(fl)) stop("File not found!")
  f.ext <- sub(".+(.)([[:alpha:]]{3})", "\\1\\2", file.name)
  if (length(grep("xls", f.ext)) > 0) {
    xls <- vector(mode = "list", length = nsheets)
    for (i in 1 : nsheets) {
      xls[[i]] <- read.xls(fl, sheet = i, header = header, dec = dec,
                           stringsAsFactor = F, comment.char = "",
                           na.strings = c(na.strings, paste(na.strings, " ",
                           sep = ""), "#N/A!", "#N/A! "))
    }
  } else if (length(grep("txt|tsv|csv", f.ext)) > 0) {
    nsheets <- 1
    ve <- c(",", ";", "\t")
    se <- which(lapply(ve, function(x) grep(x, readLines(fl, n = 1))) == 1)
    sep <- ve[se]
    xls <- list(read.table(fl, header = header, dec = dec, stringsAsFactor = FALSE,
                           sep = sep, comment.char = "", na.strings = na.strings))
  } else if (length(grep("xdr", f.ext)) > 0) {
    xls <- loadObject(fl)
  } else if (length(grep("RData", f.ext)) > 0) {
    l <- ls()
    load(fl)
    nl <- ls()
    if (length(nl) == length(l) + 2) {
      xls <- get(nl[- c(which(nl == "l"), match(l, nl))])
    } else {
      stop("Image file must contain only one (1) object!")
    }
  } else {
    stop("Please provide either an Excel document or a tab-, comma- or semi-colon-delimited text file or an R image file")
  }
  return(xls)

}
