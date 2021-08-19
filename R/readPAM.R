#' @noRd
readPAM <-
  function (file.name, path = ".", nsheets = 1, dec = ".", header = T, na.strings = "NA")
  {

    ## checking file extension and then reading accordingly into lists (sheet-wise)
    ## if image file return object
    if (unique(dirname(file.name)) != ".") {
      fl <- file.name
    } else {
      fl <- file.path(path, file.name)
    }
    if (!file.exists(fl)) stop("File not found!")
    f.ext <- sub(".+(.)([[:alpha:]]{3})", "\\1\\2", file.name)
    if (length(grep("xls", f.ext)) > 0) {
      xls <- plyr::llply(1 : nsheets, function(i) {
        as.data.frame(
          readxl::read_excel(fl, sheet = i, na = c(na.strings, paste(na.strings, " ", sep = ""), "#N/A!", "#N/A! ")))
      })
    } else if (length(grep("txt|tsv|csv", f.ext)) > 0) {
      nsheets <- 1
      ve <- c(",", ";", "\t")
      se <- which(plyr::llply(ve, function(x) grep(x, readLines(fl, n = 1))) == 1)
      sep <- ve[se]
      xls <- list(read.table(fl, header = header, dec = dec, stringsAsFactors = FALSE,
                             sep = sep, comment.char = "", na.strings = na.strings))
    } else {
      stop("Please provide either an Excel document or a tab-, comma- or semi-colon-delimited text file or an R image file")
    }
    return(xls)

  }
