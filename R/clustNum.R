#' @noRd
clustNum <-
  function (cluster.number)
  {

    if (is.numeric(cluster.number)) {
      cluster.number <- as.character(as.integer(cluster.number))
      if (length(cluster.number) > 1) {
        cluster.number <- paste(cluster.number, collapse = ",")
      }
    }

    num1 <- trimws(unlist(strsplit(cluster.number, ",")))
    clusternum <- c(plyr::llply(num1, function(x) {
      y <- unlist(strsplit(x, "-"))
      if(length(y) == 2) {
        y <- y[1] : y[2]
      }
      y <- as.numeric(y)
      y
    }),
    recursive = T)

  }
