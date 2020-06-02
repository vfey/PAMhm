makeClustList <-
function (xlslist, clusternum, metric = "manhatten")
{

  clustlist <- lapply(xlslist, function(x) {
                      y <- list()
                      y$dat <- x
                      y$PAM <- lapply(clusternum, function(z) {
                                      p <- list()
                                      p$clusternum <- z
                                      p$PAM <- pam(x, z, metric = metric)
                                      p
                                      })
                      y
                      })

}
