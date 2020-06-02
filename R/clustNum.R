clustNum <-
function (cluster.number)
{

num1 <- unlist(strsplit(cluster.number, ","))
clusternum <- c(lapply(num1, function(x) {
                       y <- unlist(strsplit(x, "-"))
                       if(length(y) == 2) {
                         y <- y[1] : y[2]
                       }
                       y <- as.numeric(y)
                       y
                       }),
                recursive = T)

}
