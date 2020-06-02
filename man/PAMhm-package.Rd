\name{PAMhm-package}
\alias{PAMhm-package}
\alias{PAMhm}
\docType{package}
\title{
  Generate _fractioned_ heatmaps of partitioned data.
}
\description{
  The package offers tools to perform partitioning (clustering) of the data into ‘k’ clusters “around
     medoids”, which is a more robust version of K-means, before generating one or more heatmaps. The number
     of partitions can be controlled by the user. It is also possible to provide a range of desired numbers
     of clusters, whereupon several plots will be generated, one for each number. PDF and/or PNG files can
     be generated. The heatmap functionality relies on the 'heatmap.n' framework developed by Henri Sara.
}
\details{
\tabular{ll}{
Package: \tab PAMhm\cr
Type: \tab Package\cr
Version: \tab 0.1-0\cr
License: \tab MIT License\cr
}
  The main function 'doPAMhm' takes in a list of data.frames and calls 'makeXlsList' and 'makeClustList'
  before plotting by calling 'plot.PAM'.
}
\note{
  Based on a script by Pekka Kohonen and using functions developed by Henri Sara.
  Author: Vidal Fey <vidal.fey@utu.fi>
  Maintainer: Dhanaprakash Jambulingam <dhajam@utu.fi>
}
\keyword{ package }
