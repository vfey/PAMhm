#' Create results folder for images
#' @noRd
make_res_folder <-
function (project.folder = ".", makeFolder = TRUE)
{

  if (makeFolder) {
    res.dir <- paste("Results", gsub("-", "", unlist(strsplit(as.character(Sys.time()),
                     " "))[1]), gsub(":", "", unlist(strsplit(as.character(Sys.time()),
                     " "))[2]), sep = "_")
    dir.create(path = file.path(project.folder, res.dir, sep = ""))
    resultsFolder <- file.path(project.folder, res.dir, sep = "")
  } else {
    resultsFolder <- project.folder
  }
  return(resultsFolder)

}
