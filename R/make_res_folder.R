make_res_folder <-
function (folder.name, makeFolder = TRUE)
{

  if (makeFolder) {
    res.dir <- paste("Results", gsub("-", "", unlist(strsplit(as.character(Sys.time()),
                     " "))[1]), gsub(":", "", unlist(strsplit(as.character(Sys.time()),
                     " "))[2]), sep = "_")
    dir.create(path = file.path(folder.name, res.dir, sep = ""))
    resultsFolder <- file.path(folder.name, res.dir, sep = "")
  } else {
    resultsFolder <- folder.name
  }
  return(resultsFolder)

}
