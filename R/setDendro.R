setDendro <-
function (dendrograms)
{

  if (dendrograms == "Both") {
    reorder <- c(TRUE, TRUE)
  } else if (dendrograms == "Vertical") {
    reorder <- c(TRUE, FALSE)
  } else if (dendrograms == "Horizontal") {
    reorder <- c(FALSE, TRUE)
  } else if (dendrograms == "None") {
    reorder <- c(FALSE, FALSE)
  }
  return(reorder)

}
