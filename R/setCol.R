#' @noRd
setCol <-
function (cols)
{

if(cols=="BlueWhiteRed") {colrs <- colorRampPalette(c("darkblue", "white", "red"))(30)}
if(cols=="RedBlackGreen") {colrs <- colorRampPalette(c("darkred", "black", "green"))(30)}
if(cols=="PurpleWhiteGreen") {colrs <- RColorBrewer::brewer.pal(11, "PRGn")}
if(cols=="OrangeWhitePurple") {colrs <- RColorBrewer::brewer.pal(11, "PuOr")}
if(cols=="RedWhiteBlue") {colrs <- RColorBrewer::brewer.pal(11, "RdBu")}
if(cols=="BrownWhiteTurquoise") {colrs <- RColorBrewer::brewer.pal(11, "BrBG")}
return(colrs)

}
