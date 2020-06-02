setCol <-
function (cols)
{

if(cols=="BlueWhiteRed") {colrs <- colorRampPalette(c("darkblue", "white", "red"))(30)}
if(cols=="RedBlackGreen") {colrs <- hmcols()}
if(cols=="PurpleWhiteGreen") {colrs <- brewer.pal(11, "PRGn")}
if(cols=="OrangeWhitePurple") {colrs <- brewer.pal(11, "PuOr")}
if(cols=="RedWhiteBlue") {colrs <- brewer.pal(11, "RdBu")}
if(cols=="BrownWhiteTurquoise") {colrs <- brewer.pal(11, "BrBG")}
return(colrs)

}
