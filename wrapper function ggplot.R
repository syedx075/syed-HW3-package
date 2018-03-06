plotMyDat <- function(x){
  x%>% ggplot2::ggplot()+ggplot2::aes(x=x, y=p)
}