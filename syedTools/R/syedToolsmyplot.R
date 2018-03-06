#' Wrapper function for ggplot2 for data d
#'
#' Computes the mean, variance and sd of a vector
#'
#' @param x data.frame
#'
#' @return ggplot2
#' @export
#' @examples
#' data(d)
#' plotMyData(d)
plotMyData<-function(x){
  library(magrittr)
 x%>% ggplot2::ggplot()+ggplot2::aes(x=x, y=p)+ggplot2::geom_point()
}
############################################################################################

#' Wrapper function for dplyr
#' 
#' @param x data.frame
#' 
#' @return dplyr
#' @export
#' @examples
#' data(d)
#' dplyr::filter(d, p>0.05)
#'

filterFun<-function(x){
  dplyr::filter(x, p>0.05) 
}