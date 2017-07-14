#' String of current time
#' 
#' These functions gives the current time
#' @keywords time
#' @export
#' @examples
#' 
get_legend<-function(myggplot){
  tmp <- ggplot2::ggplotGrob(myggplot + ggplot2::theme(legend.position="right"))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}