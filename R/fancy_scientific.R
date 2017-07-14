#' String of current time
#' 
#' These functions gives the current time
#' @keywords time
#' @export
#' @examples
#' 
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  #print(l)
  #print(class(l)=="character")
  if (class(l)=="character"){
    l=as.numeric(l)
  }
  l <- format(l, digits=3,scientific = TRUE)
  l <- gsub("0e\\+00","0",l)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  l <- gsub("\\+", "", l)
  # return this as an expression
  parse(text=l)
}

#' @rdname fancy_scientific
fancy2_scientific<-function(plot,digits=2){
  temp_labelsx=ggplot_build(plot)$layout$panel_ranges[[1]]$y.labels
  temp_labelsy=ggplot_build(plot)$layout$panel_ranges[[1]]$y.labels
  
  coeff_x=as.numeric(names(sort(table(floor(log10(as.numeric(temp_labelsx)))),decreasing = TRUE))[1])
  coeff_y=as.numeric(names(sort(table(floor(log10(as.numeric(temp_labelsy)))),decreasing = TRUE))[1])
  
  new_labelsx=round(as.numeric(temp_labelsx)/(10^coeff_x),digits=digits)
  new_labelsy=round(as.numeric(temp_labelsy)/(10^coeff_y),digits=digits)
  
  plot+scale_x_continuous(labels=new_labelsx,breaks=as.numeric(temp_labelsx))+scale_y_continuous(labels=new_labelsy,breaks=as.numeric(temp_labelsy))
}