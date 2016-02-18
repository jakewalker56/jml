#This is an internal function called by convert_to_string
atomic_convert_to_string <- function(d){
  if(!is.character(d)){
      d <- as(d, "character")
  }
  return(d)
}

#' Convert To String
#'
#' This function converts a vector or data frame to string values
#' @param d the data to be converted
#' @keywords string convert
#' @export
#' @examples
#' convert_to_string(data.frame(y=rbind(1, 2, 3)))
convert_to_string <- function (d){
  if(is.atomic(d)) {
    return(atomic_convert_to_string(d))
  } else {
    for(di in colnames(d)) {
      #not sure if you can have a frame with a frame inside it, but I guess we'll support it with a recursive call?
      #note that this will break if convert_to_numeric doesn't simplify to an atomic eventually
      d[,di] <- convert_to_string(d[,di])
    }
  }
  return(d)
}