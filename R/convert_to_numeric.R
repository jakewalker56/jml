#This is an internal function called by convert_to_numeric
atomic_convert_to_numeric <- function(d){
  if(!is.numeric(d)){
    if(is.factor(d)) {
      #if the value is NA, set it to 0
      d[is.na(d)] <- "0"
      #if factor, convert it to its char representation before converting to numeric. This at least supports factored integer representations
      d <- as(d, "character")
    }
    d <- as(d,"numeric")
  } else {
    #this is numeric, but it could still have NA's
    d[is.na(d)] <- 0
  }
  return(d)
}

#' Convert To Numeric
#'
#' This function converts a vector or data frame to numeric values
#' @param d the data to be converted
#' @keywords numeric convert
#' @export
#' @examples
#' convert_to_numeric(data.frame(y=array("1", "2", "3")))
convert_to_numeric <- function (d){
  if(is.atomic(d)) {
    return(atomic_convert_to_numeric(d))
  } else {
    for(di in colnames(d)) {
      #not sure if you can have a frame with a frame inside it, but I guess we'll support it with a recursive call?
      #this will break if convert_to_numeric doesn't simplify to an atomic eventually
      d[,di] <- convert_to_numeric(d[,di])
    }
 }
  return(d)
}
