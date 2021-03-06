#' Plot the ROC curve
#'
#' plot the ROC curve for classification of y with p
#' @param p the probability vector
#' @param y the response vector
#' @keywords roc specificity sensitivity
#' @export
#' @examples
#' roc(p, y, ...)
roc <- function(p,y, ...){
  y <- factor(y)
  n <- length(p)
  p <- as.vector(p)
  Q <- p > matrix(rep(seq(0,1,length=100),n),ncol=100,byrow=TRUE)
  specificity <- colMeans(!Q[y==levels(y)[1],])
  sensitivity <- colMeans(Q[y==levels(y)[2],])
  plot(1-specificity, sensitivity, type="l", ...)
  abline(a=0,b=1,lty=2,col=8)
}

