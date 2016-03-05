#' MultiLogLoss
#'
#' This function returns the logloss cost function value for actual and predicted values
#' @param act the actual observed values for the data set
#' @param pred the predicted probabilities for the data set
#' @keywords loss log multilogloss
#' @export
#' @examples
#' multi_log_loss(y, predict(model, newdata=x))
multi_log_loss <- function(act, pred)
{
  eps = 1e-15;
  nr <- nrow(pred)
  pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
  pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  ll = sum(act*log(pred) + (1-act)*log(1-pred))
  ll = ll * -1/(nrow(act))      
  return(ll);
}