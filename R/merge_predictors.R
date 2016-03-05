#' Merge Predictors
#'
#' This function merges multiple predictors to find the optimal combination for prediction
#' @param y the target matrix
#' @param pred1 the matrix of predictions given by the first model
#' @param pred2 the matrix of predictions given by the second model
#' @keywords merge predict predictor
#' @export
#' @examples
#' merge(y, predict(model1, newdata=x), predict(model2, newdata=x))
merge_predictors <- function(y, pred1, pred2){
  min_loss = Inf
  min_i = 0
  for(i in seq(from=0, to=1, length.out=101)){
    loss = multi_log_loss(y, pred1* i + pred2*(1-i))
    if(loss < min_loss){
      min_loss = loss
      min_i = i
    }
  }
  print(min_loss)
  return(min_i)
}