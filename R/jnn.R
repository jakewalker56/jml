#' Jeneral Neural Net
#'
#' This function is a custom implementation of a neural network
#' @param X the input matrix
#' @param Y the output input matrix
#' @keywords jeneral neural net network
#' @export
#' @examples
#' jnn(X, Y)
jnn <- function(X, Y, type="basic", activation="sigmoid", cost="logit", 
                optimization="sgd", neurons=c(4, 6), step="const", alpha=0.01,
                iterations=1000,
                ...){
  #custom implementation to train a neural network based on X and Y
  use_sgd <- optimization == "sgd"
  
  X = matrix(data=runif(10),ncol=5)
  weights = list(rep(1,ncol(X)))
  bias = list(1)
  batch_size = 1
  
  #create weight and bias (intercept) arrays from neuron vector
  for(neuron_count in neurons){
    weights= c(weights, list(rep(1,neuron_count)))
    bias = c(bias, list(1))
  }

  for(iteration in 1:iterations){
    if(use_sgd){
      #consume 1 random observation
      index = runif(1, min=1, max=nrow(Y))
      x = X[index,]
      y = Y[index,]
      
      layer_values = list(X)
      #calculate values of each node in each layer
      for(i in length(weights))
      {
        layer = weights[i,] %*% layer_values[i,] + matrix(rep(bias[i,],batch_size))
        layer_values = list(layer_values, layer)
        
        
      }
      #backpropogate gradient through network
      for(i in length(weights):1){
        #calculate the derivative
        #modify the weights and bias
      }
    }
  }
}
