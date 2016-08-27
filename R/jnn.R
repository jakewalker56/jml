#' Jeneral Neural Net
#'

#Any nonlinear transform from inputs to outputs can be be treated as a network node IFF you can forward propagate inputs (trivial) and back propagate errors (requires approximate differentiability).  Backpropogating errors within the node itself is optional- we can choose to hold the node constant, or to modify it as we train.  If we choose to hold it constant, we do not require differentiability - we may simply treat its output as constant input to the rest of the model.  It does mean we can't backpropogate errors through that network, so stacking differentiable and non-differentiable elements doesn't make a ton of sense.

#For any Engine structure, we can retrain from scratch, or we can retrain from our current weights, which should make us much faster, but may also lock us into local optima.


#' This function is a custom implementation of a neural network
#' @param X the input matrix
#' @param Y the output input matrix
#' @keywords jeneral neural net network
#' @export
#' @examples
#' jnn(X, Y)
jnn <- function(neurons, predictors, responses, activations=NULL, gradients=NULL, ...){
  #define structure of the network
  
  #weights is a 3 dimensional array
  #index 1 is which layer we are in
  #index 2 is which neuron in that layer we are looking at
  #index 3 is which neuron in the previous layer we are associated with
  
  nn <- list( 
    predictors = predictors,
    responses = responses,
    weights = NULL,
    bias = NULL, 
    activations= activations,
    gradients= gradients,
    trainingIterations = 0
  ) 
  
  ## Set the name for the class 
  class(nn) <- append(class(nn),"jnn")
  
  graph_weight <- vector()
  
  nn$bias = list()
  nn$weights = list()
  
  for(i in 1:(length(neurons) + 1)){
    nn$weights[[i]] = list()
    nn$bias[[i]] = list()
    if(i > length(neurons)){
      #these are the weights from the last layer to the output 
      for(j in 1:ncol(responses)){
        nn$weights[[i]][[j]] = matrix(rep(1, length(nn$weights[[i-1]])), nrow=length(nn$weights[[i-1]]))
        nn$bias[[i]][[j]] = runif(min=-1, max=1, 1)
      }
    } else {
      for(j in 1:neurons[i]){
        nn$bias[[i]][[j]] = runif(min=-1, max=1, 1)
        if (i > 1) {
          nn$weights[[i]][[j]] = matrix(rep(0, length(nn$weights[[i-1]])), nrow=length(nn$weights[[i-1]]))
        }
        else {
          #these are the weights assoociated with the inputs
          nn$weights[[i]][[j]] = matrix(rep(0, ncol(predictors)), nrow=ncol(predictors))
        }
      }
    }
  }
  
  return(nn) 
}

#' This function is a custom implementation of a neural network
#' @param nn the current state of the neural network (a jnn object)
#' @param X the input matrix
#' @param Y the output input matrix
#' @param alphas the per-layer update parameter
#' @keywords jeneral neural net network
#' @export
#' @examples
#' jnn(X, Y)
train.jnn <- function(nn, X, Y, iterations, alphas=NULL) {
  #train the network
  #iterate through X and Y and train our network
  
   #for sgd, need to set batch size
  batch_size = 1
  
  for(iteration in 1:iterations){
    if(iteration %% 1000 == 0){
      print("Iteration:")
      print(iteration)
    }
    
    #graph_weight <- c(graph_weight, nn$weights[[1]][[1]])
    
    #consume 1 random observation
    #runif returns float not int! index = runif(1, min=1, max=nrow(Y))
    index = sample(1:nrow(Y), batch_size)
    #print(c("training on: ",index))
    x = X[index,]
    y = Y[index,]
    
    layers = forward_propogate_values(nn, x)
    
    #backpropogate gradient through network
    #to backpropogate errors, we use the chain rule.  
    #to find the derivative of the error with respect to a weight, we first find the partial derivative 
    #of the error with respect to the activation function that our weight touches, and multiply it by
    #the partial derivative of that activation funtion with respect to our weight
    
    #we do this using dynamic programming to build an array of partial derivatives for each
    #node value.  To find our weight partials, we will then iterate through that and apply
    #take the partial derivative at that node with respect to the weight
    
    node_partials = list()
    
    #the last node partial slot describes the partial derivative of the error function
    #with respect to our outpus
    node_partials[[length(layers)]] = list()
    for(j in 1:length(layers[length(layers)])){
      #assuming a loss function of y(1-y^) + (1-y)y^
      #dLoss/dy^ = 1-2y.  if y = 0, derivative is 1, else y = 1, derivative is -1
      #node_partials[[length(layers)]][[j]] = (1-2*y[j])
      
      #actually that's a terrible loss function.  It turns out that is pushing us to
      #predict everything as a 1, because at each individual step it is locally
      #beneficial to push all outputs to 1, even if the global optimum is not that
      
      #put it another way: when optimizing for just loss, if you have more 1's than
      #0's, then our aggregate loss function is made better off if we increase
      #the predicted values by 0.01 for every set of inputs.  This unfortunately
      #can result in runaway optimization, where one of the layers updates faster
      #than the others and leads us to just predict 1 for everything.  This is hard
      #to recover from, because sigmoid prime by its nature multiplies a number 
      #between 1 and -1 by exp(-(linear combination of inputs, weights and biases)).
      #unless your linear combination evaluates to a negative number, sigmoid prime
      #is always < 1, meaning earlier layers (closer to inputs) are updated more
      #slowly than later layers. So this scheme will be particularly bad at 
      #correcting when we have more 1's than 0's in our training set.
      
      #lets try to set error function to the square of the loss
      y1 = y[j] #observed value from data
      y2 = layers[[length(layers)]][[j]] #fitted value from nn output
      node_partials[[length(layers)]][[j]] = quadratic_loss_prime(y1, y2)
    }
    for(i in (length(layers) - 1):1){
      #find the partial derivative of the error function with respect to each node
      
      #Note that we don't actually care about the input nodes, but we calculate them because
      #it's easier than special casing it
      
      node_partials[[i]] = list()
      j = 1
      for(j in 1:length(layers[[i]])){
        #node_partials[[i]][[j]] is the partial derivative of the error with respect to 
        #the value of node j in layer i
        
        #this is equal to the sum of (the derivatives of the error with respect 
        #to the nodes layer i + 1, times the derivative of those node values with
        #respect to node j in layer i)
        
        #the partial derivate of a node n in layer i + 1 with respect to node j in layer i is 
        #equal to the derivative of node n with respect to the activation function of node n times
        #the derivative of the activation function with respect to node j
        
        #if we're using sigmoid as the activation function, and a linear combination of inputs 
        #as the input to the sigmoid function, this yields sigmoid_prime * w, where w is the weight
        #between node j and node n
        val = 0
        for(k in 1:length(layers[[i+1]])){
          #k is the number of nodes in the next layer (assuming fully connected layers)
          val = val + nn$gradients[i][[1]](matrix(layers[[i]], nrow=1), as.numeric(nn$weights[[i]][[k]]), nn$bias[[i]][[k]]) * 
                      nn$weights[[i]][[k]][j] * 
                      node_partials[[i+1]][[k]]    
        }
        node_partials[[i]][[j]] = val
      }
      #step the weights in the direction of the derivative
      #calculate the derivative
      #modify the weights and bias
    }
    
    # print(paste(c("Node Partials:", node_partials)))
    # print(paste(c("Weights:", weights)))
    # print(paste(c("Bias:", bias)))
    # print(paste(c("Layers:", layers)))
    
    #Now use the activation partials and alpha to update the weights
    for(i in 1:length(nn$weights)){
      for(j in 1:length(nn$weights[[i]])){
        for(k in 1:length(nn$weights[[i]][[j]])) {
          #update the weight by the step size times the partial derivative.  Note this is very similar
          #to our node_partial calculation, except that d/dWeight is different than d/dX (we multiply
          #by layer value instead of by weight value)
          nn$weights[[i]][[j]][k] = nn$weights[[i]][[j]][k] - alphas[i] * 
            nn$gradients[i][[1]](matrix(layers[[i]], nrow=1), as.matrix(nn$weights[[i]][[j]]), nn$bias[[i]][[j]]) * 
            layers[[i]][k] *
            node_partials[[i+1]][[j]]
        }
        nn$bias[[i]][[j]] = nn$bias[[i]][[j]] - alphas[i] * 
          nn$gradients[i][[1]](matrix(layers[[i]], nrow=1), as.matrix(nn$weights[[i]][[j]]), nn$bias[[i]][[j]]) * 
          node_partials[[i+1]][[j]]
      }
    }
    #print(paste(c("Updated Weights:", weights)))
    
  }
  #plot(graph_weight)
  #update number of iterations run (so we can have a scaling alpha)
  
  nn$trainingIterations = nn$trainingIterations + iterations
  return(nn)
}


forward_propogate_values.jnn <- function(nn, x) {
  layers = list()
  layers[[1]] = as.matrix(x)
  #calculate values of each node in each layer based on input
  for(i in 1:length(nn$weights))
  {
    layers[[i+1]] = array(rep(0, length(nn$weights[[i]])))
    for(j in 1:length(nn$weights[[i]])) {
      #value of node i + 1, j is weights at node i, j times value at nodes i, plus bias, then 
      #run through sigmoid function
      layers[[i+1]][j] =  nn$activations[i][[1]](layers[[i]], nn$weights[[i]][[j]], nn$bias[[i]][[j]])
    }
  }
  return(layers)
} 

print.jnn <- function(nn){
  #todo: make it a pretty graph
  print(nn$weights)
  print(nn$bias)
}

predict.jnn <- function(nn, x){
  layers = forward_propogate_values(nn, x)
  return(layers[[length(layers)]])
}

sigmoid <- function(x, w, bias) {
  return(1/(1 + exp(-1 * (x %*% w + bias))))
} 
sigmoid_prime <- function(x, w, bias) {
  #print(paste(c("W:", w, "X:", x)))
  f = sigmoid(x, w, bias)
  return(f * f * exp(-1 * (x %*% w + bias)))
}

RELU <- function(x, w, bias) {
  lin = linear(x, w, bias)
  if(lin > 0) {
    return(lin)
  }
  return(0)
} 
RELU_prime <- function(x, w, bias) {
  rel = RELU(x,w,bias)
  #if(rel >= 0) {
    return(linear_prime(x,w,bias))
  #}
  #return(0)
}
linear <- function(x, w, bias) {
  return(x %*% w + bias)
} 
linear_prime <- function(x, w, bias) {
  return(1)
}
quadratic_loss <- function(act, pred) {
  return((act - pred)^2)
}
quadratic_loss_prime <- function(act, pred){
  return(-2*(act-pred))
}

X = data.frame(input_1=c(0.5, 1.5, 1.0), input_2 = c(2, 1, 0.5))
Y = data.frame(output=c(0, 1, 1))

neurons=c(1,1)

alphas = c(1, 0.1, 0.1)
activations=c(sigmoid, linear, linear)
gradients=c(sigmoid_prime, linear_prime, linear_prime)

nn = jnn(neurons = neurons, 
              activations=activations, 
              gradients=gradients)
nn = train(nn, X, Y, alphas=alphas, iterations = 6000)

print(nn)

predict(nn, weights, bias, activations, X[1,])
predict(nn, weights, bias, activations, X[2,])
predict(nn, weights, bias, activations, X[3,])
