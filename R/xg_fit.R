#' XG Fit
#'
#' This function iterates over the selected parameter space to find the minimal OOS loss using xgboost cross validation
#' @param y the target matrix
#' @param max_depth_vals the values for max_depth to take
#' @param min_child_weight_vals the values for min_child_weight to take
#' @param nrounds the number of trees to build and cross validate
#' @param subsample_vals the values for subsample to take
#' @param eta_vals the values for eta to take
#' @param colsample_bytree_vals the values for colsample_bytree to take
#' @keywords xgboost fit
#' @export
#' @examples
#' xg_fit()
xg_fit <- function(max_depth_vals=c(5), min_child_weight_vals = c(1), nrounds=200,
                   subsample_vals=c(0.6), eta_vals = c(0.3), colsample_bytree_vals = c(0.2),
                   metrics = c("logloss"),
                   ... )
{
  total_iterations = length(max_depth_vals) * length(min_child_weight_vals) *
    length(subsample_vals) * length(eta_vals) * length(colsample_bytree_vals)
  iteration = 0
  min_error_vals = data.frame(max_depth=c(0), min_child_weight=c(0), subsample=c(0),
                              eta=c(0), colsample_bytree=c(0), nrounds=c(0), 
                              error=c(Inf), error_sd=c(Inf))
  error_vals = data.frame(max_depth=vector(), min_child_weight=vector(), subsample=vector(),
                          eta=vector(), colsample_bytree=vector(), nrounds=vector(), 
                          error=vector(), error_sd=vector())
  
  for(max_depth in max_depth_vals){
    for(min_child_weight in min_child_weight_vals){
      for(subsample in subsample_vals){
        for(eta in eta_vals){
          for(colsample_bytree in colsample_bytree_vals){
            print("current min:")
            print(min_error_vals)
            
            params = list(
              "max.depth" = max_depth,
              "min_child_weight" = min_child_weight,
              "subsample" = subsample,
              "colsample_bytree" = colsample_bytree,
              "eta" = eta
            )
            
            print("fitting")
            print(params)
            print(paste("nrounds:", nrounds))
            
            bst <- xgb.cv(params=params,
                          metrics=metrics, nrounds=nrounds,
                          ...)
            #if 1sd above current error is less than 1sd above best error, replace
            if(bst$test.logloss.mean[which.min(bst$test.logloss.mean)] + bst$test.logloss.std[which.min(bst$test.logloss.mean)]
               < min_error_vals$error + min_error_vals$error_sd) {
              min_error_vals = data.frame(max_depth=c(max_depth), 
                                          min_child_weight=c(min_child_weight), subsample=c(subsample),
                                          eta=c(eta), colsample_bytree=c(colsample_bytree), 
                                          nrounds=c(which.min(bst$test.logloss.mean)),
                                          error=c(bst$test.logloss.mean[which.min(bst$test.logloss.mean)]),
                                          error_sd=c(bst$test.logloss.std[which.min(bst$test.logloss.mean)]))
              print("found new min:")
              print(min_error_vals)
            }
            error_vals = rbind(
              error_vals,
              data.frame(max_depth=c(max_depth), min_child_weight=c(min_child_weight), 
                         subsample=c(subsample), eta=c(eta), 
                         colsample_bytree=c(colsample_bytree), 
                         nrounds=c(which.min(bst$test.logloss.mean)), 
                         error=c(bst$test.logloss.mean[which.min(bst$test.logloss.mean)]),
                         error_sd=c(bst$test.logloss.std[which.min(bst$test.logloss.mean)]))
            )
            
            iteration = iteration + 1
            print(paste(100*iteration/total_iterations,"% completed", sep=""))
          }
        }
      }
    }
  }
  #train a tree based on min params
  print("you should train a tree using the following params:")
  print(min_error_vals)
  # return(xgboost(
  #   max.depth = min_error_vals$max_depth,
  #   min_child_weight = min_error_vals$min_child_weight,
  #   subsample = min_error_vals$subsample,
  #   eta = min_error_vals$eta,
  #   colsample_bytree = min_error_vals$colsample_bytree,
  #   nrounds = min_error_vals$nrounds,
  #   metrics=c("logloss"),
  #   ...
  # ))
  return(error_vals)
}


#' XG Train
#'
#' This function iterates over the selected parameter space to find the minimal OOS loss using xgboost cross validation
#' @param y the target matrix
#' @param max_depth_vals the values for max_depth to take
#' @param min_child_weight_vals the values for min_child_weight to take
#' @param nrounds the number of trees to build and cross validate
#' @param subsample_vals the values for subsample to take
#' @param eta_vals the values for eta to take
#' @param colsample_bytree_vals the values for colsample_bytree to take
#' @keywords xgboost fit
#' @export
#' @examples
#' xg_train()
xg_train <- function(train_target, train_data, test_target = NULL, test_clean = NULL, bst, ...){
  xgb_train <- sparse.model.matrix(
    train_target ~ . , 
    data=train_clean)[,-1]
  
  dtrain <- xgb.DMatrix(xgb_train, label=train_target)
  if(!is.null(test_target) && !is.null(test_clean)){
    xgb_test <- sparse.model.matrix(
      test_target ~ . , 
      data=test_clean)[,-1]
    
    dtest <- xgb.DMatrix(xgb_test, label=test_target)
  }
  
  build_params = list("objective" = "binary:logistic"
                      , "eval_metric" = "logloss"
                      , 'eta' = bst$eta
                      , 'max.depth' = bst$max_depth
                      , 'min_child_weight' = bst$min_child_weight
                      , 'subsample' = bst$subsample
                      , 'colsample_bytree' = bst$colsample_bytree,
                      ...)
  nrounds = bst$nrounds
  
  old = Sys.time()
  print("training...")
  
  build_params <- append(build_params, list(...))
  
  if(!is.null(test_target) && !is.null(test_clean)){
    watchlist <- list(train = dtrain, test = dtest)
  } else {
    watchlist <- list(train = dtrain)
  }
  
  xgb_model <- xgb.train(param=build_params, data = dtrain, nrounds=nrounds, watchlist=watchlist, 
                         verbose = 2)
  
  # xgb_model <- xgboost(
  #     data=xgb_train,
  #     label = train_target,
  #     params = build_params,
  #     verbose=2,
  #     nrounds = nrounds,
  #     ...
  #   )
  
  print(Sys.time() - old)
  print("predicting")
  print("full sample")
  pred_in_xgb <- as.data.frame(predict(xgb_model, xgb_train))[,1]
  print(multi_log_loss(act=data.frame(train_target), pred=rbind(pred_in_xgb)))
  
  return(xgb_model)
}
