#' Find kurtosis of a distribution
#'
#' This function takes in a set of values from a gaussian distribution and returns the kurtosis of that distribution
#' @param x the data to find the kurtosis of
#' @keywords kurtosis
#' @export
#' @examples
#' kurtosis(x)
kurtosis = function(x) {
  x = na.omit(x)
  x.bar = mean(x)
  N = length(x)
  sum((x - x.bar)^4)/((N - 1) * sd(x)^4)
}