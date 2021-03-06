% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jnn.R
\name{jnn}
\alias{jnn}
\title{Jeneral Neural Net}
\usage{
jnn(X, Y, type = "basic", activation = "sigmoid", cost = "logit",
  optimization = "sgd", neurons = c(4, 6), step = "const", alpha = 0.01,
  iterations = 1000, ...)
}
\arguments{
\item{X}{the input matrix}

\item{Y}{the output input matrix}
}
\description{
This function is a custom implementation of a neural network
}
\examples{
jnn(X, Y)
}
\keyword{jeneral}
\keyword{net}
\keyword{network}
\keyword{neural}

