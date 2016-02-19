#' Print Formula From Coefficients
#'
#' This function prints the fully expanded formula from the coeficients
#' @param coefs the coefficients to be printed
#' @keywords formula coefficients coef print
#' @export
#' @examples
#' print_formula_from_coefs(reg$coefs)
print_formula_from_coefs <- function(coefs) {
  myFormula = ""
  for(i in 1:length(coefs)) {
    if(names(coefs)[i] != "(Intercept)") {
      myFormula = paste(myFormula, names(coefs)[i], sep="")
      myFormula = paste(myFormula, "*", sep="")
    }
    myFormula = paste(myFormula, coefs[i], sep="")
    if(i != length(coefs)) {
      myFormula = paste(myFormula, " + ", sep="")
    }
  }
  return(myFormula)
}
