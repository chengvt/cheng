#' Calculate SR from mvr
#' 
#' Mostly edited and made to work by Y. Uwadaira of \code{plsropt} package. 
#' Validated with the values from MATLAB's PLS toolbox v8.1.1.
#' 
#' @param mvr pls model from pls package
#' @param ncomp ncomp 
#' 
#' @export

SR <- function(mvr, ncomp = mvr$ncomp){

  # X-variable
  X <- mvr$model[[2]]
  X <- apply(X, 2, function(x) x - mean(x)) # mean-centering
  if(!is.null(mvr$scale)){
    X <- apply(X, 2, function(x) x/sd(x)) # scaling
  }
  
  # projection of the rows of X onto the normalized regression coefficients vector
  # t_TP is proportional on the predicted values, yhat
  b <- as.matrix(mvr$coefficients[, 1, ncomp])
  t_TP <- X %*% (b/norm(b))
  
  # loading, p_TP, are obtained by projecting the columns of X onto the obtained score vector, t_TP
  p_TP <- t(X) %*% (t_TP/c(t(t_TP) %*% t_TP))

  SelR <- c()
  for (i in 1:ncol(X)){
    E_TPi <- as.matrix(X[, i]) - as.matrix(p_TP[i] * t_TP)
    V_ex <- var(t_TP %*% p_TP[i]) # explained variance
    V_re <- var(E_TPi) # residual variance
    
    SelR <- c(SelR, V_ex/V_re)
  }
  return(SelR)
}
