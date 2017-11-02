cosine_similarity <- function(x){
  # given that x is a matrix mxn, the output is a mxm matrix
  (x %*% t(x))/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))) 
}
