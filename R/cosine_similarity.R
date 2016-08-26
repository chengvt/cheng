cosine_similarity <- function(x){
  (x %*% t(x))/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))) 
}
