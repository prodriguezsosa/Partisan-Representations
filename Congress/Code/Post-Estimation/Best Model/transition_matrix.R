library(text2vec)
transition_matrix <- function(word_embeddings, method, diagonal = 0){
  # compute distance matrix
  dist_matrix <- sim2(x = word_embeddings, y = word_embeddings, method = method)
  dist_matrix[dist_matrix > 1] <- 1
  dist_matrix <- acos(-dist_matrix)/pi
  diag(dist_matrix) <- 0
  # transition matrix
  transition_matrix <- matrix(0, nrow = nrow(dist_matrix), ncol = ncol(dist_matrix))
  for(i in 1:nrow(dist_matrix)){
    transition_matrix[i,] <- dist_matrix[i,]/sum(dist_matrix[i,])
  }
  diag(transition_matrix) <- diagonal
  # add names
  colnames(transition_matrix) <- rownames(transition_matrix) <- rownames(word_embeddings)
  return(transition_matrix)
}