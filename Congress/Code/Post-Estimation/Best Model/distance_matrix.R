library(text2vec)
distance_matrix <- function(word_embeddings, method, diagonal = NA){
  # compute distance matrix
  dist_matrix <- sim2(x = word_embeddings, y = word_embeddings, method = method)
  dist_matrix[dist_matrix > 1] <- 1
  dist_matrix <- 1 - acos(-dist_matrix)/pi
  diag(dist_matrix) <- diagonal
  # add names
  colnames(dist_matrix) <- rownames(dist_matrix) <- rownames(word_embeddings)
  return(dist_matrix)
}