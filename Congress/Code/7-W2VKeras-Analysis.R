rm(list = ls())

# ================================
# load libraries & functions
# ================================
library(keras)
library(reticulate)
library(purrr)
library(dplyr)
library(text2vec)
library(magrittr)
library(data.table)
library(LaplacesDemon)

# function to compute transition matrix
source("/Users/pedrorodriguez/Dropbox/Research/WordEmbeddings/US/General/ImportedEmbeddings/WordScoring/Code/transition_matrix.R")

# ================================
# set paths
# ================================
in_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Outputs/"
out_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Post-Estimation/Transitions/"

# ================================
# define parameters
# ================================
FOLDS <- 10

# ================================
# load data & average over embeddings
# ================================
embedding_matrix_R <- readRDS(paste0(in_path, "R", 1, "_E2_embedding_matrix.rds"))
embedding_matrix_D <- readRDS(paste0(in_path, "D", 1, "_E2_embedding_matrix.rds"))
embedding_matrix_F <- readRDS(paste0(in_path, "F", 1, "_E3_embedding_matrix.rds"))
embedding_matrix_M <- readRDS(paste0(in_path, "M", 1, "_E3_embedding_matrix.rds"))

for(i in 1:FOLDS){
  embedding_matrix_R <- embedding_matrix_M1 + readRDS(paste0(in_path, "R", i, "_E2_embedding_matrix.rds"))
  embedding_matrix_D <- embedding_matrix_M1 + readRDS(paste0(in_path, "D", i, "_E2_embedding_matrix.rds"))
  embedding_matrix_F <- embedding_matrix_M1 + readRDS(paste0(in_path, "F", i, "_E3_embedding_matrix.rds"))
  embedding_matrix_M <- embedding_matrix_M1 + readRDS(paste0(in_path, "M", i, "_E3_embedding_matrix.rds"))
}

embeddings_list <- list(embedding_matrix_R, embedding_matrix_D, embedding_matrix_F, embedding_matrix_M)

# spring cleaning
rm(embedding_matrix_R, embedding_matrix_D, embedding_matrix_F, embedding_matrix_M)
# ================================
# compute transition matrices
# ================================
transitions_matrices <- lapply(embeddings_list, function(i) transition_matrix(i, method = 'cosine', diagonal = 0))

# ================================
# compute KL-Divergence
# ================================
kl_divergence <- data.table(vocab = rownames(transitions_matrices[[1]]))
kl_divergence <- kl_divergence[, DR_divergence := unlist(lapply(kl_divergence$vocab, function(v) KLD(transitions_matrices[[1]][v,], transitions_matrices[[2]][v,])[["mean.sum.KLD"]]))]
kl_divergence <- kl_divergence[, FM_divergence := unlist(lapply(kl_divergence$vocab, function(v) KLD(transitions_matrices[[3]][v,], transitions_matrices[[4]][v,])[["mean.sum.KLD"]]))]
# test means difference
t.test(kl_divergence$DR_divergence, kl_divergence$FM_divergence)

# ================================
# explore nearest neighbors
# ================================
find_similar_words <- function(word, embedding_matrix, n = 10) {
  similarities <- embedding_matrix[word, , drop = FALSE] %>%
    sim2(embedding_matrix, y = ., method = "cosine")
  
  similarities[,1] %>% sort(decreasing = TRUE) %>% head(n)
}

token <- "healthcare"
find_similar_words(token, embedding_matrix_R)
find_similar_words(token, embedding_matrix_D)




