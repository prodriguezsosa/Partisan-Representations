rm(list = ls())

# ================================
# load libraries
# ================================
library(keras)
library(reticulate)
library(purrr)
library(dplyr)
library(text2vec)
library(magrittr)
library(data.table)
library(LaplacesDemon)
library(stringr)

# ================================
#
# STEP - 1
# COMPUTE MEAN MODEL
#
# ================================

# ================================
# define paths
# ================================
in_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Outputs/"

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
  embedding_matrix_R <- embedding_matrix_R + readRDS(paste0(in_path, "R", i, "_E2_embedding_matrix.rds"))
  embedding_matrix_D <- embedding_matrix_D + readRDS(paste0(in_path, "D", i, "_E2_embedding_matrix.rds"))
  embedding_matrix_F <- embedding_matrix_F + readRDS(paste0(in_path, "F", i, "_E3_embedding_matrix.rds"))
  embedding_matrix_M <- embedding_matrix_M + readRDS(paste0(in_path, "M", i, "_E3_embedding_matrix.rds"))
}

embeddings_list <- list("R" = embedding_matrix_R, "D" = embedding_matrix_D, "F" = embedding_matrix_F, "M" = embedding_matrix_M)

# ================================
#
# STEP - 2 
# COMPUTE KL-DIVERGENCE
#
# ================================

# function to compute transition matrix
source("/Users/pedrorodriguez/Drobox/GitHub/Partisan-Representations/Congress/Code/Post-Estimation/Best Model/transition_matrix.R")

# ================================
# compute transition matrices
# ================================
transitions_matrices <- lapply(embeddings_list, function(i) transition_matrix(i, method = 'cosine', diagonal = 0))

# ================================
# compute KL-Divergence
# ================================
kl_divergence <- data.table(vocab = rownames(transitions_matrices[[1]]))
kl_divergence <- kl_divergence[, RD_divergence := unlist(lapply(kl_divergence$vocab, function(v) KLD(transitions_matrices[["R"]][v,], transitions_matrices[["D"]][v,])[["mean.sum.KLD"]]))]
kl_divergence <- kl_divergence[, FM_divergence := unlist(lapply(kl_divergence$vocab, function(v) KLD(transitions_matrices[["F"]][v,], transitions_matrices[["M"]][v,])[["mean.sum.KLD"]]))]
# test means difference
t.test(kl_divergence$RD_divergence, kl_divergence$FM_divergence)




