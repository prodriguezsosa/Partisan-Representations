rm(list = ls())

# ================================
# load libraries & functions
# ================================
library(text2vec)
library(magrittr)
library(data.table)
library(stringr)
library(Matrix)
library(tidyverse)

# ================================
#
# STEP - 1
# IDENTIFY BEST MODELS
#
# ================================

# ================================
# define paths
# ================================
in_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Post-Estimation/Loss/"

# ================================
# load data
# ================================
files <- list.files(in_path)
loss_list <- lapply(files, function(i) readRDS(paste0(in_path, i)))

# ================================
# choose best model for each group
# ================================
best_models_labels <- list()  # empty list
# for each MODEL-TEST select model with lowest mean loss
for(i in 1:length(loss_list)){
  loss_folds <- loss_list[[i]]
  mean_loss_folds <- unlist(lapply(loss_folds, mean))
  best_model_name <- names(which(mean_loss_folds == min(mean_loss_folds)))
  best_models_labels[i] <- best_model_name
  model_label <- gsub('[0-9]+', '', best_model_name)
}

# spring cleaning 
rm(loss_folds, loss_list, best_model_name, files, i, in_path, mean_loss_folds, model_label)

# ================================
#
# STEP - 2
# NEAREST NEIGHBORS FOR KEY WORDS
#
# ================================

# ================================
# set paths
# ================================
in_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Outputs/"
out_path <- "/Users/pedrorodriguez/Drobox/GitHub/Partisan-Representations/Congress/Code/Post-Estimation/Human Validation/data/"

# ================================
# load embeddings
# ================================
embeddings_list <- list()
embeddings_list[["R"]] <- readRDS(paste0(in_path, "R", gsub("[[:alpha:]]", "", best_models_labels[grepl("RR", best_models_labels)][[1]]), "_E2_embedding_matrix.rds"))
embeddings_list[["D"]] <- readRDS(paste0(in_path, "D", gsub("[[:alpha:]]", "", best_models_labels[grepl("DD", best_models_labels)][[1]]), "_E2_embedding_matrix.rds"))
embeddings_list[["F"]] <- readRDS(paste0(in_path, "F", gsub("[[:alpha:]]", "", best_models_labels[grepl("FF", best_models_labels)][[1]]), "_E3_embedding_matrix.rds"))
embeddings_list[["M"]] <- readRDS(paste0(in_path, "M", gsub("[[:alpha:]]", "", best_models_labels[grepl("MM", best_models_labels)][[1]]), "_E3_embedding_matrix.rds"))

# ================================
# nearest neighbors
# ================================
# cosine distance function
closest_neighbors <- function(seed, embed_matrix, num_neighbors){
  seed <- Matrix(seed, nrow = 1, ncol = length(seed))
  mat_magnitudes <- rowSums(embed_matrix^2)
  vec_magnitudes <- rowSums(seed^2)
  sim <- (t(tcrossprod(seed, embed_matrix)/(sqrt(tcrossprod(vec_magnitudes, mat_magnitudes)))))
  sim2 <- matrix(sim, dimnames = list(rownames(sim)))
  w <- sim2[order(-sim2), , drop = FALSE]
  w[1:num_neighbors,]
}

# top words mentioned
seeds <- list("abortion", "welfare", "healthcare", "conservative", "liberal", "freedom", "taxes", "immigrants", "equality")

# loop over groups
nearest_neighbors <- list()
for(i in c("R", "D", "M", "F")){
  cos_sim_seeds <- lapply(seeds, function(w) closest_neighbors(embeddings_list[[i]][w,], embeddings_list[[i]], 10))
  names(cos_sim_seeds) <- seeds
  results <- as.tibble(lapply(cos_sim_seeds, names))
  results <- melt(results, measure.vars = colnames(results))
  colnames(results) <- c("seed", i)
  nearest_neighbors[[i]] <- results
}

# rbind data
nearest_neighbors <- do.call(cbind, nearest_neighbors) %>% .[,c(1,2,4,6,8)] %>% set_colnames(c("seed", "R", "D", "F", "M"))


