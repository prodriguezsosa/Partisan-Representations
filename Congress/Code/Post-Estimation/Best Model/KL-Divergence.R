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
best_models_loss <- list()  # empty list
# for each MODEL-TEST select model with lowest mean loss
for(i in 1:length(loss_list)){
  loss_folds <- loss_list[[i]]
  mean_loss_folds <- unlist(lapply(loss_folds, mean))
  best_model_name <- names(which(mean_loss_folds == min(mean_loss_folds)))
  best_models_labels[i] <- best_model_name
  model_label <- gsub('[0-9]+', '', best_model_name)
  best_models_loss[model_label] <- do.call(c, loss_list)[best_model_name]
  
}

# keep only within models
best_models_loss <- lapply(list("RR", "DD", "MM", "FF"), function(i) best_models_loss[i]) %>% do.call(c,.)

# spring cleaning 
rm(loss_folds, loss_list, best_model_name, files, i, in_path, mean_loss_folds, model_label)

# ================================
#
# STEP - 2 
# COMPUTE KL-DIVERGENCE
#
# ================================

# function to compute transition matrix
source("/Users/pedrorodriguez/Drobox/GitHub/Partisan-Representations/Congress/Code/Post-Estimation/Best Model/transition_matrix.R")

# ================================
# set paths
# ================================
in_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Outputs/"
out_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Post-Estimation/Transitions/"

# ================================
# load embeddings
# ================================
embeddings_list <- list()
embeddings_list[["R"]] <- readRDS(paste0(in_path, "R", gsub("[[:alpha:]]", "", best_models_labels[grepl("RR", best_models_labels)][[1]]), "_E2_embedding_matrix.rds"))
embeddings_list[["D"]] <- readRDS(paste0(in_path, "D", gsub("[[:alpha:]]", "", best_models_labels[grepl("DD", best_models_labels)][[1]]), "_E2_embedding_matrix.rds"))
embeddings_list[["F"]] <- readRDS(paste0(in_path, "F", gsub("[[:alpha:]]", "", best_models_labels[grepl("FF", best_models_labels)][[1]]), "_E3_embedding_matrix.rds"))
embeddings_list[["M"]] <- readRDS(paste0(in_path, "M", gsub("[[:alpha:]]", "", best_models_labels[grepl("MM", best_models_labels)][[1]]), "_E3_embedding_matrix.rds"))

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




