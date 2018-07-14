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
library(Matrix)
library(pbapply)

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
# COMPUTE OVERLAP
#
# ================================

# ================================
# set paths
# ================================
in_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Outputs/"

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

# setdiff function
#seeds <- list("abortion", "welfare", "healthcare", "conservative", "liberal", "freedom", "taxes", "immigrants", "equality")
ContextDiff <- function(seeds, embed1, embed2, N){
  context1 <- pblapply(seeds, function(w) names(closest_neighbors(embeddings_list[[embed1]][w,], embeddings_list[[embed1]], N)))
  context2 <- pblapply(seeds, function(w) names(closest_neighbors(embeddings_list[[embed2]][w,], embeddings_list[[embed2]], N)))
  setdiff1 <- pblapply(seq(1:length(seeds)), function(x) setdiff(context1[[x]], context2[[x]]))
  setdiff2 <- pblapply(seq(1:length(seeds)), function(x) setdiff(context2[[x]], context1[[x]]))
  names(setdiff1) <- names(setdiff2) <- seeds
  return(list(setdiff1, setdiff2))
}

# apply function
setdiffRD <- ContextDiff(seeds = rownames(embeddings_list[["R"]]), embed1 = "R", embed2 = "D", 6)
setdiffFM <- ContextDiff(seeds = rownames(embeddings_list[["F"]]), embed1 = "F", embed2 = "M", 6)

# overlap function
#seeds <- list("abortion", "welfare", "healthcare", "conservative", "liberal", "freedom", "taxes", "immigrants", "equality")
ContextOverlapStat <- function(seeds, embed1, embed2, N){
  context1 <- pblapply(seeds, function(w) names(closest_neighbors(embeddings_list[[embed1]][w,], embeddings_list[[embed1]], N)))
  context2 <- pblapply(seeds, function(w) names(closest_neighbors(embeddings_list[[embed2]][w,], embeddings_list[[embed2]], N)))
  overlap <- pblapply(seq(1:length(seeds)), function(x) length(intersect(context1[[x]], context2[[x]]))/N)
  return(data.table(token = unlist(seeds), overlap = unlist(overlap)))
}

# apply function
OverlapStatRD <- ContextOverlapStat(seeds = rownames(embeddings_list[["R"]]), embed1 = "R", embed2 = "D", 6)
OverlapStatFM <- ContextOverlapStat(seeds = rownames(embeddings_list[["F"]]), embed1 = "F", embed2 = "M", 6)

# overlap function
#seeds <- list("abortion", "welfare", "healthcare", "conservative", "liberal", "freedom", "taxes", "immigrants", "equality")
ContextOverlap <- function(seeds, embed1, embed2, N){
  context1 <- pblapply(seeds, function(w) names(closest_neighbors(embeddings_list[[embed1]][w,], embeddings_list[[embed1]], N)))
  context2 <- pblapply(seeds, function(w) names(closest_neighbors(embeddings_list[[embed2]][w,], embeddings_list[[embed2]], N)))
  overlap <- pblapply(seq(1:length(seeds)), function(x) intersect(context1[[x]], context2[[x]]))
  names(overlap) <- seeds
  return(overlap)
}

# apply function
OverlapRD <- ContextOverlap(seeds = rownames(embeddings_list[["R"]]), embed1 = "R", embed2 = "D", 6)
OverlapFM <- ContextOverlap(seeds = rownames(embeddings_list[["F"]]), embed1 = "F", embed2 = "M", 6)

# ================================
# differences between both covariates
# ================================
# explore results
seed <- "conservative"
overlapRD[token == seed,]
overlapFM[token == seed,]
# explore differences
overlap <- cbind(overlapRD, overlapFM[,2]) %>% set_colnames(c("token", "RD", "FM"))
overlap <- overlap[, covariate_diff := RD - FM]




