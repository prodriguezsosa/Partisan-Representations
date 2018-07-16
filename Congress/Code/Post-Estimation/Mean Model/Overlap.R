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
# DEFINE DISTANCE THRESHOLDS
#
# ================================
source("/Users/pedrorodriguez/Drobox/GitHub/Partisan-Representations/Congress/Code/Post-Estimation/Best Model/distance_matrix.R") # distance-matrix function

# distance matrices
distance_matrices <- pblapply(c("R", "D", "F", "M"), function(x) distance_matrix(embeddings_list[[x]], method = 'cosine', diagonal = NA)) 
names(distance_matrices) <- c("R", "D", "F", "M")

# compute distance threshold
distanceThreshold <- function(distance_matrix, group, percentile = 0.01){
  threshold <- unname(quantile(distance_matrix[lower.tri(distance_matrix, diag = FALSE)], percentile))
  return(threshold)
}

dist_thresholds <- pblapply(c("R", "D", "F", "M"), function(x) distanceThreshold(distance_matrices[[x]], group = x, percentile = 0.01)) 
names(dist_thresholds) <- c("R", "D", "F", "M")

# ================================
#
# STEP - 3
# COMPUTE OVERLAP & SETDIFF
# INDIVIDUAL SEEDS
#
# ================================

# ================================
# nearest neighbors
# ================================
vocab <- rownames(embeddings_list[["R"]])
# cosine distance function
closest_neighbors <- function(seed, distance_matrix, num_neighbors = NULL, threshold = NULL){
  w <- distance_matrix[seed,]
  w <- w[order(w)]
  if(!is.null(threshold)){return(w[w <= threshold])}
  if(!is.null(num_neighbors)){return(w[1:num_neighbors])}
}

# setdiff function
#seeds <- list("abortion", "welfare", "healthcare", "conservative", "liberal", "freedom", "taxes", "immigrants", "equality")
ContextDiff <- function(seeds, dist_matrix1, dist_matrix2, N = NULL, threshold1 = NULL, threshold2 = NULL, label1 = NULL, label2 = NULL){
  context1 <- pblapply(seeds, function(w) names(closest_neighbors(w, dist_matrix1, num_neighbors = N, threshold = threshold1)))
  context2 <- pblapply(seeds, function(w) names(closest_neighbors(w, dist_matrix2, num_neighbors = N, threshold = threshold2)))
  setdiff1 <- pblapply(seq(1:length(seeds)), function(x) setdiff(context1[[x]], context2[[x]]))
  setdiff2 <- pblapply(seq(1:length(seeds)), function(x) setdiff(context2[[x]], context1[[x]]))
  names(setdiff1) <- names(setdiff2) <- seeds
  setdiff_list <- list(setdiff1, setdiff2) 
  names(setdiff_list) <- c(label1, label2)
  return(setdiff_list)
}

# apply function
#setdiffFM <- ContextDiff(seeds = rownames(embeddings_list[["F"]]), dist_matrix1 = distance_matrices[["F"]], dist_matrix2 = distance_matrices[["M"]], threshold1 = dist_thresholds[["F"]], threshold2 = dist_thresholds[["M"]])
setdiffRD <- ContextDiff(seeds = vocab, dist_matrix1 = distance_matrices[["R"]], dist_matrix2 = distance_matrices[["D"]], N = 10, label1 = "R", label2 = "D")
setdiffFM <- ContextDiff(seeds = vocab, dist_matrix1 = distance_matrices[["F"]], dist_matrix2 = distance_matrices[["M"]], N = 10, label1 = "F", label2 = "M")

# overlap function
#seeds <- list("abortion", "welfare", "healthcare", "conservative", "liberal", "freedom", "taxes", "immigrants", "equality")
ContextOverlapStat <- function(seeds, dist_matrix1, dist_matrix2, N = NULL, threshold1 = NULL, threshold2 = NULL){
  context1 <- pblapply(seeds, function(w) names(closest_neighbors(w, dist_matrix1, num_neighbors = N, threshold = threshold1)))
  context2 <- pblapply(seeds, function(w) names(closest_neighbors(w, dist_matrix2, num_neighbors = N, threshold = threshold2)))
  overlap <- pblapply(seq(1:length(seeds)), function(x) length(intersect(context1[[x]], context2[[x]]))/N)
  return(data.table(token = unlist(seeds), overlap = unlist(overlap)))
}

# apply function
OverlapStatRD <- ContextOverlapStat(seeds = vocab, dist_matrix1 = distance_matrices[["R"]], dist_matrix2 = distance_matrices[["D"]], N = 10)
OverlapStatFM <- ContextOverlapStat(seeds = vocab, dist_matrix1 = distance_matrices[["F"]], dist_matrix2 = distance_matrices[["M"]], N = 10)

# overlap function
#seeds <- list("abortion", "welfare", "healthcare", "conservative", "liberal", "freedom", "taxes", "immigrants", "equality")
ContextOverlap <- function(seeds, dist_matrix1, dist_matrix2, N = NULL, threshold1 = NULL, threshold2 = NULL){
  context1 <- pblapply(seeds, function(w) names(closest_neighbors(w, dist_matrix1, num_neighbors = N, threshold = threshold1)))
  context2 <- pblapply(seeds, function(w) names(closest_neighbors(w, dist_matrix2, num_neighbors = N, threshold = threshold2)))
  overlap <- pblapply(seq(1:length(seeds)), function(x) intersect(context1[[x]], context2[[x]]))
  names(overlap) <- seeds
  return(overlap)
}

# apply function
OverlapRD <- ContextOverlap(seeds = vocab, dist_matrix1 = distance_matrices[["R"]], dist_matrix2 = distance_matrices[["D"]], N = 10)
OverlapFM <- ContextOverlap(seeds = vocab, dist_matrix1 = distance_matrices[["F"]], dist_matrix2 = distance_matrices[["M"]], N = 10)

# ================================
# explore differences
# ================================
#seeds <- list("abortion", "welfare", "healthcare", "conservative", "liberal", "freedom", "taxes", "immigrants", "equality")
seed <- "taxes"
# setdiff tokens
lapply(list("R", "D"), function(x) setdiffRD[[x]][[seed]])
lapply(list("F", "M"), function(x) setdiffFM[[x]][[seed]])

# overlap tokens
OverlapRD[[seed]]
OverlapFM[[seed]]

# overlap statistic
OverlapStatRD[token == seed, overlap]
OverlapStatFM[token == seed, overlap]

# ================================
#
# STEP - 4
# COMPUTE OVERLAP & SETDIFF
# TOPICS
#
# ================================

# topic differences
TopicDiff <- function(topic, dist_matrix1, dist_matrix2, N = NULL, threshold1 = NULL, threshold2 = NULL, label1 = NULL, label2 = NULL){
  context1 <- pblapply(topic, function(w) names(closest_neighbors(w, dist_matrix1, num_neighbors = N, threshold = threshold1))) %>% unlist()
  context2 <- pblapply(topic, function(w) names(closest_neighbors(w, dist_matrix2, num_neighbors = N, threshold = threshold2))) %>% unlist()
  setdiff1 <- setdiff(context1, context2)
  setdiff2 <- setdiff(context2, context1)
  setdiff_list <- list(setdiff1, setdiff2) 
  names(setdiff_list) <- c(label1, label2)
  return(setdiff_list)
}

# topic overlap
TopicOverlap <- function(topic, dist_matrix1, dist_matrix2, N = NULL, threshold1 = NULL, threshold2 = NULL){
  context1 <- pblapply(topic, function(w) names(closest_neighbors(w, dist_matrix1, num_neighbors = N, threshold = threshold1))) %>% unlist()
  context2 <- pblapply(topic, function(w) names(closest_neighbors(w, dist_matrix2, num_neighbors = N, threshold = threshold2))) %>% unlist()
  overlap <- intersect(context1, context2)
  return(overlap)
}

# topic overlap stat
TopicOverlapStat <- function(topic, dist_matrix1, dist_matrix2, N = NULL, threshold1 = NULL, threshold2 = NULL){
  context1 <- pblapply(topic, function(w) names(closest_neighbors(w, dist_matrix1, num_neighbors = N, threshold = threshold1))) %>% unlist()
  context2 <- pblapply(topic, function(w) names(closest_neighbors(w, dist_matrix2, num_neighbors = N, threshold = threshold2))) %>% unlist()
  overlap <- length(intersect(context1, context2))/(10*length(topic))
  return(overlap)
}

topic <- list("immigration", "immigrants", "immigrant")
# Republicans - Democrats
TopicDiff(topic, dist_matrix1 = distance_matrices[["R"]], dist_matrix2 = distance_matrices[["D"]], N = 10, label1 = "R", label2 = "D")
TopicOverlap(topic, dist_matrix1 = distance_matrices[["R"]], dist_matrix2 = distance_matrices[["D"]], N = 10)
TopicOverlapStat(topic, dist_matrix1 = distance_matrices[["R"]], dist_matrix2 = distance_matrices[["D"]], N = 10) 
# Female - Male
TopicDiff(topic, dist_matrix1 = distance_matrices[["F"]], dist_matrix2 = distance_matrices[["M"]], N = 10, label1 = "F", label2 = "M")
TopicOverlap(topic, dist_matrix1 = distance_matrices[["F"]], dist_matrix2 = distance_matrices[["M"]], N = 10)
TopicOverlapStat(topic, dist_matrix1 = distance_matrices[["F"]], dist_matrix2 = distance_matrices[["M"]], N = 10) 





