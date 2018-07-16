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
# set paths
# ================================
in_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Outputs/"

# ================================
# load embeddings of best models
# ================================
embeddings_list <- list()
embeddings_list[["R"]] <- readRDS(paste0(in_path, "R", gsub("[[:alpha:]]", "", best_models_labels[grepl("RR", best_models_labels)][[1]]), "_E2_embedding_matrix.rds"))
embeddings_list[["D"]] <- readRDS(paste0(in_path, "D", gsub("[[:alpha:]]", "", best_models_labels[grepl("DD", best_models_labels)][[1]]), "_E2_embedding_matrix.rds"))
embeddings_list[["F"]] <- readRDS(paste0(in_path, "F", gsub("[[:alpha:]]", "", best_models_labels[grepl("FF", best_models_labels)][[1]]), "_E3_embedding_matrix.rds"))
embeddings_list[["M"]] <- readRDS(paste0(in_path, "M", gsub("[[:alpha:]]", "", best_models_labels[grepl("MM", best_models_labels)][[1]]), "_E3_embedding_matrix.rds"))

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
  overlap <- pblapply(seq(1:length(seeds)), function(x) length(intersect(context1[[x]], context2[[x]]))/length(union(context1, context2)))
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
# explore results
# ================================
seed <- "freedom"
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
  context1 <- pblapply(topic, function(w) names(closest_neighbors(w, dist_matrix1, num_neighbors = N, threshold = threshold1))) %>% unlist() %>% unique
  context2 <- pblapply(topic, function(w) names(closest_neighbors(w, dist_matrix2, num_neighbors = N, threshold = threshold2))) %>% unlist() %>% unique
  overlap <- length(intersect(context1, context2))/length(union(context1, context2))
  return(overlap)
}

# Republicans - Democrats
TopicDiff(topic, dist_matrix1 = distance_matrices[["R"]], dist_matrix2 = distance_matrices[["D"]], N = 10, label1 = "R", label2 = "D")
TopicOverlap(topic, dist_matrix1 = distance_matrices[["R"]], dist_matrix2 = distance_matrices[["D"]], N = 10)
TopicOverlapStat(topic, dist_matrix1 = distance_matrices[["R"]], dist_matrix2 = distance_matrices[["D"]], N = 10) 
# Female - Male
TopicDiff(topic, dist_matrix1 = distance_matrices[["F"]], dist_matrix2 = distance_matrices[["M"]], N = 10, label1 = "F", label2 = "M")
TopicOverlap(topic, dist_matrix1 = distance_matrices[["F"]], dist_matrix2 = distance_matrices[["M"]], N = 10)
TopicOverlapStat(topic, dist_matrix1 = distance_matrices[["F"]], dist_matrix2 = distance_matrices[["M"]], N = 10) 

# ================================
#
# STEP - 5
# IoU
# TOPICS
#
# ================================
token_counts <- readRDS("/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Post-Estimation/TopTokens/token_counts.rds")
topics <- token_counts$token[token_counts$token %in% vocab][1:10]
topics[topics == "right"] <- "justice"

topics <- list("healthcare" = "healthcare",
               "liberal" = c("liberal", "liberals"),
               "freedom" = c("freedom", "freedoms"),
               "justice" = "justice",
               "conservative" = c("conservative", "conservatives"),
               "welfare" = "welfare",
               "equality" = "equality",
               "abortion" = c("abortion", "abortions"),
               "taxes" = c("tax", "taxation", "taxes"),
               "immigration" = c("immigrant", "immigration", "immigrants"))

# top N
TopicDiffsList <- lapply(topics, function(x) TopicDiff(x, dist_matrix1 = distance_matrices[["R"]], dist_matrix2 = distance_matrices[["D"]], N = 10, label1 = "R", label2 = "D"))
TopicOverlapList <- lapply(topics, function(x) TopicOverlap(x, dist_matrix1 = distance_matrices[["R"]], dist_matrix2 = distance_matrices[["D"]], N = 10))
TopipOverlapStatList <- lapply(topics, function(x) TopicOverlapStat(x, dist_matrix1 = distance_matrices[["R"]], dist_matrix2 = distance_matrices[["D"]], N = 10)) %>% unlist

# with threshold
TopicDiffsList <- lapply(topics, function(x) TopicDiff(x, dist_matrix1 = distance_matrices[["R"]], dist_matrix2 = distance_matrices[["D"]], threshold1 = dist_thresholds[["R"]], threshold2 = dist_thresholds[["D"]], label1 = "R", label2 = "D"))
TopicOverlapList <- lapply(topics, function(x) TopicOverlap(x, dist_matrix1 = distance_matrices[["R"]], dist_matrix2 = distance_matrices[["D"]], threshold1 = dist_thresholds[["R"]], threshold2 = dist_thresholds[["D"]]))
TopipOverlapStatList <- lapply(topics, function(x) TopicOverlapStat(x, dist_matrix1 = distance_matrices[["R"]], dist_matrix2 = distance_matrices[["D"]], threshold1 = dist_thresholds[["R"]], threshold2 = dist_thresholds[["D"]])) %>% unlist

# bat chart
RDIoU <- lapply(topics, function(x) TopicOverlapStat(x, dist_matrix1 = distance_matrices[["R"]], dist_matrix2 = distance_matrices[["D"]], N = 10)) %>% unlist
FMIoU <- lapply(topics, function(x) TopicOverlapStat(x, dist_matrix1 = distance_matrices[["F"]], dist_matrix2 = distance_matrices[["M"]], N = 10)) %>% unlist
#RDIoU <- lapply(topics, function(x) TopicOverlapStat(x, dist_matrix1 = distance_matrices[["R"]], dist_matrix2 = distance_matrices[["D"]], threshold1 = dist_thresholds[["R"]], threshold2 = dist_thresholds[["D"]])) %>% unlist
#FMIoU <- lapply(topics, function(x) TopicOverlapStat(x, dist_matrix1 = distance_matrices[["F"]], dist_matrix2 = distance_matrices[["M"]], threshold1 = dist_thresholds[["F"]], threshold2 = dist_thresholds[["M"]])) %>% unlist
plot.data <- data.table(group = c(rep("R-D", length(RDIoU)), rep("F-M", length(RDIoU))), topic = names(topics), iou = unlist(lapply(list(RDIoU, FMIoU), unlist)))
plot.data <- plot.data[order(-plot.data$group, -plot.data$iou)]

plot.data <- transform(plot.data, topic = factor(topic, levels=plot.data$topic[1:(nrow(plot.data)/2)]))  # order levels by coefficient magnitude
ggplot(plot.data, aes(x = topic, y = iou, fill = group)) +
  geom_bar(stat="identity", position=position_dodge()) +
  #geom_text(aes(label = iou), vjust = 1.6, color = "white", position = position_dodge(0.9), size = 3.5) +
  scale_fill_brewer(palette="Paired") +
  xlab("") + ylab("IoU") +
  #theme_minimal() +
  theme(legend.title=element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(size=15, angle = 90),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 10, r = 0, b = 20, l = 0), hjust = 0.5),
        text = element_text(size=15), legend.position = "bottom", legend.text = element_text(size=20)) +
  ggtitle("Intersect over Union")


