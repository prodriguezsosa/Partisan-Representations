rm(list = ls())

# ================================
# load libraries
# ================================
library(keras)
library(reticulate)
library(purrr)
library(dplyr)
library(pbapply)
library(data.table)
library(progress)
library(magrittr)

# ================================
# parameters
# ================================
FOLDS <- 10

# ================================
# define covariates
# ================================
PARTY <- list("D", "R")
GENDER <- list("M", "F")
GROUPS <- c(PARTY, GENDER)

# ================================
# define paths
# ================================
out_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Inputs/"

# ================================
# load data
# ================================
corpora <- readRDS("/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Inputs/corpora.rds")

# ================================
# split into folds
# ================================
corpora_folds <- list()
for(i in GROUPS){
  #set.seed(12111984)
  
  corpus <-  corpora[[i]]
  #corpus <- sample(corpus)  # randomize order
  text_seq <- seq(1, length(corpus), 1)  # sequence id
  chunks <- split(text_seq, ceiling(seq_along(text_seq)/(floor(length(corpus)/FOLDS))))
  corpus_folds <- data.table("group" = i, "corpus" = corpus, "fold" = NA, stringsAsFactors = FALSE)
  for(f in 1:length(chunks)){
    corpus_folds$fold[chunks[[f]]] <- f
  }
  
  corpus_folds <- corpus_folds[fold < 11,]  # remove extra folds
  corpora_folds[[i]] <- corpus_folds
  
  
}

# ================================
# save corpora with folds
# ================================
corpora_folds <- do.call(rbind, corpora_folds)  # rbind
table(corpora_folds$group, corpora_folds$fold)  # check
saveRDS(corpora_folds, paste0(out_path, "corpora_folds.rds"))
