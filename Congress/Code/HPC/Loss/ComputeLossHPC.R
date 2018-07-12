#!/usr/bin/env Rscript
rm(list = ls())
library(keras)
library(reticulate)
library(purrr)
library(dplyr)
library(data.table)
#library(pbapply)

# args to process
args <- commandArgs(trailingOnly = TRUE)
print(args)
if(length(args)!=2) stop(paste0("Not the right number of arguments!", args))
#args <- as.numeric(args)

# set paths
#in_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Outputs/Folds/Gender/"
in_path <- "/scratch/plr250/WordEmbeddings/PartisanEmbeddings/Congress/Outputs/"
out_path <- "/scratch/plr250/WordEmbeddings/PartisanEmbeddings/Congress/Post-Estimation/Loss/Outputs/"

# define source
#MODEL <- "F"
#TEST <- "M"
MODEL <- as.character(args[1])
TEST <- as.integer(args[2])
NUM_FOLDS <- 10

# set parameters
WINDOW_SIZE <- 6  # how many words to consider left and right
NEGATIVE_SAMPLES <- 0  # number of negative examples to sample for each word
EMBEDDING_SIZE <- 300  # dimension of the embedding vector

# ================================
# load data
# ================================
#vocab <- readRDS("/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Inputs/vocab.rds")
#corpora <- readRDS("/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Inputs/corpora_folds.rds")
vocab <- readRDS("/scratch/plr250/WordEmbeddings/PartisanEmbeddings/Congress/Inputs/vocab.rds")
corpora <- readRDS("/scratch/plr250/WordEmbeddings/PartisanEmbeddings/Congress/Inputs/corpora_folds.rds")

loss_history <- list()
for(i in 1:1){
  
  if(MODEL %in% c("F", "M")){pre_trained_embeddings <- readRDS(paste0(in_path, paste0(MODEL, i, "_E3_embedding_matrix.rds")))}
  if(MODEL %in% c("R", "D")){pre_trained_embeddings <- readRDS(paste0(in_path, paste0(MODEL, i, "_E2_embedding_matrix.rds")))}
  
  # ================================
  # loop over folds
  # ================================
  corpus <- corpora[group == TEST & fold == i, corpus]
  
  # ================================
  # create vocab and tokenizer
  # ================================
  tokenizer <- text_tokenizer(length(vocab))
  tokenizer %>% fit_text_tokenizer(vocab)
  VOCAB_SIZE <- tokenizer$num_words
  
  # ================================
  # check item length
  # ================================
  # first check all batches have more than one token in vocab
  corpus_check <- texts_to_sequences(tokenizer, corpus) %>% lapply(., function(x) length(x) > 1) %>% unlist(.)
  corpus <- corpus[corpus_check]
  corpus <- sample(corpus)  # double shuffling
  
  # ================================
  # skip-gram generator
  # ================================
  skipgrams_generator <- function(text, tokenizer, window_size, negative_samples) {
    gen <- texts_to_sequences_generator(tokenizer, sample(text))
    function() {
      skip <- generator_next(gen) %>%
        skipgrams(
          vocabulary_size = tokenizer$num_words, 
          window_size = window_size, 
          negative_samples = 1
        )
      x <- transpose(skip$couples) %>% map(. %>% unlist %>% as.matrix(ncol = 1))
      y <- skip$labels %>% as.matrix(ncol = 1)
      list(x, y)
    }
  }
  
  input_target <- layer_input(shape = 1)
  input_context <- layer_input(shape = 1)
  
  embedding <- layer_embedding(
    input_dim = tokenizer$num_words + 1, 
    output_dim = EMBEDDING_SIZE, 
    weights = list(pre_trained_embeddings),
    input_length = 1, 
    trainable = FALSE,
    name = "embedding"
  )
  
  target_vector <- input_target %>% 
    embedding() %>% 
    layer_flatten()
  
  context_vector <- input_context %>%
    embedding() %>%
    layer_flatten()
  
  dot_product <- layer_dot(list(target_vector, context_vector), axes = 1)
  output <- layer_dense(dot_product, units = 1, activation = "sigmoid")
  
  model <- keras_model(list(input_target, input_context), output)
  model %>% compile(loss = "binary_crossentropy", optimizer = "adam")
  
  summary(model)
  
  model %>%
    fit_generator(
      skipgrams_generator(corpus, tokenizer = tokenizer, window_size = WINDOW_SIZE, negative_samples = NEGATIVE_SAMPLES),
      steps_per_epoch = 1, epochs = length(corpus), verbose = FALSE
    )
  
  # ================================
  # extract loss
  # ================================
  loss_history[[paste0(MODEL, TEST, i)]] <- unname(unlist(model$history$history))
}

# ================================
# save loss history
# ================================
saveRDS(loss_history, paste0(out_path, MODEL, TEST, "_loss_history.rds"))

