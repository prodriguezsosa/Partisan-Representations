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
in_path <- "/scratch/plr250/WordEmbeddings/PartisanEmbeddings/Congress/Inputs/"
out_path <- "/scratch/plr250/WordEmbeddings/PartisanEmbeddings/Congress/Outputs/"

# define source
SOURCE <- as.character(args[1])
FOLD <- as.integer(args[2])
#SOURCE <- "R"
#FOLD <- 1

# set parameters
#WINDOW_SIZE <- as.numeric(args[2])  # how many words to consider left and right
WINDOW_SIZE <- 6  # how many words to consider left and right
NEGATIVE_SAMPLES <- 1  # number of negative examples to sample for each word
EMBEDDING_SIZE <- 300  # dimension of the embedding vector
EPOCHS <- 1

# ================================
# load data
# ================================
#vocab <- readRDS("/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Inputs/vocab.rds")
#corpora <- readRDS("/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Inputs/corpora_folds.rds")
vocab <- readRDS(paste0(in_path, "vocab.rds"))
corpora <- readRDS(paste0(in_path, "corpora_folds.rds"))
#corpora <- corpora[group == SOURCE,]

# ================================
# loop over folds
# ================================
#embeddings_list <- list()
#for(i in START:END){
  #corpus <- corpora[fold!= FOLD, corpus]
  corpus <- corpora[group == SOURCE & fold!= FOLD, corpus]
  #corpus <- corpus[1:10]
  # ================================
  # create vocab and tokenizer
  # ================================
  tokenizer <- text_tokenizer(length(vocab))
  tokenizer %>% fit_text_tokenizer(vocab)
  VOCAB_SIZE <- tokenizer$num_words
  
  # dictionary
  dictionary <- tokenizer$word_index
  
  # reverse dictionary
  reverse_dictionary <- as.list(names(dictionary))
  names(reverse_dictionary) <- unlist(dictionary)
  
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
    embeddings_initializer = initializer_random_uniform(minval = -0.05, maxval = 0.05, seed = 11121984),
    input_length = 1, 
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
      steps_per_epoch = length(corpus)-1, epochs = 1
    )
  
  # ================================
  # extract embeddings
  # ================================
  embedding_matrix <- get_weights(model)[[1]]
  
  words <- data_frame(
    word = names(tokenizer$word_index), 
    id = as.integer(unlist(tokenizer$word_index))
  )
  
  words <- words %>%
    filter(id <= tokenizer$num_words) %>%
    arrange(id)
  
  #embedding_matrix <- embedding_matrix[-1,]
  row.names(embedding_matrix) <- c("unk", words$word)
  #row.names(embedding_matrix) <- words$word
  
#  embeddings_list[[i]] <- embedding_matrix
#}

# ================================
# save embeddings
# ================================
#saveRDS(embedding_matrix, paste0(out_path, SOURCE, "_", FOLD, "_", WINDOW_SIZE, "_", VOCAB_SIZE, "_embedding_matrix.rds"))
saveRDS(embedding_matrix, paste0(out_path, SOURCE, FOLD, "_embedding_matrix.rds"))
  
