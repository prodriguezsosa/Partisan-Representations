rm(list = ls())
library(keras)
library(reticulate)
library(purrr)
library(dplyr)
library(data.table)
#library(pbapply)

# args to process
#args <- commandArgs(trailingOnly = TRUE)
#if(length(args)!=2) stop(paste0("Not the right number of arguments!", args))
#args <- as.numeric(args)

# set paths
in_path <- "/scratch/plr250/WordEmbeddings/PartisanEmbeddings/Congress/Inputs/"
out_path <- "/scratch/plr250/WordEmbeddings/PartisanEmbeddings/Congress/Outputs/"

# define source
#SOURCE <- as.character(args[1])
SOURCE <- "R"

# set parameters
#WINDOW_SIZE <- as.numeric(args[2])  # how many words to consider left and right
WINDOW_SIZE <- 2  # how many words to consider left and right
NEGATIVE_SAMPLES <- 1  # number of negative examples to sample for each word
EMBEDDING_SIZE <- 300  # dimension of the embedding vector
EPOCHS <- 1

# ================================
# load data
# ================================
#corpus <- readRDS("/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Inputs/corpus.rds")
#vocab <- readRDS("/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Inputs/vocab_list.rds")
corpus <- readRDS(paste0(in_path, SOURCE, "_corpus.rds"))
#corpus <- corpus[party == SOURCE, speech]
vocab <- readRDS(paste0(in_path, "vocab_list.rds"))

# ================================
# create vocab and tokenizer
# ================================
vocab <- intersect(vocab[["D"]], vocab[["R"]]) # intersection of the top N terms in all sources
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
# first check all batches have more than one token in vocab
corpus_check <- texts_to_sequences(tokenizer, corpus) %>% lapply(., function(x) length(x) > 1) %>% unlist(.)
corpus <- corpus[corpus_check]
corpus <- rep(corpus, EPOCHS)
#corpus <- corpus[1:1000]

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

# ================================
# save embeddings
# ================================
saveRDS(embedding_matrix, paste0(out_path, EPOCHS, SOURCE, WINDOW_SIZE, "_", VOCAB_SIZE, "_embedding_matrix.rds"))
#saveRDS(tokenizer, paste0(out_path, SOURCE, "_", WINDOW_SIZE, "_tokenizer.rds"))
#saveRDS(dictionary, paste0(out_path, SOURCE, "_", WINDOW_SIZE, "_dictionary.rds"))
#saveRDS(reverse_dictionary, paste0(out_path, SOURCE, "_", WINDOW_SIZE, "_reverse_dictionary.rds"))
