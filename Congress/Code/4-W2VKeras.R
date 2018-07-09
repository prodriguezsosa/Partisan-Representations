rm(list = ls())
library(keras)
library(reticulate)
library(purrr)
library(dplyr)
library(pbapply)
library(data.table)
library(text2vec)

# set paths
in_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Inputs/"
out_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Outputs/"

# define source
SOURCE <- "D"

# set parameters
WINDOW_SIZE <- 6  # how many words to consider left and right
NEGATIVE_SAMPLES <- 1  # number of negative examples to sample for each word
EMBEDDING_SIZE <- 300  # dimension of the embedding vector
EPOCHS <- 1

# ================================
# load data
# ================================
corpora_folds <- readRDS(paste0(in_path, "corpora_folds.rds"))
corpus <- corpus[group == "D-M" & fold != 1, corpus]
vocab <- readRDS(paste0(in_path, "vocab.rds"))

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
    steps_per_epoch = length(corpus), epochs = 1
  )

# ================================
# save embeddings
# ================================
embedding_matrix <- get_weights(model)[[1]]
#saveRDS(embedding_matrix, paste0(out_path, SOURCE, "_embedding_matrix.rds"))

words <- data_frame(
  word = names(tokenizer$word_index), 
  id = as.integer(unlist(tokenizer$word_index))
)

words <- words %>%
  filter(id <= tokenizer$num_words) %>%
  arrange(id)

#embedding_matrix <- embedding_matrix[-1,]
row.names(embedding_matrix) <- c("UNK", words$word)
#row.names(embedding_matrix) <- words$word

find_similar_words <- function(word, embedding_matrix, n = 10) {
  similarities <- embedding_matrix[word, , drop = FALSE] %>%
    sim2(embedding_matrix, y = ., method = "cosine")
  
  similarities[,1] %>% sort(decreasing = TRUE) %>% head(n)
}

find_similar_words("economy", embedding_matrix)

library(Rtsne)
library(ggplot2)
library(plotly)

tsne <- Rtsne(embedding_matrix[2:500,], perplexity = 50, pca = FALSE)

tsne_plot <- tsne$Y %>%
  as.data.frame() %>%
  mutate(word = row.names(embedding_matrix)[2:500]) %>%
  ggplot(aes(x = V1, y = V2, label = word)) + 
  geom_text(size = 3)
tsne_plot


