rm(list = ls())
library(keras)
library(reticulate)
library(purrr)
library(dplyr)
library(text2vec)
library(udpipe)
library(pbapply)
library(data.table)

# load parsing/POS model
#el <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(file = "english-ud-2.0-170801.udpipe")

# set paths
in_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/News/Output"
out_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/News/Output"

# define source
SOURCE <- "FOX"
WINDOW_SIZE <- 2
N <- 500

# ================================
# load data
# ================================
#reverse_dictionary <- readRDS(paste0(out_path, SOURCE, "_", WINDOW_SIZE, "_reverse_dictionary.rds"))
#dictionary <- readRDS(paste0(out_path, SOURCE, "_", WINDOW_SIZE, "_dictionary.rds"))
embedding_matrix_FOX <- readRDS("/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/News/Output/FOX_8712_6_2_embedding_matrix.rds")
embedding_matrix_MSNBC <- readRDS("/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/News/Output/MSNBC_8712_6_2_embedding_matrix.rds")

#embedding_matrix <- readRDS(paste0(out_path, SOURCE, "_", N, "_", WINDOW_SIZE, "_embedding_matrix.rds"))

#embedding_matrix <- embedding_matrix[-1,]
#row.names(embedding_matrix) <- c("UNK", names(dictionary))
#row.names(embedding_matrix) <- names(dictionary)

find_similar_words <- function(word, embedding_matrix, n = 10) {
  similarities <- embedding_matrix[word, , drop = FALSE] %>%
    sim2(embedding_matrix, y = ., method = "cosine")
  
  similarities[,1] %>% sort(decreasing = TRUE) %>% head(n)
}

token <- "welfare"
find_similar_words(token, embedding_matrix_FOX)
find_similar_words(token, embedding_matrix_MSNBC)

embedding_matrix <- embedding_matrix_FOX

# ================================
# POS tagging
# ================================
vocab <- rownames(embedding_matrix)
vocab_annotated <- pblapply(vocab, function(s) as.data.frame(udpipe_annotate(udmodel_english, x = s, parser = "none")))
vocab_pos <- data.table(token = lapply(vocab_annotated, function(s) s[c("token")]) %>% unlist(.) %>% unname(.),
                        pos = lapply(vocab_annotated, function(s) s[c("upos")]) %>% unlist(.) %>% unname(.))
vocab_adjectives <- vocab_pos[pos == "ADJ", token]

target <- "republicans"
vocab_sentiment <- c(target, vocab_adjectives)

sub_embedding_matrix_FOX <- embedding_matrix_FOX[rownames(embedding_matrix_FOX) %in% vocab_sentiment,]
find_similar_words(target, sub_embedding_matrix_FOX)

sub_embedding_matrix_MSNBC <- embedding_matrix_MSNBC[rownames(embedding_matrix_MSNBC) %in% vocab_sentiment,]
find_similar_words(target, sub_embedding_matrix_MSNBC)

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


