rm(list = ls())
library(stringr)
library(text2vec)
library(tm)
library(data.table)

SOURCE <- list("FOX", "MSNBC")
VOCAB_SIZE <- 10000
in_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/News/Inputs/CSV/"
out_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/News/Inputs/"

# pre-trained word embeddings vocab
global_vocab <- readRDS("/Users/pedrorodriguez/Dropbox/Research/Concept Polarization/Targeted/Inputs/GloVe/global_vocab.Rds")

# stop words
stop_words <- stopwords(kind = "en") 
stop_words <- str_replace_all(stop_words, "'", "")  

vocab_list <- list()
for(s in SOURCE){
  source_in_path <- paste0(in_path, s, "/")
  it <- idir(source_in_path)  # iterator over files in directory
  it2 <- itoken(it, tokenizer = function(x) str_split(x, fixed(" ")))  # iterator over tokens
  count <- create_vocabulary(it2)
  count <- count[order(-count$term_count),] # order by count
  count <- count[count$term %in% global_vocab,] # drop words not in global vocab
  count <- count[!(count$term %in% stop_words),] # drop stopwords
  count <- count[nchar(count$term, type = "chars") > 2,] # remove <3 letter words
  count <- count[1:(VOCAB_SIZE),]  # keep most frequent num_words
  vocab_list[[s]] <- count$term
}

saveRDS(vocab_list, paste0(out_path, "vocab_list.rds"))
