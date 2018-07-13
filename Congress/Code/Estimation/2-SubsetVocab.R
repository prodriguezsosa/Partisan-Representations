rm(list = ls())
library(stringr)
library(text2vec)
library(tm)
library(data.table)
library(progress)
library(magrittr)

PARTY <- list("D", "R")
GENDER <- list("M", "F")
GROUPS <- c(PARTY, GENDER)
VOCAB_SIZE <- 10000
in_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Inputs/"
out_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Inputs/"

# load corpus
corpus <- readRDS(paste0(in_path, "corpus.rds"))

# pre-trained word embeddings vocab
#global_vocab <- readRDS("/Users/pedrorodriguez/Dropbox/Research/Concept Polarization/Targeted/Inputs/GloVe/global_vocab.Rds")

# stop words
stop_words <- stopwords(kind = "en") 
stop_words <- str_replace_all(stop_words, "'", "")  

# ================================
# vocab count for each group
# ================================
vocab_list <- list()
pb <- progress_bar$new(total = length(GROUPS))
for(i in GROUPS){
  if(i %in% PARTY){
    sub_corpus <- corpus[party == i,]
    }else{
    sub_corpus <- corpus[gender == i,]
  }
  tokens = space_tokenizer(sub_corpus$speech)
  it = itoken(tokens, progressbar = FALSE)
  count = create_vocabulary(it)
  count <- count[order(-count$term_count),] # order by count
  #count <- count[count$term %in% global_vocab,] # drop words not in global vocab
  count <- count[!(count$term %in% stop_words),] # drop stopwords
  count <- count[!(grepl("[[:digit:]]",count$term)),] # drop numbers
  count <- count[nchar(count$term, type = "chars") > 2,] # remove <3 letter words
  #count <- count[count$term_count >= 10,] # remove <3 letter words
  count <- count[1:(VOCAB_SIZE),]  # keep most frequent num_words
  vocab_list[[i]] <- count$term
  pb$tick()
}

# intersecting vocab
vocab <- Reduce(intersect, vocab_list)

# save
saveRDS(vocab, paste0(out_path, "vocab.rds"))
