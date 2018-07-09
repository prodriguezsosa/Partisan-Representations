rm(list = ls())
library(stringr)
library(text2vec)
library(tm)
library(data.table)
library(progress)
library(magrittr)

PARTY <- list("D", "R")
GENDER <- list("M", "F")
JOINT <- expand.grid(PARTY, GENDER) %>% setnames(c("party", "gender"))
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

vocab_list <- list()

pb <- progress_bar$new(total = nrow(JOINT))
for(i in 1:nrow(JOINT)){
  sub_corpus <- corpus[party == JOINT$party[i] & gender == JOINT$gender[i],]
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

# label lists
names(vocab_list) <- apply( JOINT , 1 , paste , collapse = "-" )

# intersecting vocab
vocab <- Reduce(intersect, vocab_list)

# save
saveRDS(vocab, paste0(out_path, "vocab.rds"))
