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
# define paths
# ================================
out_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Inputs/"

# ================================
# load data
# ================================
corpus <- readRDS("/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Inputs/corpus.rds")
vocab <- readRDS("/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Inputs/vocab.rds")

# ================================
# define covariates
# ================================
PARTY <- list("D", "R")
GENDER <- list("M", "F")
GROUPS <- c(PARTY, GENDER)

# ================================
# check corpora length
# ================================
# create vocab and tokenizer
tokenizer <- text_tokenizer(length(vocab))
tokenizer %>% fit_text_tokenizer(vocab)
VOCAB_SIZE <- tokenizer$num_words

# subset each corpus to texts with more than one token in vocab
corpus_check <- texts_to_sequences(tokenizer, corpus$speech) %>% pblapply(., function(x) length(x) > 1) %>% unlist(.)
corpus <- corpus[corpus_check]

# ================================
# stratify population
# ================================
pop_size <- corpus[, .(pop_size = length(speech_id)), by = c("gender", "party")]
pop_size <- pop_size[(gender %in% GENDER) & (party %in% PARTY),]
min_pop <- list("M" =  min(pop_size[gender == "M", pop_size]),
                "F" =  min(pop_size[gender == "F", pop_size]),
                "D" =  min(pop_size[party == "D", pop_size]),
                "R" =  min(pop_size[party == "R", pop_size])) 

# balance party samples
pop_party <- list()
for(i in PARTY){
  set.seed(12111984)
  pop_party[[i]] <- c(sample(corpus[party == i & gender == "F", speech_id], min_pop[["F"]], replace = FALSE),
                       sample(corpus[party == i & gender == "M", speech_id], min_pop[["M"]], replace = FALSE))
}
  
# balance gender samples
pop_gender <- list()
for(i in GENDER){
  set.seed(12111984)
  pop_gender[[i]] <- c(sample(corpus[party == "D" & gender == i, speech_id], min_pop[["D"]], replace = FALSE),
                       sample(corpus[party == "R" & gender == i, speech_id], min_pop[["R"]], replace = FALSE))
}

# ================================
# subset corpora
# ================================
corpora <- list()

# by party
for(i in PARTY){
  corpora[[i]] <- corpus[party == i & (speech_id %in% pop_party[[i]]), speech]
}

# by gender
for(i in GENDER){
  corpora[[i]] <- corpus[gender == i & (speech_id %in% pop_gender[[i]]), speech]
}

#table(corpora[["F"]]$party)  # check
#table(corpora[["R"]]$gender)  # check

# ================================
# save stratified corpora
# ================================
saveRDS(corpora, paste0(out_path, "corpora.rds"))



#=============================================STRATIFY BY NUMBER OF TOKENS=========================================


# ================================
# create vocab and tokenizer
# ================================
tokenizer <- text_tokenizer(length(vocab))
tokenizer %>% fit_text_tokenizer(vocab)
VOCAB_SIZE <- tokenizer$num_words

# ================================
# check sub-corpora
# ================================
#corpora_tokens <- list()  # empty list
corpora_length <- list()  # empty list
corpora_check <- list()
pb <- progress_bar$new(total = nrow(JOINT))  # progress bar
for(i in 1:nrow(JOINT)){
  sub_corpus <- unlist(corpus[party == JOINT$party[i] & gender == JOINT$gender[i], "speech"])
  corpus_check <- texts_to_sequences(tokenizer, sub_corpus) %>% pblapply(., function(x) length(x) > 1) %>% unlist(.)
  corpora_check[[i]] <- corpus_check
  sub_corpus <- sub_corpus[corpus_check]
  sub_corpus <- space_tokenizer(sub_corpus)
  #corpora_tokens[[i]] <- sub_corpus
  corpora_length[[i]] <- length(sub_corpus)
  rm(sub_corpus)
  pb$tick()
}

# ================================
# subset corpora
# ================================
min_length <-  min(unlist(corpora_length))
corpora <- list()  # empty list
pb <- progress_bar$new(total = nrow(JOINT))  # progress bar
for(i in 1:nrow(JOINT)){
  sub_corpus <- unlist(corpus[party == JOINT$party[i] & gender == JOINT$gender[i], "speech"])
  sub_corpus <- sub_corpus[corpus_check[[i]]]
  sub_corpus <- space_tokenizer(sub_corpus)
  index <- sample(1:(length(sub_corpus) - min_length), 1)
  sub_corpus <- sub_corpus[index:(index + min_length)]
  

  
  pb$tick()
}


corpus_list <- list(corpus[party == SOURCE[1], speech], corpus[party == SOURCE[2], speech])
names(corpus_list) <- SOURCE
# balance corpus size
corpus_size <- unname(unlist(lapply(corpus_list, length)))
set.seed(12111984)
corpus_list[[which(corpus_size == min(corpus_size))]] <- c(corpus_list[[which(corpus_size == min(corpus_size))]], sample(corpus_list[[which(corpus_size == min(corpus_size))]],  (max(corpus_size) - min(corpus_size))))
# save
for(s in SOURCE){
  saveRDS(corpus_list[[s]], paste0(out_path, s, "_corpus.rds"))
}

corpus_check <- texts_to_sequences(tokenizer, corpus) %>% pblapply(., function(x) length(x) > 1) %>% unlist(.)
corpus <- corpus[corpus_check]
corpus <- rep(corpus, EPOCHS)