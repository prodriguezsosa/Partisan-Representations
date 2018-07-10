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
library(quanteda)
library(stringr)
library(text2vec)

# ================================
# define parameters
# ================================
WINDOW_SIZE <- 6

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
# add buffer
# ================================
buffer <- paste(rep("unk", WINDOW_SIZE), collapse = " ")
corpus$speech <- unlist(pblapply(corpus$speech, function(x) paste(buffer, x, buffer, collapse = " ")))

# ================================
# randomize speeches
# ================================
set.seed(12111984)
corpus$speech <- sample(corpus$speech)

# ================================
# corpora by gender 
# stratifying by party & number of tokens in vocab
# ================================
sub_corpus_D <- list()
sub_corpus_R <- list()
in_vocab_D <- list()
in_vocab_R <- list()
count_D <- list()
count_R <- list()
corpus_D <- list()
corpus_R <- list()

for(i in GENDER){
  # subset corpora to relevant groups and tokenize
  sub_corpus_D[[i]] <- unlist(space_tokenizer(paste(corpus[gender == i & party == "D", speech], collapse = " ")))
  sub_corpus_R[[i]] <- unlist(space_tokenizer(paste(corpus[gender == i & party == "R", speech], collapse = " ")))
  # check whether word is in voacb
  in_vocab_D[[i]] <- unlist(pblapply(list(sub_corpus_D[[i]]), function(x) x %in% vocab))
  in_vocab_R[[i]] <- unlist(pblapply(list(sub_corpus_R[[i]]), function(x) x %in% vocab))
  # cumulative count of words in vocab
  count_D[[i]] <- ave(in_vocab_D[[i]] == TRUE, in_vocab_D[[i]], FUN=cumsum)
  count_R[[i]] <- ave(in_vocab_R[[i]] == TRUE, in_vocab_R[[i]], FUN=cumsum)
}

for(i in GENDER){
  # subset vocab such that there's the same number of republican and democrat words in each group
  corpus_D[[i]] <- sub_corpus_D[[i]][1:which(count_D[[i]] == min(unlist(lapply(count_D, max))))]
  corpus_R[[i]] <- sub_corpus_R[[i]][1:which(count_R[[i]] == min(unlist(lapply(count_R, max))))]
}

# check
#check <- corpus_R[["M"]] %in% vocab
#length(check[check == TRUE])

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

NumTokensInVocab <- function(text, vocab){
  vtokens <- unlist(str_split(text, pattern = " "))
  num_tokens <- vtokens %in% vocab
  num_tokens <- length(num_tokens[num_tokens == TRUE])
}

corpora_folds <- corpora_folds[,num_tokens:=ntoken(corpus)]
temp <- corpora_folds
temp$num_tokens <- unlist(pblapply(corpora_folds$corpus, function(x) NumTokensInVocab(x, vocab = vocab)))


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

#table(corpora[["M"]]$party)  # check
#table(corpora[["D"]]$gender)  # check

# ================================
# save stratified corpora
# ================================
saveRDS(corpora, paste0(out_path, "corpora.rds"))
