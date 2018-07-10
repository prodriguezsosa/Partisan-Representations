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
corpora <- list()

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
# NOTE: the total number of tokens may be slightly different
# total number of tokens %in% vocab should be the same for both genders
#check1 <- corpus_D[["M"]] %in% vocab
#check2 <- corpus_R[["M"]] %in% vocab
#length(check1[check1 == TRUE])
#length(check1[check1 == TRUE])

# join corpora by gender
for(i in GENDER){
  corpora[[i]] <- c(corpus_D[[i]], corpus_R[[i]])
}

# spring cleaning
rm(sub_corpus_D, sub_corpus_R, in_vocab_D, in_vocab_R, count_D, count_R, corpus_D, corpus_R)

# ================================
# corpora by party 
# stratifying by gender & number of tokens in vocab
# ================================
sub_corpus_M <- list()
sub_corpus_F <- list()
in_vocab_M <- list()
in_vocab_F <- list()
count_M <- list()
count_F <- list()
corpus_M <- list()
corpus_F <- list()

for(i in PARTY){
  # subset corpora to relevant groups and tokenize
  sub_corpus_M[[i]] <- unlist(space_tokenizer(paste(corpus[gender == "M" & party == i, speech], collapse = " ")))
  sub_corpus_F[[i]] <- unlist(space_tokenizer(paste(corpus[gender == "F" & party == i, speech], collapse = " ")))
  # check whether word is in voacb
  in_vocab_M[[i]] <- unlist(pblapply(list(sub_corpus_M[[i]]), function(x) x %in% vocab))
  in_vocab_F[[i]] <- unlist(pblapply(list(sub_corpus_F[[i]]), function(x) x %in% vocab))
  # cumulative count of words in vocab
  count_M[[i]] <- ave(in_vocab_M[[i]] == TRUE, in_vocab_M[[i]], FUN=cumsum)
  count_F[[i]] <- ave(in_vocab_F[[i]] == TRUE, in_vocab_F[[i]], FUN=cumsum)
}

for(i in PARTY){
  # subset vocab such that there's the same number of republican and democrat words in each group
  corpus_M[[i]] <- sub_corpus_M[[i]][1:which(count_M[[i]] == min(unlist(lapply(count_M, max))))]
  corpus_F[[i]] <- sub_corpus_F[[i]][1:which(count_F[[i]] == min(unlist(lapply(count_F, max))))]
}

# check
# NOTE: the total number of tokens may be slightly different
# total number of tokens %in% vocab should be the same for both genders
#check1 <- corpus_F[["D"]] %in% vocab
#check2 <- corpus_M[["D"]] %in% vocab
#length(check1[check1 == TRUE])
#length(check1[check1 == TRUE])

# join corpora by gender
for(i in PARTY){
  corpora[[i]] <- c(corpus_M[[i]], corpus_F[[i]])
}

# ================================
# save stratified corpora
# ================================
saveRDS(corpora, paste0(out_path, "corpora.rds"))
