rm(list = ls())
library(stringr)

# ================================
# parameters
# ================================
FOLDS <- 10
CHUNK_SIZE <- 250

# ================================
# define covariates
# ================================
PARTY <- list("D", "R")
GENDER <- list("M", "F")
GROUPS <- c(PARTY, GENDER)

# ================================
# define paths
# ================================
out_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Inputs/"

# ================================
# load data
# ================================
corpora <- readRDS("/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Inputs/corpora.rds")

# ================================
# chunk by group
# ================================
corpora_chunks <- list()

for(i in GROUPS){
  corpus <- corpora[[i]]
  # create one big text
  corpus <- paste(corpus, collapse = " ")
  # corpus chunking
  corpus <- unlist(str_split(corpus, " ")) # tokenize
  corpus <- corpus[!is.na(corpus)]
  corpus <- corpus[corpus!="NA"] # remove empty cells
  corpus <- corpus[corpus!=""] # remove empty cells
  corpus <- split(corpus, ceiling(seq_along(corpus)/CHUNK_SIZE)) # split into chunks of size N
  corpus <- lapply(corpus, function(x) paste(x, collapse = " ")) # collapse words within chunks
  corpus <- unname(unlist(corpus))
  corpus <- iconv(corpus, to = "UTF-8")
  set.seed(12111984)
  corpus <- sample(corpus)  # randomize order
  corpus_list[[s]] <- corpus 
}


# balance corpus size
corpus_size <- unname(unlist(lapply(corpus_list, length)))
corpus_list[[which(corpus_size == min(corpus_size))]] <- c(corpus_list[[which(corpus_size == min(corpus_size))]], sample(corpus_list[[which(corpus_size == min(corpus_size))]],  (max(corpus_size) - min(corpus_size))))
# save
for(s in SOURCE){
  saveRDS(corpus_list[[s]], paste0(out_path, s, "_corpus.rds"))
}