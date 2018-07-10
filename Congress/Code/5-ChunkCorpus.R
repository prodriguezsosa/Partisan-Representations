rm(list = ls())
library(stringr)

# ================================
# parameters
# ================================
CHUNK_SIZE <- 500

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
  corpus <- split(corpus, ceiling(seq_along(corpus)/CHUNK_SIZE)) # split into chunks of size N
  corpus <- lapply(corpus, function(x) paste(x, collapse = " ")) # collapse words within chunks
  corpus <- unname(unlist(corpus))
  corpus <- iconv(corpus, to = "UTF-8")
  set.seed(12111984)
  corpus <- sample(corpus)  # randomize order
  corpora_chunks[[i]] <- corpus
}

# ================================
# save corpora with folds
# ================================
saveRDS(corpora_chunks, paste0(out_path, "corpora_chunks.rds"))
