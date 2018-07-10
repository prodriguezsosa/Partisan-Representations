rm(list = ls())
library(stringr)

# set paths
in_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/News/Inputs/RDS/"
out_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/News/Inputs/"

# list of files
corpus_list <- list()
SOURCE <- c("FOX", "MSNBC")
for(s in SOURCE){
#s <- "FOX"
sub_corpora <- as.list(list.files(paste0(in_path, s)))
corpus <- as.character()

# create one big text
for(i in 1:length(sub_corpora)){
  sub_corpus <- readRDS(paste0(in_path, s, "/", sub_corpora[[i]])) # load sub-corpus
  corpus <- paste(corpus, sub_corpus, sep = " ") # paste
}

# corpus chunking
N <- 50 # chunk size
corpus <- unlist(str_split(corpus, " ")) # tokenize
corpus <- corpus[!is.na(corpus)]
corpus <- corpus[corpus!="NA"] # remove empty cells
corpus <- corpus[corpus!=""] # remove empty cells
corpus <- split(corpus, ceiling(seq_along(corpus)/N)) # split into chunks of size N
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