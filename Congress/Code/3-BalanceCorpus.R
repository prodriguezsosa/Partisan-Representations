rm(list = ls())
# load data
out_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Inputs/"
SOURCE <- c("R", "D")
corpus <- readRDS("/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Inputs/corpus.rds")
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