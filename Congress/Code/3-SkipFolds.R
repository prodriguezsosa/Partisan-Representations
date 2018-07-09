rm(list = ls())
library(keras)
library(text2vec)
library(data.table)
library(magrittr)
library(progress)

# parameters
SOURCE <- list("FOX", "MSNBC")
SKIP_WINDOW <- 6
FOLDS <- 10
fold_size <- data.table()
pb <- progress_bar$new(total = length(SOURCE))
for(s in SOURCE){
# file paths
in_path <- paste0("/Users/pedrorodriguez/Dropbox/Research/Concept Polarization/Full/Outputs/SkipGrams/", s, "/")
out_path <- paste0("/Users/pedrorodriguez/Dropbox/Research/Concept Polarization/Full/Outputs/SkipFolds/", s, "/")
dictionary <- readRDS("/Users/pedrorodriguez/Dropbox/Research/Concept Polarization/Full/Outputs/SkipGrams/dictionary.Rds")

# load skip-grams
skip_grams <- readRDS(paste0(in_path, "skip_grams_", s, ".Rds"))
skip_grams <- data.table(do.call(rbind, skip_grams)) %>% set_colnames(c("center", "target")) 
# subset to have only cue as center word
#if(!(is.null(w))){skip_grams <- skip_grams[center == dictionary[[w]]]}
#if(!(is.null(w))){skip_grams <- skip_grams[center == dictionary[[w]] | target == dictionary[[w]]]}
#if(!(is.null(w))){skip_grams <- skip_grams[target == dictionary[[w]]]}
set.seed(12111984)
skip_grams <- skip_grams[sample(nrow(skip_grams)),]
skip_id <- seq(1, nrow(skip_grams), 1)
chunks <- split(skip_id, ceiling(seq_along(skip_id)/(floor(nrow(skip_grams)/FOLDS))))
fold_size <- rbind(fold_size, data.table(source = s, fold_size = length(chunks[[1]]), num_folds = length(chunks))) # keep registry of fold size
skip_grams[,fold:=0]
for(f in 1:length(chunks)){
  skip_grams$fold[chunks[[f]]] <- f
}
saveRDS(skip_grams, paste0(out_path, "skip_folds_", s, ".Rds"))
pb$tick()
}

saveRDS(fold_size, "/Users/pedrorodriguez/Dropbox/Research/Concept Polarization/Full/Outputs/SkipFolds/fold_size.Rds")
