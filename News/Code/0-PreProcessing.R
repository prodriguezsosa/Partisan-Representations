rm(list = ls())
library(readtext)
library(data.table)
library(stringr)
library(progress)

SOURCE <- "FOX"
in_path <- paste0("/Users/pedrorodriguez/Dropbox/Research/Concept Polarization/Targeted/Inputs/TEXTS/", SOURCE, "/")
out_path_csv <- paste0("/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/News/Inputs/CSV/", SOURCE, "/")
#out_path_rds <- paste0("/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/News/Inputs/RDS/", SOURCE, "/")
texts <- as.list(list.files(in_path))
texts <- texts[grepl("2016", texts) | grepl("2017", texts)]

#===============================
# ITERATIVELY PRE-PROCESS TEXTS
#===============================
pb <- progress_bar$new(total = length(texts))
for(i in 1:length(texts)){
  text <- readtext(paste0(in_path, texts[[i]]))[2]
  text <- gsub("[\t\n]+", " ", text) # remove all spacing and tabs
  text <- str_replace_all(text, "[[A-Z]][[A-Z]]+", " ") # remove speakers
  text <- gsub("[^[:alnum:] ]", "", text) # remove all non-alpha-numeric characters
  text <- str_replace_all(text, "^ +| +$|( ) +", "\\1")  # remove excess white space
  text <- tolower(text)  # lowercase
  write(text, paste0(out_path_csv, SOURCE, i, ".csv"))
  #saveRDS(text, paste0(out_path_rds, SOURCE, i, ".Rds"))
  rm(text)
  pb$tick()
}


