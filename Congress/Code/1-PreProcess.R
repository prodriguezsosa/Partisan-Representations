rm(list = ls())
library(readtext)
library(data.table)
library(stringr)
library(progress)
library(keras)
library(udpipe)
library(pbapply)

# ================================
# define paths
# ================================
in_path <- "/Users/pedrorodriguez/Dropbox/Research/WordEmbeddings/US/Congress/Inputs/Transcripts/hein-bound/"
out_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Inputs/"

# ================================
# list of file names
# ================================
files <- as.list(list.files(in_path))
files_meta <- files[grepl(pattern = "SpeakerMap", x = files)]  # meta data
files_text <- files[grepl(pattern = "speeches", x = files)]  # text
files_text <- files_text[(length(files_text)-9):length(files_text)]  # keep most recent 10 sessions
files_meta <- files_meta[(length(files_meta)-9):length(files_meta)]  # keep most recent 10 sessions

# ================================
# load and pre-process data
# ================================
text_meta <- list()
pb <- progress_bar$new(total = length(files_text))
for(i in 1:length(files_text)){
  # ================================
  # upload text
  # ================================
  text <- read.table(paste(in_path, files_text[[i]], sep =""), 
                     header = FALSE, sep = "|", 
                     colClasses = "character", quote = "", 
                     col.names = c("speech_id", "speech"), skip = 1, 
                     blank.lines.skip = TRUE, skipNul = TRUE, 
                     strip.white = TRUE, fill = TRUE)
  
  # transcripts have all sorts of redundant full stops (problematic when parsing)
  text$speech <- gsub("([.])([[:space:]][[:lower:]])", "\\2", text$speech)
  text$speech <- gsub("[.][[:space:]][.]", " ", text$speech)
  
  # basic pre-processing
  text$speech <- gsub("[^[:alnum:] ]", "", text$speech) # remove all non-alpha-numeric characters
  text$speech <- str_replace_all(text$speech, "^ +| +$|( ) +", "\\1")  # remove excess white space
  text$speech <- tolower(text$speech)  # lowercase
  
  # ================================
  # upload meta data
  # ================================
  meta <- read.table(paste(in_path, files_meta[[i]], sep =""), 
                     header = FALSE, sep = "|", 
                     colClasses = "character", quote = "", 
                     col.names = c("speakerid", "speech_id", "lastname", "firstname", "chamber", "state", 
                                   "gender", "party", "district", "nonvoting"), skip = 1, 
                     blank.lines.skip = TRUE, skipNul = TRUE, 
                     strip.white = TRUE, fill = TRUE)
  
  # add session id
  meta$session_id <- unlist(str_split(files_meta[[i]], pattern = "_"))[1]
  
  # ================================
  # merge text and meta
  # ================================
  text_meta[[i]] <- data.table(merge(text, meta, by = "speech_id"))
  pb$tick()
}

corpus <- do.call(rbind, text_meta)
saveRDS(corpus, paste0(out_path, "corpus.rds"))
