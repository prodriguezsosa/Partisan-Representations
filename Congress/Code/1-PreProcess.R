rm(list = ls())
library(readtext)
library(data.table)
library(stringr)
library(progress)
library(keras)
library(udpipe)
library(pbapply)

# load parsing/POS model
#el <- udpipe_download_model(language = "english")
#udmodel_english <- udpipe_load_model(file = "english-ud-2.0-170801.udpipe")

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
files_text <- files_text[(length(files_text)-9):length(files_text)]
files_meta <- files_meta[(length(files_meta)-9):length(files_meta)]
#files_meta <- files_meta[(length(files_meta)-9):length(files_meta)]  # keep most recent 10 sessions
#files_text <- files_text[(length(files_text)-9):length(files_text)]  # keep most recent 10 sessions

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
  
  # ================================
  # POS tagging
  # ================================
  #speech_annotated <- pblapply(text_meta$speech, function(s) as.data.frame(udpipe_annotate(udmodel_english, x = s, parser = "none")))
  #corpus_token <- lapply(speech_annotated, function(s) s[c("token")]) %>% unlist(.) %>% unname(.)
  #corpus_lemma <- lapply(speech_annotated, function(s) s[c("lemma")]) %>% unlist(.) %>% unname(.)
  #corpus_pos <- lapply(speech_annotated, function(s) s[c("upos")]) %>% unlist(.) %>% unname(.)
  #corpus_speechid <- lapply(seq(1,length(speech_annotated), 1), function(s) rep(text_meta$speech_id[s], nrow(speech_annotated[[s]]))) %>% unlist(.)
  #corpus_speakerid <- lapply(seq(1,length(speech_annotated), 1), function(s) rep(text_meta$speakerid[s], nrow(speech_annotated[[s]]))) %>% unlist(.)
  #corpus_party <- lapply(seq(1,length(speech_annotated), 1), function(s) rep(text_meta$party[s], nrow(speech_annotated[[s]]))) %>% unlist(.)
  #corpus_gender <- lapply(seq(1,length(speech_annotated), 1), function(s) rep(text_meta$gender[s], nrow(speech_annotated[[s]]))) %>% unlist(.)
  #corpus_chamber <- lapply(seq(1,length(speech_annotated), 1), function(s) rep(text_meta$chamber[s], nrow(speech_annotated[[s]]))) %>% unlist(.)
  #corpus_state <- lapply(seq(1,length(speech_annotated), 1), function(s) rep(text_meta$state[s], nrow(speech_annotated[[s]]))) %>% unlist(.)
  #corpus_sessionid <- lapply(seq(1,length(speech_annotated), 1), function(s) rep(text_meta$session_id[s], nrow(speech_annotated[[s]]))) %>% unlist(.)
  
  # store as list
  #corpus_meta <- list(corpus_token = corpus_token,
  #                    corpus_lemma = corpus_lemma,
  #                    corpus_pos = corpus_pos,
  #                    corpus_speechid = corpus_speechid,
  #                    corpus_speakerid = corpus_speakerid,
  #                    corpus_party = corpus_party,
  #                    corpus_gender = corpus_gender,
  #                    corpus_chamber = corpus_chamber,
  #                    corpus_state = corpus_state,
  #                    corpus_sessionid = corpus_sessionid)
  
  # save list
  #saveRDS(corpus_meta, paste0(out_path, "corpus_meta_", i, ".rds"))
  pb$tick()
}

corpus <- do.call(rbind, text_meta)
saveRDS(corpus, paste0(out_path, "corpus.rds"))
