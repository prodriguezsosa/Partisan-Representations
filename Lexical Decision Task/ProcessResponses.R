library(ggplot2)
library(data.table)
library(magrittr)
library(dplyr)
rm(list=ls())

# ================================
# set paths
# ================================
in_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Lexical Decision Task/Output/Congress/"
#out_path <- "PATH HERE"

# ================================
# functions
# ================================
# function to clean LDTs data
cleanLDT <- function(hit_file){
  hit <- read.table(paste0(in_path, hit_file), sep = ",", header = TRUE)  # load hit
  hit <- data.frame(lapply(hit, as.character), stringsAsFactors=FALSE) 
  hit$winner <- ifelse(hit$left.choice == "TRUE", hit$left.source, hit$right.source)
  return(hit)
}

# function to clean survey data
cleanSurvey <- function(hit_file){
  hit <- read.table(paste0(in_path, hit_file), sep = ",", header = TRUE)  # load hit
  return(hit)
}

# ================================
# load data
# ================================
mturk_hits <- as.list(list.files(in_path))
ldt_data <- mturk_hits[grepl("_ldt.csv", mturk_hits)]
survey_data <- mturk_hits[grepl("_survey.csv", mturk_hits)]

# apply function and bind results LDT data
ldts <- lapply(ldt_data, function(x) cleanLDT(x))
ldts <- data.table(do.call(rbind, ldts))

# apply function and bind results survey data
surveys <- lapply(survey_data, function(x) cleanSurvey(x))
surveys <- data.table(do.call(rbind, surveys))

# merge by workerid
hits <- merge(ldts, surveys, by = "workerid")

# ================================
# clean data
# ================================
# check for duplicates
tasks_by_worker <- hits[, .N, by = "workerid"]
hits <- hits[!(workerid %in% tasks_by_worker[N > 4, workerid]),] # somehow some workers where able to perform the HIT twice (6 in total)
hits <- hits[left.word != right.word,] # despite check, right - left words ended up being the same (<4% of cases)
#saveRDS(hits, "")

# ================================
# count preference by workerid
# ================================
mean_tibble <- hits
mean_tibble$winner <- dplyr::recode(mean_tibble$winner, "R" = 0, "D" = 1)
mean_preference <- mean_tibble[, sum(winner), by = workerid] %>% set_colnames(c("workerid", "votes_D"))
mean_preference$votes_R <- 4 - mean_preference$votes_D
mean_preference <- merge(mean_preference, surveys, by = "workerid")

# recode
mean_preference$party[mean_preference$party == 8 | mean_preference$party == 9] <- NA
mean_preference$ideology[mean_preference$ideology == 8] <- NA
mean_preference$party[mean_preference$party %in% c(1,2,3)] <- "D"
mean_preference$party[mean_preference$party == 4] <- "I"
mean_preference$party[mean_preference$party %in% c(5,6,7)] <- "R"

# ================================
# count preference by party
# ================================
mean_preference <- mean_preference[,lapply(.SD,sum), by = party, .SDcols=c("votes_D", "votes_R")]
mean_preference <- melt(mean_preference, measure.vars = c("votes_D", "votes_R"))

# plot
ggplot(mean_preference, aes(x = party, y = value, fill = factor(variable)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Source", labels=c("D-Embeddings", "R-Embeddings")) +
  xlab("Party") + ylab("Vote Count") + ggtitle("D-Embeddings vs. R-Embeddings \n Vote Count by Party") + 
  theme(legend.title=element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(size=15),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 10, r = 0, b = 20, l = 0), hjust = 0.5),
        text = element_text(size=15), legend.position = "bottom", legend.text = element_text(size=20))




