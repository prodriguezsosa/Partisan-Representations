library(ggplot2)
library(data.table)
library(magrittr)
library(dplyr)
rm(list=ls())

# upload mturk hits
in_path <- "/Users/pedrorodriguez/Drobox/GitHub/EmbeddingsProject/RShiny/Lexical Decision Task/Output/"
out_path <- "/Users/pedrorodriguez/Drobox/GitHub/EmbeddingsProject/R/Comparison"

mturk_hits <- as.list(list.files(in_path))

# function to clean HITs
cleanHIT <- function(hit_file){
  hit <- read.table(paste0(in_path, hit_file), sep = ",", header = TRUE)  # load hit
  hit <- hit[,2:ncol(hit)]
  hit <- data.frame(lapply(hit, as.character), stringsAsFactors=FALSE) 
  hit$winner <- ifelse(hit$left.choice == "TRUE", hit$left.source, hit$right.source)
  return(hit)
}

# apply function and bind results
hits <- lapply(mturk_hits, function(x) cleanHIT(x))
hits <- data.table(do.call(rbind, hits))
tasks_by_worker <- hits[, .N, by = "workerid"]
hits <- hits[!(workerid %in% tasks_by_worker[N >8, workerid]),] # somehow some workers where able to perform the HIT twice (6 in total)
hits <- hits[left.word != right.word,] # despite check, right - left words ended up being the same (<4% of cases)
saveRDS(hits, "/Users/pedrorodriguez/Drobox/GitHub/EmbeddingsProject/R/Comparison/hits.rds")

# tibble
hits[,.N, by = winner]
preference_tibble <- hits[,.(count = .N), by = c("cue", "winner")]
preference_tibble <- rbind(preference_tibble[winner == "H",][order(-count)], preference_tibble[winner == "M",][order(count)])
#preference_tibble <- rbind(preference_tibble, data.table("cue" = "animals", "winner" = "H", "count" = 0))
cues <- preference_tibble$cue[1:8]
preference_tibble$cue <- factor(preference_tibble$cue, levels = cues)

# plot
ggplot(preference_tibble, aes(x = cue, y = count, fill = factor(winner)))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Source", labels=c("Human", "Machine")) +
  xlab("Cue") + ylab("Vote Count") + ggtitle("Machine vs. Human Embeddings Vote Count (by cue)") + theme(legend.position="right", legend.title = element_blank(), panel.background = element_blank())

# mean result
mean_tibble <- hits
mean_tibble$winner <- dplyr::recode(mean_tibble$winner, "H" = 0, "M" = 1)
mean_preference <- mean_tibble[, mean(winner), by = workerid] %>% set_colnames(c("workerid", "mean"))
