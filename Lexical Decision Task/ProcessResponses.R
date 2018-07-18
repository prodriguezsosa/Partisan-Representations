library(ggplot2)
library(data.table)
library(magrittr)
library(dplyr)
rm(list=ls())

# ================================
# set paths
# ================================
in_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Lexical Decision Task/Output/Congress/Party/"
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
count_tibble <- hits
count_tibble$winner <- dplyr::recode(count_tibble$winner, "R" = 0, "D" = 1)
count_preference <- count_tibble[, sum(winner), by = workerid] %>% set_colnames(c("workerid", "votes_D"))
count_preference$votes_R <- 4 - count_preference$votes_D
count_preference <- merge(count_preference, surveys, by = "workerid")

# recode
count_preference$party[count_preference$party == 8 | count_preference$party == 9] <- NA
count_preference$ideology[count_preference$ideology == 8] <- NA
count_preference$party[count_preference$party %in% c(1,2,3)] <- "D"
count_preference$party[count_preference$party == 4] <- "I"
count_preference$party[count_preference$party %in% c(5,6,7)] <- "R"

# ================================
# count preference by party
# ================================
#mean_preference <- mean_preference[,lapply(.SD,sum), by = party, .SDcols=c("votes_D", "votes_R")]
#mean_preference <- melt(mean_preference, measure.vars = c("votes_D", "votes_R"))

computeMean <- function(data, group){
  data.table(party = group, mean_D = data[party == group, mean(votes_D)], mean_R = data[party == group, mean(votes_R)])
}

computeSE <- function(data, group){
  data.table(party = group, se_D = data[party == group, sd(votes_D)/sqrt(4)], se_R = data[party == group, sd(votes_R)/sqrt(4)])
}

# apply functions
meand.dt <- lapply(list("D", "I", "R"), function(x) computeMean(count_preference, x)) %>% do.call(rbind, .) %>% melt(., measure.vars = c("mean_D", "mean_R"))
se.dt <- lapply(list("D", "I", "R"), function(x) computeSE(count_preference, x)) %>% do.call(rbind, .) %>% melt(., measure.vars = c("se_D", "se_R"))
plot.dt <- cbind(meand.dt[,c("party", "variable", "value")], se.dt[,value]) %>% set_colnames(c("party", "variable", "mean", "se"))
plot.dt$ci.upper <- plot.dt$mean + qnorm(1-0.025)*plot.dt$se
plot.dt$ci.lower <- plot.dt$mean - qnorm(1-0.025)*plot.dt$se

# plot
ggplot(plot.dt, aes(x = party, y = mean, fill = factor(variable))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Source", labels=c("D-Embeddings", "R-Embeddings")) +
  geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper), size=.3, width=.2, position=position_dodge(.9)) +
  xlab("Party") + ylab("Average Vote Count \n (out of 4 votes)") + ggtitle("D-Embeddings vs. R-Embeddings \n Vote Count by Party") + 
  theme(legend.title=element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(size=15),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 10, r = 0, b = 20, l = 0), hjust = 0.5),
        text = element_text(size=15), legend.position = "bottom", legend.text = element_text(size=20))

# ttest function
ttest.data <- function(party, results1, results2){
  test.result <- t.test(results1, results2)
  test.data <- data.table(party = party, mean = test.result$estimate[1] - test.result$estimate[2], ci.upper = test.result$conf.int[1], ci.lower = test.result$conf.int[2], pvalue = test.result$p.value)
  return(test.data)
}


vote_counts <- do.call(rbind, list(ttest.data("D", count_preference[party == "D", votes_D], count_preference[party == "D", votes_R]),
                                   ttest.data("I", count_preference[party == "I", votes_D], count_preference[party == "I", votes_R]),
                                   ttest.data("R", count_preference[party == "R", votes_D], count_preference[party == "R", votes_R])))
                       
# plot
ggplot(vote_counts, aes(x = party, y = mean, fill = factor(party))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=ci.lower, ymax=ci.upper), size=.3, width=.2, position=position_dodge(.9)) +
  xlab("Party") + ylab("Mean Difference \n Votes D - Votes R") + ggtitle("D-Embeddings vs. R-Embeddings \n Mean Difference by Party") + 
  theme(legend.title=element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(size=15),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 10, r = 0, b = 20, l = 0), hjust = 0.5),
        text = element_text(size=15), legend.position = "bottom", legend.text = element_text(size=20))



