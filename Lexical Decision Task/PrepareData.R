#rm(list = ls())

out_file <- "/Users/pedrorodriguez/Drobox/GitHub/Partisan-Representations/Lexical Decision Task/data/"

# top-5
N <- 5
token <- "healthcare"
republicans <- data.frame("republican" = c(token, TopicDiffsList[[token]][["R"]][1:N]), stringsAsFactors = FALSE)
democrats <- data.frame("democrat" = c(token, TopicDiffsList[[token]][["D"]][1:N]), stringsAsFactors = FALSE)
females <- data.frame("female" = c(token, TopicDiffsListFM[[token]][["F"]][1:N]), stringsAsFactors = FALSE)
males <- data.frame("male" = c(token, TopicDiffsListFM[[token]][["M"]][1:N]), stringsAsFactors = FALSE)
topN <- data.frame(republicans, democrats, females, males, stringsAsFactors = FALSE)
print(topN)
#topN[topN == "vas"] <- "veterans affairs"

# trial data
trial_data <- list(c("food", "nutrition", "ship", TRUE, FALSE))#,
                   #c("forest", "juice", "woodland", FALSE, TRUE))

# screening data
screening_data <- list(#c("church", "kitten", "temple", FALSE, TRUE),
                       #c("gradual", "slow", "quick", TRUE, FALSE)),
                       c("laugh", "elect", "giggle", FALSE, TRUE))
                       #c("soccer", "goal", "mousetrap", TRUE, FALSE),
                       #c("brochure", "water", "booklet", FALSE, TRUE))
                       #c("dial", "ring", "drive", TRUE, FALSE),
                       #c("break", "fracture", "mail", TRUE, FALSE),
                       #c("party", "wooden", "celebration", FALSE, TRUE),
                       #c("sick", "doctor", "brick", TRUE, FALSE),
                       #c("anatomy", "automobile", "body", FALSE, TRUE))

# save top-5
saveRDS(topN, paste0(out_file, "topN.rds"))
saveRDS(screening_data, paste0(out_file, "screening_data.rds"))
saveRDS(trial_data, paste0(out_file, "trial_data.rds"))