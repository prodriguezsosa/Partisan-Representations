rm(list = ls())

# ================================
# load libraries
# ================================
library(ggplot2)
library(data.table)
library(magrittr)
library(stringr)

# ================================
# define paths
# ================================
in_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Post-Estimation/Loss/"

# ================================
# define groups
# ================================
GROUPS <- list("FF", "MM", "FM", "MF", "RR", "DD", "RD", "DR")

# ================================
# load data
# ================================
loss_list <- lapply(GROUPS, function(i) readRDS(paste0(in_path, i,"_loss_history.rds")))

# ================================
# mean tests
# ================================
# ttest function
ttest.data <- function(test_label, model_index, test_index){
  test.result <- t.test(unlist(loss_list[[model_index]]), unlist(loss_list[[test_index]]))
  test.data <- data.table(test = test_label, mean = (test.result$estimate[1] - test.result$estimate[2]), ci.upper = test.result$conf.int[1], ci.lower = test.result$conf.int[2], pvalue = test.result$p.value)
  return(test.data)
}

mean_loss_diff <- do.call(rbind, list(ttest.data("Female", 1, 4), ttest.data("Male", 2, 3), ttest.data("Republican", 5, 8), ttest.data("Democrat", 6, 7)))

# ================================
# mean tests plot
# ================================

# order levels
mean_loss_diff <- transform(mean_loss_diff, test = factor(test, levels=list("Female", "Male", "Democrat", "Republican")))
# plot (code borrowed from https://gist.github.com/dsparks/4332698)
ggplot(mean_loss_diff, aes(colour = test)) +
  geom_linerange(aes(x = "", ymin = ci.lower, ymax = ci.upper), lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = "", y = mean, ymin = ci.lower, ymax = ci.upper), lwd = 1/2, position = position_dodge(width = 1/2), shape = 21, fill = "WHITE") +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  coord_flip() + theme_bw() +
  xlab("") + ylab("Within Loss - Across Loss") +
  theme(legend.title=element_blank()) + theme(axis.ticks.y=element_blank()) +
  ggtitle("Within Model vs. Across Model Loss Difference")

# ================================
# choose best model
# ================================
best_models <- list()
for(i in 1:length(loss_list)){
  loss_folds <- loss_list[[i]]
  mean_loss_folds <- unlist(lapply(loss_folds, mean))
  best_models[[i]] <- names(which(mean_loss_folds == min(mean_loss_folds)))
}

best_models_loss <- lapply(best_models, function(i) do.call(c, loss_list)[i]) %>% do.call(c, .)

# ================================
# mean tests
# ================================
in_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Post-Estimation/BestModel/"

# ================================
# load data
# ================================
bm_loss_files <- list.files(in_path)
bm_loss_list <- lapply(bm_loss_files, function(i) readRDS(paste0(in_path, i)))

names(bm_loss_list) <- lapply(bm_loss_files, function(i) unlist(str_split(i, "_"))[1])

bm_loss <- list("FF" = best_models_loss[["FF1"]], "MF" = bm_loss_list[["MF1"]], "MM" = best_models_loss[["MM2"]], "FM" = bm_loss_list[["FM2"]],
                "RR" = best_models_loss[["RR4"]], "DR" = bm_loss_list[["DR4"]], "DD" = best_models_loss[["DD7"]], "RD" = bm_loss_list[["RD7"]])
  

# ttest function
ttest.data <- function(test_label, model_index, test_index){
  test.result <- t.test(bm_loss[[model_index]], bm_loss[[test_index]])
  test.data <- data.table(test = test_label, mean = (test.result$estimate[1] - test.result$estimate[2]), ci.upper = test.result$conf.int[1], ci.lower = test.result$conf.int[2], pvalue = test.result$p.value)
  return(test.data)
}

bm_mean_loss_diff <- do.call(rbind, list(ttest.data("Female", 1, 2), ttest.data("Male", 3, 4), ttest.data("Republican", 5, 6), ttest.data("Democrat", 7, 8)))

# ================================
# mean tests plot
# ================================

# order levels
bm_mean_loss_diff <- transform(bm_mean_loss_diff, test = factor(test, levels=list("Republican", "Male", "Democrat", "Female")))
# plot (code borrowed from https://gist.github.com/dsparks/4332698)
ggplot(bm_mean_loss_diff, aes(colour = test)) +
  geom_linerange(aes(x = "", ymin = ci.lower, ymax = ci.upper), lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = "", y = mean, ymin = ci.lower, ymax = ci.upper), lwd = 1/2, position = position_dodge(width = 1/2), shape = 21, fill = "WHITE") +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  coord_flip() + theme_bw() +
  xlab("") + ylab("Mean Loss Difference") +
  theme(legend.title=element_blank(), axis.ticks.y=element_blank(), text = element_text(size=15), plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.text = element_text(size=20)) +
  ggtitle("Within Model vs. Across Model")


