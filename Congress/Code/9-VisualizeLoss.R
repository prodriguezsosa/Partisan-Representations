rm(list = ls())

# ================================
# load libraries
# ================================
library(ggplot2)
library(data.table)
library(magrittr)

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




