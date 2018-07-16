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
# load data
# ================================
loss_files <- list.files(in_path)
mean_model_loss <- lapply(loss_files, function(i) readRDS(paste0(in_path, i)))
names(mean_model_loss) <- lapply(loss_files, function(i) unlist(str_split(i, "_"))[1]) %>% unlist(.) %>% gsub('[0-9]+', '', .)
mean_model_loss <- lapply(mean_model_loss, function(x) unlist(x))

# ================================
# mean difference tests
# ================================
# ttest function
ttest.data <- function(label, within_loss, across_loss){
  test.result <- t.test(within_loss, across_loss)
  test.data <- data.table(test = label, mean = (test.result$estimate[1] - test.result$estimate[2]), ci.upper = test.result$conf.int[1], ci.lower = test.result$conf.int[2], pvalue = test.result$p.value)
  return(test.data)
}

mean_loss_diff <- do.call(rbind, list(ttest.data("Female", mean_model_loss[["FF"]], mean_model_loss[["MF"]]), 
                                      ttest.data("Male", mean_model_loss[["MM"]], mean_model_loss[["FM"]]),
                                      ttest.data("Democrat", mean_model_loss[["DD"]], mean_model_loss[["RD"]]),
                                      ttest.data("Republican", mean_model_loss[["RR"]], mean_model_loss[["DR"]])))

# ================================
# mean tests plot
# plot (code borrowed from https://gist.github.com/dsparks/4332698)
# ================================
mean_loss_diff <- transform(mean_loss_diff, test = factor(test, levels=list("Male", "Democrat", "Female", "Republican")))  # order levels by coefficient magnitude
ggplot(mean_loss_diff, aes(colour = test)) +
  geom_linerange(aes(x = "", ymin = ci.lower, ymax = ci.upper), lwd = 2, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = "", y = mean, ymin = ci.lower, ymax = ci.upper), lwd = 1, position = position_dodge(width = 1/2), shape = 21, fill = "WHITE") +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  coord_flip() + theme_bw() +
  xlab("") + ylab("Mean Loss Difference") +
  theme(legend.title=element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(size=15),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.title = element_text(margin = margin(t = 10, r = 0, b = 20, l = 0), hjust = 0.5),
        text = element_text(size=15), legend.position = "bottom", legend.text = element_text(size=20)) +
  ggtitle("Within Model vs. Cross Model")

