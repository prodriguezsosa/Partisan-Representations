rm(list = ls())

# ================================
# load libraries
# ================================
library(ggplot2)
library(data.table)
library(magrittr)
library(stringr)

# ================================
#
# STEP - 1
# IDENTIFY BEST MODELS
#
# ================================

# ================================
# define paths
# ================================
in_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Post-Estimation/Loss/"

# ================================
# load data
# ================================
files <- list.files(in_path)
loss_list <- lapply(files, function(i) readRDS(paste0(in_path, i)))

# ================================
# choose best model for each group
# ================================
best_models_labels <- list()  # empty list
best_models_loss <- list()  # empty list
# for each MODEL-TEST select model with lowest mean loss
for(i in 1:length(loss_list)){
  loss_folds <- loss_list[[i]]
  mean_loss_folds <- unlist(lapply(loss_folds, mean))
  best_model_name <- names(which(mean_loss_folds == min(mean_loss_folds)))
  best_models_labels[i] <- best_model_name
  model_label <- gsub('[0-9]+', '', best_model_name)
  best_models_loss[model_label] <- do.call(c, loss_list)[best_model_name]
  
}

# keep only within models
best_models_loss <- lapply(list("RR", "DD", "MM", "FF"), function(i) best_models_loss[i]) %>% do.call(c,.)

# spring cleaning 
rm(loss_folds, loss_list, best_model_name, files, i, in_path, mean_loss_folds, model_label)

# ================================
# STEP - 2
# POST-LOSS COMPUATION
# see: ComputeLossFoldSpecificHPC.R
#
# ================================
# NOTE: given each group's best model we compute the loss for the best cross-model on the respective fold (on the HPC)
# e.g. given for R the best model is RR4 we evaluate the best D model (DD7) on fold 4 of the republican corpus
# e.g. given for F the best model is FF1 we evaluate the best M model (MM2) on fold 1 of the female corpus
# this way we compare each group's best performing model on the same data

# ================================
# define paths
# ================================
in_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Post-Estimation/BestModel/"

# ================================
# load data
# ================================
loss_files <- list.files(in_path)
cross_models_loss <- lapply(loss_files, function(i) readRDS(paste0(in_path, i)))
names(cross_models_loss) <- lapply(loss_files, function(i) unlist(str_split(i, "_"))[1]) %>% unlist(.) %>% gsub('[0-9]+', '', .)

# ================================
# mean difference tests
# ================================
# ttest function
ttest.data <- function(label, within_loss, across_loss){
  test.result <- t.test(within_loss, across_loss)
  test.data <- data.table(test = label, mean = (test.result$estimate[1] - test.result$estimate[2]), ci.upper = test.result$conf.int[1], ci.lower = test.result$conf.int[2], pvalue = test.result$p.value)
  return(test.data)
}

mean_loss_diff <- do.call(rbind, list(ttest.data("Female", best_models_loss[["FF"]], cross_models_loss[["MF"]]), 
                                      ttest.data("Male", best_models_loss[["MM"]], cross_models_loss[["FM"]]),
                                      ttest.data("Democrat", best_models_loss[["DD"]], cross_models_loss[["RD"]]),
                                      ttest.data("Republican", best_models_loss[["RR"]], cross_models_loss[["DR"]])))

# ================================
# mean tests plot
# plot (code borrowed from https://gist.github.com/dsparks/4332698)
# ================================
mean_loss_diff <- transform(mean_loss_diff, test = factor(test, levels=list("Republican", "Male", "Democrat", "Female")))  # order levels by coefficient magnitude
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


