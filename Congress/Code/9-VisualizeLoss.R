

in_path <- "/Users/pedrorodriguez/Dropbox/GitHub/Partisan-Representations/Congress/Post-Estimation/Loss/"

GROUPS <- list("FF", "MF", "MM", "FM")

loss_list <- lapply(GROUPS, function(i) readRDS(paste0(in_path, i,"_loss_history.rds")))

t.test( unlist(loss_list[[1]]), unlist(loss_list[[4]]))
t.test( unlist(loss_list[[3]]), unlist(loss_list[[2]]))

# ================================
# classifier accuracy
# ================================