# Load in transformed data
FGF.all <- readRDS('FGF_all_pooled_trans.rds')

# Assign identities automatically
FGF.all <- identify(FGF.all)
# Order factors in Identity.auto
FGF.all$Identity.auto <- factor(FGF.all$Identity.auto, 
                                levels = c('ICM', 'TE', 'DN', 'EPI', 'DP', 'PRE'))
# Stage embryos
FGF.all <- stage(FGF.all)
