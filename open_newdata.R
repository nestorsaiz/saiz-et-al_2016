# Load necessary packages
library(ggplot2)
library(plyr)
library(dplyr)
library(knitr)

# Run the following custom functions
source('stage.R')
source('correct.fgf.R')
source('identify.R')

# Load in transformed data
FGF.all <- readRDS('FGF-id-by-clustering-final.rds')
FGF.exp.ref <- read.csv('FGFonCD1_exp_ref.csv')
FGF.all <- merge(FGF.all, FGF.exp.ref)

# Assign identities automatically
FGF.all <- id.linear(FGF.all)
# Order factors
FGF.all$Identity <- factor(FGF.all$Identity, 
                           levels = c('DN', 'EPI', 'DP', 'PRE', 'TE'))
FGF.all$Identity.km <- factor(FGF.all$Identity.km, 
                                levels = c('DN', 'EPI', 'DP', 'PRE', 'ICM', 'TE'))
FGF.all$Regime <- factor(FGF.all$Regime, 
                         levels = c('R1', 'R5', 'R3', 'R3L', 'R4', 'R6', 'R8', 'R9'))
FGF.all$Treatment <- factor(FGF.all$Treatment, 
                            levels = c('Littermate', 'Control', 
                                       'FGF4_1000', 'PD03_1', 
                                       'AZD_1', 'SU_10', 'SU_20', 'FGF42PD'))
FGF.all$TE_ICM <- factor(FGF.all$TE_ICM, levels = c('ICM', 'TE'))
FGF.all$Xpoint <- factor(FGF.all$Xpoint, levels = c('sp', 'xp', 'ep'))
# Stage embryos
FGF.all <- stage(FGF.all)


