# Load necessary packages
library(ggplot2)
library(plyr)
library(dplyr)
library(knitr)

# Run the following custom functions
source('stage.R')
source('correct.fgf.R')
source('identify.R')

# Load FGF_all_pooled csv file
FGF.all <- read.csv('FGF_all_pooled_raw.csv', header = TRUE)
# Load experimental reference file (metadata)
exp_ref <- read.csv('FGFonCD1_exp_ref.csv', header = TRUE)
# Combine the two
FGF.all <- merge(FGF.all, exp_ref)

# Add TE vs ICM column based on manual identity assignment
FGF.all$TE_ICM <- ifelse(FGF.all$Identity == 'TE', 'TE', 'ICM')

# Stage embryos
FGF.all <- stage(FGF.all)

# Order the variables as desired
FGF.all$Regime <- factor(FGF.all$Regime, 
                         levels = c('R1', 'R5', 'R3', 'R3L', 
                                    'R4', 'R6', 'R8', 'R9'))
FGF.all$Treatment <- factor(FGF.all$Treatment, 
                            levels = c('Littermate', 'Control', 
                                       'FGF4_1000', 'PD03_1', 
                                       'AZD_1', 'SU_10', 'SU_20', 'FGF42PD'))
FGF.all$Identity <- factor(FGF.all$Identity, 
                           levels = c('TE', 'DN', 'EPI', 'DP', 'PRE'))
FGF.all$TE_ICM <- factor(FGF.all$TE_ICM, levels = c('ICM', 'TE'))
FGF.all$Xpoint <- factor(FGF.all$Xpoint, levels = c('sp', 'xp', 'ep'))

# Check structure
str(FGF.all)
