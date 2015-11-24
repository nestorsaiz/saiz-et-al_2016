# Load necessary packages
library(ggplot2)
library(plyr)
library(dplyr)
library(knitr)

# Run the following custom functions
source('stage.R')
source('correct.fgf.R')
identify <- function(dataframe) {
  ## Calculate the ratio of GATA6 to NANOG levels 
  ## If raw data table, run 'correct' first
  dataframe['G6toNG'] <- dataframe$CH4.logCor / dataframe$CH5.logCor
  dataframe['Identity.auto'] <- 'NA'
  dataframe$Identity.auto <- ifelse(dataframe$Treatment == 'Littermate', 
                                    ifelse(dataframe$Identity == 'TE', 'TE', 
                                           ifelse((dataframe$CH4.logCor < 0.5 &
                                                     dataframe$CH5.logCor < 1.25), 'DN', 
                                                  ifelse(abs(dataframe$G6toNG) > 1.5 | 
                                                           (dataframe$CH4.logCor > 0.5 & 
                                                              dataframe$CH5.logCor <1.25), 'PRE', 
                                                         ifelse(abs(dataframe$G6toNG) < 0.66 | 
                                                                  (dataframe$CH5.logCor > 0.5 & 
                                                                     dataframe$CH4.logCor < 1.25), 
                                                                'EPI', 'DP')))), 
                                    ifelse(dataframe$Identity == 'TE', 'TE',
                                           ifelse((dataframe$CH4.logCor < 4 &
                                                     dataframe$CH5.logCor < 4), 'DN', 
                                                  ifelse(abs(dataframe$G6toNG) > 1.1 | 
                                                           (dataframe$CH4.logCor > 3 & 
                                                              dataframe$CH5.logCor <4), 'PRE', 
                                                         ifelse(abs(dataframe$G6toNG) < 0.9 | 
                                                                  (dataframe$CH5.logCor > 4 & 
                                                                     dataframe$CH4.logCor < 4), 
                                                                'EPI', 'DP')))))
  dataframe$Identity.auto <- as.factor(dataframe$Identity.auto)
  return(dataframe)
}

# Load FGF_all_pooled csv file
FGF.all <- read.csv('FGF_all_pooled.csv', header = TRUE, sep = ',')

# Add TE vs ICM column
FGF.all$TE_ICM <- ifelse(FGF.all$Identity == 'TE', 'TE', 'ICM')
FGF.all$TE_ICM <- factor(FGF.all$TE_ICM, levels = c('ICM', 'TE'))

# Add experimental point:
# start (sp): reference littermates ('Littermate') 
# exchange (xp): media exchange point of reference
# for regimes 8 and 9; 24h timepoint for regime 1
# end (ep): end of experiment (120-150 cells)
FGF.all$Xpoint <- ifelse(FGF.all$Regime == 'R1' & 
                           FGF.all$Tt_length == '24h' | 
                           FGF.all$Regime == 'R8' & 
                           FGF.all$Tt_length == '24h' | 
                           FGF.all$Regime == 'R9' & 
                           FGF.all$Tt_length == '30h', 
                         'xp', 
                         ifelse(FGF.all$Treatment == 'Littermate', 
                                'sp', 
                                'ep'))
FGF.all$Xpoint <- factor(FGF.all$Xpoint, levels = c('sp', 'xp', 'ep'))

# Stage embryos
FGF.all <- stage(FGF.all)

# Add temporary marker variable
FGF.all$Markers <- ifelse(FGF.all$Experiment == '060915_R3' & 
                            FGF.all$Treatment != 'Littermate' | 
                            FGF.all$Experiment == '060915_R4' & 
                            FGF.all$Treatment != 'Littermate', 'O4G4NG', 'C2G6NG')
FGF.all$Markers <- as.factor(FGF.all$Markers)

# Z-correction
FGF.all <- correct.fgf(FGF.all)

# Identity assignment
FGF.all <- identify(FGF.all)

# Order the variables as desired
FGF.all$Regime <- factor(FGF.all$Regime, 
                         levels = c('R1', 'R5', 'R3', 'R3L', 'R4', 'R6', 'R8', 'R9'))
FGF.all$Treatment <- factor(FGF.all$Treatment, 
                            levels = c('Littermate', 'Control', 
                                       'FGF4_1000', 'PD03_1', 
                                       'AZD_1', 'SU_10', 'SU_20', 'FGF42PD'))
FGF.all$Identity <- factor(FGF.all$Identity, 
                           levels = c('TE', 'DN', 'EPI', 'DP', 'PRE'))
FGF.all$Identity.auto <- factor(FGF.all$Identity.auto, levels = c('TE', 'DN', 'EPI', 'DP', 'PRE'))
FGF.all$TE_ICM <- factor(FGF.all$TE_ICM, levels = c('ICM', 'TE'))

# Check structure
str(FGF.all)