## Load necessary packages
library(reshape2)
library(ggplot2)
library(plyr)
library(dplyr)
library(locfit)

## Run the following custom functions
source('stage.R')
source('correct.fgf.R')
source('identify.R')

## Load FGF_all_pooled csv file
FGF.all <- read.csv('FGF_all_pooled_raw.csv', header = TRUE)
## Load experimental reference file (metadata)
exp.ref <- read.csv('FGFonCD1_exp_ref.csv', header = TRUE)
## Combine the two
FGF.all <- merge(FGF.all, exp.ref)

## Load scaling experiments data
scaling <- read.csv('scaling_pooled.csv', header = TRUE)
## Load experimental reference file (metadata)
scal.exp.ref <- read.csv('scaling_exp_ref.csv', header = TRUE)
## Combine the two
scaling <- merge(scaling, scal.exp.ref)

## Incorporate Controls cultured for 48h from the 8-cell stage (Regime 1)
## into the scaling dataset
controls.r1 <- subset(FGF.all, Treatment == 'Control' &
                              Xpoint == 'ep' & 
                              Regime == 'R1')
scaling <- rbind.fill(scaling, controls.r1)
## Assign Regime 1 to all scaling embryos (all 48h culture from same stage)
## and wild type genotype
scaling$Regime <- 'R1'
scaling$Regime <- as.factor(scaling$Regime)
scaling$Genotype <- 'WT'
scaling$Genotype <- as.factor(scaling$Genotype)

## Add TE vs ICM column based on manual identity assignment
FGF.all$TE_ICM <- ifelse(FGF.all$Identity == 'TE', 'TE', 'ICM')
scaling$TE_ICM <- ifelse(scaling$Identity == 'TE', 'TE', 'ICM')

## Stage embryos
FGF.all <- stage(FGF.all)

## Apply correction for Z axis-associated fluorescence decay
## (will only be used for embryos stained with OCT4, GATA4 and NANOG: Markers = "O4G4NG")
FGF.all <- correct.fgf(FGF.all)

## Apply Empirical Bayes correction to each dataset
source('empirical_bayes.R')
FGF.all <- eb.cor(FGF.all)
scaling <- eb.cor(scaling)

## Assign cell identity within the ICM automatically
source('identities.R')

# Order the variables as desired
FGF.all$Regime <- factor(FGF.all$Regime, 
                         levels = c('R1', 'R5', 'R3', 'R4', 'R6', 'R8', 'R9'))
FGF.all$Treatment <- factor(FGF.all$Treatment, 
                            levels = c('Littermate', 'Control', 
                                       'FGF4_1000', 'PD03_1', 
                                       'AZD_1', 'SU_10', 'SU_20', 'FGF42PD'))
scaling$Treatment <- factor(scaling$Treatment, levels = c('Control', 'Single', 
                                                          'Half', 'Double'))
FGF.all$Identity <- factor(FGF.all$Identity, 
                           levels = c('TE', 'DN', 'EPI', 'DP', 'PRE'))
scaling$Identity <- factor(scaling$Identity, 
                           levels = c('DN', 'EPI', 'DP', 'PRE', 'TE'))
FGF.all$Identity.km <- factor(FGF.all$Identity.km, 
                              levels = c('DN', 'EPI', 'DP', 'PRE', 'TE'))
scaling$Identity.km <- factor(scaling$Identity.km, 
                              levels = c('DN', 'EPI', 'DP', 'PRE', 'TE'))
FGF.all$TE_ICM <- factor(FGF.all$TE_ICM, levels = c('ICM', 'TE'))
scaling$TE_ICM <- factor(scaling$TE_ICM, levels = c('ICM', 'TE'))
FGF.all$Xpoint <- factor(FGF.all$Xpoint, levels = c('sp', 'xp', 'ep'))
