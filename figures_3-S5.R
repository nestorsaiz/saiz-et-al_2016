# Saiz *et al* (2016)
# Figures 3 and S5 plots
library(lattice)

# Load data and apply transformations if not loaded yet
data.ok <- exists('FGF.all') 
tables.ok <- exists('FGF.sum')
if (data.ok == FALSE) {
        source('Transformations.R')
}
rm(data.ok)
if (tables.ok == FALSE) {
        source('tables.R')
}
rm(tables.ok)

# Create object for identity colors
idcols <- c('EPI' = 'red', 'PRE' = 'blue', 'DP' = 'purple', 'DN' = 'gray', 
            'TE' = 'green', 'ICM' = 'purple')

# Figure 3b
## 3D scatterplot showing the levels of OCT4, GATA4 and NANOG
## in ICM cells of embryos treated in all conditions
## collected at 64-120 cells and cultured as described in Figure 2a
## Each dot represents a single ICM cell
fig3b <- cloud(CH2.logCor ~ CH4.ebLogCor * CH5.ebLogCor | 
                       factor(Treatment, labels = c('Control', 'FGF4', 'ERKi', 
                                                    'AZD4547', 'SU5402')) + 
                       factor(TE_ICM), 
               data = subset(FGF.all, Markers == 'O4G4NG' & TE_ICM == 'ICM'), 
               groups = factor(Identity, labels = c('EPI', 'DP', 'PRE')), 
               pch = 20, col = c('red', 'purple', 'blue'), 
               screen = list(x = 290, y = 40, z = 12), 
               zlab = 'log[OCT4]', xlab = 'log[GATA4]', ylab = 'log[NANOG]', 
               main = 'OCT4 vs NANOG vs GATA4 expression in upon FGF modulation')
print(fig3b)

# Figure 3c
## Boxplots showing the level of OCT4 in log scale
## in epiblast (NANOG+) and PrE (GATA4+) cells
## for embryos in Figure 3a and b
## Each dot is teh average level per lineage per embryo
## for each treatment condition and regime
fig3c <- ggplot(FGF.ICMsumO4 %>% 
                        ## Select embryos stained for OCT4, GATA4 and NANOG only
                        ## Identity has been assigned manually
                        filter(Markers == 'O4G4NG', 
                               Identity != 'DP'), 
                ## Plot identity (EPI or PrE) against OCT4 (Channel 2)
                aes(x = Identity, y = CH2))
## Set up plot aesthetics
fig3c <- fig3c + geom_boxplot(aes(fill = Identity), color = I('black'))
fig3c <- fig3c + geom_jitter(color = I('black'), size = I(1.2))
fig3c <- fig3c + scale_fill_manual(values = idcols)
fig3c <- fig3c + facet_grid( ~ Treatment)
fig3c <- fig3c + theme_bw() + coord_fixed()
## Print plot to the screen
print(fig3c)

# Figure S5a
## Box plot showing levels of NANOG in epiblast and PrE cells
## for embryos in Figure 2
## treated in all conditions (FGF4, MEKi (PD03) and FGFRis (AZD4547 & SU5402))
## Each dot is the average level per lineage per embryo 
## for each treatment condition and regime
figS5a <- ggplot(FGF.ICMsum %>%
                         ## Select EPI and PRE cells in all embryos 
                         ## (except Littermates and 10uM SU5402)
                         ## Stained with CDX2, GATA6 and NANOG
                         ## for treatment regimes shown in Figure 2
                         filter(Treatment != 'Littermate', 
                                Treatment != 'SU_10', 
                                Xpoint != 'xp', 
                                Markers == 'C2G6NG', 
                                Regime != 'R8', 
                                Regime != 'R9', 
                                Identity.km == 'EPI' | 
                                        Identity.km == 'PRE'), 
                 aes(x = Regime, y = CH5))
## Set up plot aesthetics
figS5a <- figS5a + geom_boxplot(aes(fill = Identity.km), color = 'black', 
                                outlier.shape = 1)
figS5a <- figS5a + geom_jitter(color = 'black', size = I(1.2))
figS5a <- figS5a + scale_fill_manual(values = idcols)
figS5a <- figS5a + labs(y = 'log[NANOG]', fill = 'Identity')
figS5a <- figS5a + theme_bw() + coord_fixed(1)
## Group by treatment condition
figS5a <- figS5a + facet_grid( ~ Treatment)
## Print plot to the screen
print(figS5a)

# Figure S5b
## Box plot showing OCT4 levels in each lineage (TE, EPI and PrE)
## of embryos stained with OCT4, GATA4 and NANOG
## Levels per cell (not average per lineage)
## Each dot represents a single cell 
figS5b <- ggplot(FGF.all %>% 
                         ## Select TE, EPI and PrE cells
                         ## stained with OCT4, GATA4 and NANOG
                         filter(Markers == 'O4G4NG', 
                                Identity != 'DP'),
                 ## Plot log(OCT4 level) for each identity
                 aes(x = Identity, y = CH2.logCor))
## Set up plot aesthetics
figS5b <- figS5b + geom_boxplot(aes(fill = Identity), color = 'black')
figS5b <- figS5b + geom_jitter(color = 'black', size = I(1.2))
figS5b <- figS5b + labs(y = 'log[OCT4]')
figS5b <- figS5b + scale_fill_manual(values = idcols)
figS5b <- figS5b + theme_bw() + coord_fixed()
## Group by treatment condition
figS5b <- figS5b + facet_grid( ~ Treatment)
## Print plot to the screen
print(figS5b)