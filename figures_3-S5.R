# Saiz *et al* (2016)
# Figures 3 and S5 plots

library('lattice')

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

fig3c <- qplot(Identity, CH2,
               data = FGF.ICMsumO4 %>% 
                       filter(Markers == 'O4G4NG', 
                              Identity != 'DP'), 
               fill = Identity, 
               ylab = 'log[OCT4]', ylim = c(3, 5.5),
               geom = c('boxplot', 'jitter'), color = I('black')) + 
        scale_fill_manual(values = idcols) + 
        facet_grid( ~ Treatment) + 
        theme_bw() + coord_fixed()
print(fig3c)

# Figure S5a

figS5a <- ggplot(FGF.ICMsum %>%
                         filter(Treatment != 'Littermate', 
                                Treatment != 'SU_10', 
                                Xpoint != 'xp', 
                                Markers == 'C2G6NG', 
                                Regime != 'R8', 
                                Regime != 'R9', 
                                Regime != 'R3L', 
                                Identity.auto == 'EPI' | 
                                        Identity.auto == 'PRE'), 
                 aes(x = Regime, y = CH5))
figS5a <- figS5a + geom_boxplot(aes(fill = Identity.auto), color = 'black')
figS5a <- figS5a + geom_jitter(color = 'black', size = I(1.2))
figS5a <- figS5a + scale_fill_manual(values = idcols)
figS5a <- figS5a + labs(y = 'log[NANOG]', fill = 'Identity')
figS5a <- figS5a + facet_grid( ~ Treatment)
figS5a <- figS5a + theme_bw() + coord_fixed(1)
print(figS5a)

# Figure S5b

figS5b <- ggplot(FGF.all %>% 
                         filter(Markers == 'O4G4NG', 
                                Identity != 'DP'),
                 aes(x = Identity, y = CH2.logCor))
figS5b <- figS5b + geom_boxplot(aes(fill = Identity), color = 'black')
figS5b <- figS5b + geom_jitter(color = 'black', size = I(1.2))
figS5b <- figS5b + labs(y = 'log[OCT4]')
figS5b <- figS5b + facet_grid( ~ Treatment)
figS5b <- figS5b + scale_fill_manual(values = idcols)
figS5b <- figS5b + theme_bw() + coord_fixed()
print(figS5b)