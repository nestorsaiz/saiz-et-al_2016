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

fig3b <- cloud(CH2.logCor ~ CH4.logCor * CH5.logCor | 
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
               data = FGF.ICMsum %>% 
                       filter(Markers == 'O4G4NG', 
                              Identity != 'DP'), 
               fill = Identity, 
               ylab = 'log[OCT4]', 
               geom = c('boxplot', 'jitter')) + 
        scale_fill_manual(values = idcols) + 
        facet_grid( ~ Treatment) + 
        theme_bw() + coord_fixed()
print(fig3c)

# Figure S5a

figS5a <- qplot(Regime, CH5, 
                data = FGF.ICMsum %>%
                        filter(Treatment != 'Littermate', 
                               Treatment != 'SU_10', 
                               Xpoint != 'xp', 
                               Markers == 'C2G6NG', 
                               Regime != 'R8', 
                               Regime != 'R9', 
                               Regime != 'R3L', 
                               Identity.auto == 'EPI' | 
                                       Identity.auto == 'PRE'), 
                fill = Identity.auto, 
                geom = c('boxplot', 'jitter'), 
                ylab = 'log[NANOG]') + 
        facet_grid( ~ Treatment) + 
        scale_fill_manual(values = idcols) + 
        theme_bw() + coord_fixed(1)
print(figS5a)

# Figure S5b

figS5b <- qplot(Identity, CH2.logCor,
                data = FGF.all %>% 
                        filter(Markers == 'O4G4NG', 
                               Identity != 'DP'), 
                fill = Identity, 
                ylab = 'log[OCT4]', 
                geom = c('boxplot', 'jitter')) + 
        scale_fill_manual(values = idcols) + 
        facet_grid( ~ Treatment) + 
        theme_bw() + coord_fixed()
print(figS5b)