# Saiz *et al* (2016)
# Figures 4 and S6 plots

# Load data and apply transformations
source('Transformations.R')
source('tables.R')

# Figure 4b

fig4b <- qplot(CH4.logCor, CH5.logCor, 
               data = FGF.all %>% 
                       filter(TE_ICM != 'TE', 
                              Xpoint == 'xp', 
                              Regime == 'R9'),
               color = Identity.auto, 
               ylim = c(-3, 8), xlim = c(-3, 8)) + 
        facet_grid( ~ Treatment) + 
        scale_color_manual(values = c('EPI' = 'red', 'PRE' = 'blue', 
                                      'DP' = 'purple', 'DN' = 'gray')) + 
        theme_bw() + coord_fixed()
print(fig4b)

# Figure 4d&h

fig4dh <- qplot(CH4.logCor, CH5.logCor, 
                data = FGF.all %>% 
                        filter(TE_ICM != 'TE', 
                               #Treatment != 'FGF42PD', 
                               Xpoint == 'ep', 
                               Regime == 'R9'),
                color = Identity.auto, 
                ylim = c(0, 8), xlim = c(0, 8)) + 
        facet_grid( ~ Treatment) + 
        scale_color_manual(values = c('EPI' = 'red', 'PRE' = 'blue', 
                                      'DP' = 'purple', 'DN' = 'gray')) + 
        theme_bw() + coord_fixed()
print(fig4dh)

# Figure 4f

fig4f <- qplot(Treatment, data = FGF.all %>%
                       filter(Regime == 'R9', 
                              TE_ICM != 'TE', 
                              Treatment != 'FGF42PD'), 
               fill = Identity, geom = 'bar', position = 'fill') + 
        scale_fill_manual(values = c('EPI' = 'red', 'PRE' = 'blue', 
                                     'DP' = 'purple', 'DN' = 'gray')) + 
        facet_wrap( ~ Xpoint) + 
        theme_bw() + coord_fixed(5)
print(fig4f)

# Figure S6b

figS6b <- qplot(Treatment, Cellcount, 
                data = FGF.all %>%
                        group_by(Embryo_ID,
                                 Regime,
                                 Treatment, 
                                 Experimenter, 
                                 Xpoint, 
                                 Cellcount, 
                                 Stage) %>% 
                        summarise() %>% 
                        filter(Regime == 'R8' | 
                                       Regime == 'R9', 
                               Treatment != 'FGF42PD'), 
                geom = c('boxplot', 'jitter'), 
                fill = I('gray')) + coord_fixed(0.03) + 
        theme_bw() + facet_grid(Regime ~ Xpoint)
print(figS6b)

# Figure S6c

figS6c <- qplot(CH4.logCor, CH5.logCor, 
                data = FGF.all %>% 
                        filter(TE_ICM != 'TE', 
                               Treatment != 'FGF42PD', 
                               Regime == 'R8', 
                               Xpoint == 'xp'),
                color = Identity, 
                xlim = c(-3, 8), ylim = c(-3, 8)) + 
        facet_grid( ~ Treatment) + 
        scale_color_manual(values = c('EPI' = 'red', 'PRE' = 'blue', 
                                      'DP' = 'purple', 'DN' = 'gray')) + 
        theme_bw() + coord_fixed()
print(figS6c)

# Figure S6d

figS6d <- qplot(Treatment, data = FGF.all %>%
                        filter(Regime == 'R8', 
                               TE_ICM != 'TE'), 
                fill = Identity, geom = 'bar', position = 'fill') + 
        scale_fill_manual(values = c('EPI' = 'red', 'PRE' = 'blue', 
                                     'DP' = 'purple', 'DN' = 'gray')) + 
        facet_wrap( ~ Xpoint) + 
        theme_bw() + coord_fixed(5)
print(figS6d)

# Figure S6e

# This code generates a single plot for each box on Figure S6e
# and compiles them onto a .pdf file that is saved to the working directory
# Plots were arranged for Figure S6e manually in Adobe Illustrator

pdf(file = 'ICM_R9.pdf', paper = 'letter')
regimes <- c('R9')
regimes <- as.factor(regimes)
treatments <- c('Control', 'FGF4_1000', 'PD03_1', 'FGF42PD')
treatments <- as.factor(treatments)
for (r in regimes){
        for (t in treatments){
                plot <- qplot(Embryo_ID, data = FGF.all %>% 
                                      filter(Treatment == t, 
                                             Xpoint == 'ep', 
                                             Regime == r, 
                                             TE_ICM != 'TE'), 
                              fill = Identity.auto, geom = 'bar', position = 'fill', 
                              main = r, xlab = t) + 
                        scale_fill_manual(values = c('EPI' = 'red', 'PRE' = 'blue', 
                                                     'DP' = 'purple', 'DN' = 'gray')) + 
                        theme_bw() + coord_fixed(10)
                print(plot)
        }
}
dev.off()