# Saiz *et al* (2016)
# Figures 2, S2, S3 and S4 plots

# Load data and apply transformations
source('Transformations.R')
source('tables.R')

# Figure 2b

fig2b <- qplot(CH4.logCor, CH5.logCor, 
               data = FGF.all %>% 
                       filter(Treatment == 'Control' | 
                                      Treatment == 'FGF4_1000' | 
                                      Treatment == 'PD03_1',
                              Xpoint != 'xp', 
                              TE_ICM != 'TE', 
                              Markers == 'C2G6NG', 
                              Regime != 'R8', 
                              Regime != 'R9', 
                              Regime != 'R3L'),
               color = Identity.auto, 
               xlim = c(0, 8), ylim = c(0, 8)#, alpha = I(0.25)
               ) + 
        facet_grid(Regime ~ Treatment) + 
        scale_color_manual(values = c('EPI' = 'red', 'PRE' = 'blue', 
                                      'DP' = 'purple', 'DN' = 'gray')) + 
        theme_bw() + coord_fixed()
print(fig2b)

# Figure 2d

fig2d <- qplot(Regime, data = FGF.all %>% 
                       filter(Treatment == 'Control' | 
                                      Treatment == 'FGF4_1000' | 
                                      Treatment == 'PD03_1', 
                              Xpoint != 'xp', 
                              TE_ICM != 'TE', 
                              Markers == 'C2G6NG', 
                              Regime != 'R8', 
                              Regime != 'R9', 
                              Regime != 'R3L'),
               fill = Identity.auto, geom = 'bar', position = 'fill') + 
        scale_fill_manual(values = c('EPI' = 'red', 'PRE' = 'blue', 
                                     'DP' = 'purple', 'DN' = 'gray')) + 
        facet_wrap(~Treatment) + 
        theme_bw() + coord_fixed(5)
print(fig2d)

# Figure S2a

figS2a <- qplot(CH4.logCor, CH5.logCor, 
                data = FGF.all %>% 
                        filter(Treatment == 'Control' | 
                                       Treatment == 'AZD_1' |
                                       Treatment == 'SU_20', 
                               Xpoint != 'xp', 
                               TE_ICM != 'TE', 
                               Markers == 'C2G6NG', 
                               Regime != 'R8', 
                               Regime != 'R9', 
                               Regime != 'R3L'),
                color = Identity.auto, 
                xlim = c(0, 8), ylim = c(0, 8)) + 
        facet_grid(Regime~Treatment) + 
        scale_color_manual(values = c('EPI' = 'red', 'PRE' = 'blue', 
                                      'DP' = 'purple', 'DN' = 'gray')) + 
        theme_bw() + coord_fixed()
print(figS2a)

# Figure S2c

figS2c <- qplot(Regime, data = FGF.all %>% 
                        filter(Treatment == 'Control' | 
                                       Treatment == 'AZD_1' | 
                                       Treatment == 'SU_20', 
                               Xpoint != 'xp', 
                               TE_ICM != 'TE', 
                               Markers == 'C2G6NG', 
                               Regime != 'R8', 
                               Regime != 'R9', 
                               Regime != 'R3L'),
                fill = Identity.auto, geom = 'bar', position = 'fill') + 
        scale_fill_manual(values = c('EPI' = 'red', 'PRE' = 'blue', 
                                     'DP' = 'purple', 'DN' = 'gray')) + 
        facet_wrap(~Treatment) + 
        theme_bw() + coord_fixed(5)
print(figS2c)

# Figure S3a

figS3a <- qplot(Treatment, Cellcount, 
                data = FGF.all %>%
                        group_by(Embryo_ID, 
                                 Regime, 
                                 Treatment, 
                                 Experimenter, 
                                 Xpoint, 
                                 Cellcount, 
                                 Stage) %>%
                        summarise() %>% 
                        filter(Xpoint != 'xp', 
                               Experimenter == 'NS', 
                               Treatment != 'SU_10', 
                               Regime != 'NA', 
                               Regime != 'R8', 
                               Regime != 'R9', 
                               Regime != 'R3L'), 
                geom = c('boxplot', 'jitter'), 
                fill = I('gray')) + 
        theme_bw() + facet_grid(~Regime) + 
        coord_fixed(1/20)
print(figS3a)

# Figure S3b

figS3b <- qplot(Regime, Count, data = FGF.ICMsum %>% 
                        filter(Treatment != 'Littermate', 
                               Treatment != 'SU_10', 
                               Xpoint != 'xp', 
                               Markers == 'C2G6NG', 
                               Regime != 'NA', 
                               Regime != 'R8', 
                               Regime != 'R9', 
                               Regime != 'R3L'), 
                fill = Identity.auto, 
                geom = c('boxplot', 'jitter')) + 
        scale_fill_manual(values = c('EPI' = 'red', 'PRE' = 'blue', 
                                     'DP' = 'purple', 'DN' = 'gray')) + 
        theme_bw() + facet_grid(. ~ Treatment) + coord_fixed(1/10)
print(figS3b)

# Figure S3c

figS3c <- qplot(Regime, pcTotal_avg, 
                data = FGF.sum %>% 
                        group_by(Regime, 
                                 Treatment, 
                                 Experimenter, 
                                 Xpoint, 
                                 TE_ICM) %>% 
                        summarise(pcTotal_avg = mean(pcTotal)) %>% 
                        filter(Xpoint != 'xp', 
                               Treatment != 'Littermate', 
                               Treatment != 'SU_10', 
                               Regime != 'NA', 
                               Regime != 'R8', 
                               Regime != 'R9', 
                               Regime != 'R3L'), 
                geom = 'bar', 
                fill = TE_ICM) + 
        theme_bw() + facet_grid(. ~ Treatment) + 
        scale_fill_manual(values = c('TE' = 'green', 'ICM' = 'purple')) + 
        geom_bar(stat = 'identity') + coord_fixed(1/15)
print(figS3c)
        
# Figure S3d

figS3d <- qplot(Regime, Count, data = FGF.sum %>% 
                        filter(Treatment != 'Littermate', 
                               Treatment != 'SU_10', 
                               Xpoint != 'xp', 
                               Regime != 'NA', 
                               Regime != 'R8', 
                               Regime != 'R9', 
                               Regime != 'R3L'), 
                fill = TE_ICM, geom = c('boxplot', 'jitter')) + 
        scale_fill_manual(values = c('TE' = 'green', 'ICM' = 'purple')) + 
        theme_bw() + facet_grid(. ~ Treatment) + coord_fixed(1/20)
print(figS3d)

# Figure S4

# This code generates a single plot for each panel on Figure S4
# and compiles them onto a .pdf file that is saved to the working directory
# Plots were arranged for Figure S4 manually in Adobe Illustrator

pdf(file = 'ICM_R1-R6.pdf', paper = 'letter')
regimes <- c('R1', 'R5', 'R3', 'R4', 'R6')
regimes <- as.factor(regimes)
treatments <- c('Control', 'FGF4_1000', 'PD03_1')
treatments <- as.factor(treatments)
for (r in regimes){
        for (t in treatments){
                plot <- qplot(Embryo_ID, data = FGF.all %>% 
                                      filter(Treatment == t, 
                                             Xpoint == 'ep', 
                                             Regime == r, 
                                             TE_ICM != 'TE'), 
                              fill = Identity.auto, geom = 'bar', 
                              position = 'fill', 
                              main = r, xlab = t) + 
                        scale_fill_manual(values = c('EPI' = 'red', 
                                                     'PRE' = 'blue', 
                                                     'DP' = 'purple', 
                                                     'DN' = 'gray')) + 
                        theme_bw() + coord_fixed(10)
                print(plot)
        }
}
dev.off()