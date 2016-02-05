# Saiz *et al* (2016)
# Figures 2, S2, S3 and S4 plots

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

# Figure 2b

fig2b <- ggplot(FGF.all %>% 
                        ## Filter ICM cells of end point embryos 
                        ## for Regimes 1, 5, 3, 4 and 6;
                        ## treated with FGF4 and PD03 against Control
                        filter(Treatment == 'Control' | 
                                       Treatment == 'FGF4_1000' | 
                                       Treatment == 'PD03_1',
                               Xpoint != 'xp', 
                               TE_ICM != 'TE', 
                               Markers == 'C2G6NG', 
                               Regime != 'R8', 
                               Regime != 'R9', 
                               Regime != 'R3L'),
                ## Plot log(GATA6) against log(NANOG)
                aes(x = CH4.ebLogCor, y = CH5.ebLogCor))
fig2b <- fig2b + geom_point(aes(color = Identity.auto), size = I(0.75))
fig2b <- fig2b + geom_density2d(color = I('orangered4'), size = 0.25, bins = 7)
## Set up axes limits
fig2b <- fig2b + xlim(0, 8) + ylim(0, 8)
## Set up plot aesthetics
fig2b <- fig2b + scale_color_manual(values = idcols)
fig2b <- fig2b + theme_bw() + coord_fixed()
fig2b <- fig2b + labs(color = 'Identity', x = 'log(GATA6)', y = 'log(NANOG)')
## Arrange by Treatment condition (X axis) and Treatment regime (Y axis)
fig2b <- fig2b + facet_grid(Regime ~ Treatment)
print(fig2b)
        
# Figure 2d

fig2d <- ggplot(FGF.all %>% 
                        ## Filter ICM cells of end point embryos 
                        ## for Regimes 1, 5, 3, 4 and 6;
                        ## treated with FGF4 and PD03 against Control
                        filter(Treatment == 'Control' | 
                                       Treatment == 'FGF4_1000' | 
                                       Treatment == 'PD03_1', 
                               Xpoint != 'xp', 
                               TE_ICM != 'TE', 
                               Markers == 'C2G6NG', 
                               Regime != 'R8', 
                               Regime != 'R9', 
                               Regime != 'R3L'),
                ## Plot each treatment regime against ICM composition
                aes(x = Regime, fill = Identity.auto))
fig2d <- fig2d + geom_bar(position = 'fill')
## Set up plot aesthetics
fig2d <- fig2d + scale_fill_manual(values = idcols)
fig2d <- fig2d + theme_bw() + coord_fixed(5)
fig2d <- fig2d + labs(fill = 'Identity', x = 'Treatment regime', y = '% of ICM')
## Group by treatment condition
fig2d <- fig2d + facet_wrap( ~ Treatment)
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
                color = Identity.auto, size = I(0.75),
                xlim = c(0, 8), ylim = c(0, 8)) + 
        facet_grid(Regime~Treatment) + 
        scale_color_manual(values = idcols) + 
        theme_bw() + coord_fixed()
figS2a <- figS2a + geom_density2d(color = I('orangered4'), size = 0.25, bins = 7)
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
        scale_fill_manual(values = idcols) + 
        facet_wrap( ~ Treatment) + 
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
                fill = Identity.auto, color = I('black'),
                geom = c('boxplot')) + 
        scale_fill_manual(values = idcols) + 
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
        scale_fill_manual(values = idcols) + 
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
        scale_fill_manual(values = idcols) + 
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
                        scale_fill_manual(values = idcols) + 
                        theme_bw() + coord_fixed(10)
                print(plot)
        }
}
dev.off()