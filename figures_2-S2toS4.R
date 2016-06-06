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
                        ## treated with FGF4 and PD03 against Controls
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
fig2b <- fig2b + geom_density2d(color = I('orangered4'), size = 0.25)
## Set up axes limits
fig2b <- fig2b + xlim(0, 8) + ylim(0, 8)
## Set up plot aesthetics
fig2b <- fig2b + scale_color_manual(values = idcols)
fig2b <- fig2b + theme_bw() + coord_fixed()
fig2b <- fig2b + labs(color = 'Identity', x = 'log[GATA6]', y = 'log[NANOG]')
## Arrange by Treatment condition (X axis) and Treatment regime (Y axis)
fig2b <- fig2b + facet_grid(Regime ~ Treatment)
print(fig2b)
        
# Figure 2d

fig2d <- ggplot(FGF.all %>% 
                        ## Filter ICM cells of end point embryos 
                        ## for Regimes 1, 5, 3, 4 and 6;
                        ## treated with FGF4 and PD03 against Controls
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

figS2a <- ggplot(FGF.all %>% 
                         ## Filter ICM cells of end point embryos 
                         ## for Regimes 1, 5, 3, 4 and 6;
                         ## treated with FGFR inhibitors, against Controls
                         filter(Treatment == 'Control' | 
                                        Treatment == 'AZD_1' |
                                        Treatment == 'SU_20', 
                                Xpoint != 'xp', 
                                TE_ICM != 'TE', 
                                Markers == 'C2G6NG', 
                                Regime != 'R8', 
                                Regime != 'R9', 
                                Regime != 'R3L'),
                 ## Plot log(GATA6) against log(NANOG)
                 aes(x = CH4.ebLogCor, y = CH5.ebLogCor))
figS2a <- figS2a + geom_point(aes(color = Identity.auto), size = I(0.75))
figS2a <- figS2a + geom_density2d(color = I('orangered4'), size = 0.25)
## Set up axes limits
figS2a <- figS2a + xlim(0, 8) + ylim(0, 8)
## Color-code for identity
figS2a <- figS2a + scale_color_manual(values = idcols)
## Set up aesthetics
figS2a <- figS2a + theme_bw() + coord_fixed()
figS2a <- figS2a + labs(color = 'Identity', x = 'log[GATA6]', y = 'log[NANOG]')
## Arrange by Treatment condition (X axis) and Treatment regime (Y axis)
figS2a <- figS2a + facet_grid(Regime ~ Treatment)
print(figS2a)

# Figure S2c

figS2c <- ggplot(FGF.all %>% 
                        ## Filter ICM cells of end point embryos 
                        ## for Regimes 1, 5, 3, 4 and 6;
                        ## treated with FGFR inhibitors, against Controls
                        filter(Treatment == 'Control' | 
                                       Treatment == 'AZD_1' | 
                                       Treatment == 'SU_20', 
                               Xpoint != 'xp', 
                               TE_ICM != 'TE', 
                               Markers == 'C2G6NG', 
                               Regime != 'R8', 
                               Regime != 'R9', 
                               Regime != 'R3L'),
                ## Plot each treatment regime against ICM composition
                aes(x = Regime, fill = Identity.auto))
figS2c <- figS2c + geom_bar(position = 'fill')
## Set up plot aesthetics
figS2c <- figS2c + scale_fill_manual(values = idcols)
figS2c <- figS2c + theme_bw() + coord_fixed(5)
figS2c <- figS2c + labs(fill = 'Identity', x = 'Treatment regime', y = '% of ICM')
## Group by treatment condition
figS2c <- figS2c + facet_wrap( ~ Treatment)
print(figS2c)

# Figure S3a

figS3a <- ggplot(FGF.all %>%
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
                 aes(x = Treatment, y = Cellcount))
figS3a <- figS3a + geom_boxplot(fill = 'gray', color = 'black', 
                                outlier.shape = 1)
figS3a <- figS3a + geom_jitter(size = I(1.2), color = 'black')
figS3a <- figS3a + theme_bw() + facet_grid( ~ Regime)
figS3a <- figS3a + theme(axis.text.x = element_text(angle = 45, hjust = 1))
figS3a <- figS3a + labs(y = 'Total cell number', x = 'Treatment')
figS3a <- figS3a + coord_fixed(1/20)
print(figS3a)

# Figure S3b

figS3b <- ggplot(FGF.ICMsum %>% 
                         filter(Treatment != 'Littermate', 
                                Treatment != 'SU_10', 
                                Xpoint != 'xp', 
                                Markers == 'C2G6NG', 
                                Regime != 'NA', 
                                Regime != 'R8', 
                                Regime != 'R9', 
                                Regime != 'R3L'),
                 aes(x = Regime, y = Count))
figS3b <- figS3b + geom_boxplot(aes(fill = Identity.auto), color = I('black'), 
                                outlier.shape = 1)
figS3b <- figS3b + scale_fill_manual(values = idcols)
figS3b <- figS3b + theme_bw() + facet_grid( ~ Treatment)
figS3b <- figS3b + coord_fixed(1/10)
figS3b <- figS3b + labs(x = 'Regime', y = 'Cell number', fill = 'Identity')
print(figS3b)

# Figure S3c

figS3c <- ggplot(FGF.all %>% 
                         filter(Xpoint != 'xp', 
                                Treatment != 'Littermate', 
                                Treatment != 'SU_10', 
                                Regime != 'NA', 
                                Regime != 'R8', 
                                Regime != 'R9', 
                                Regime != 'R3L'), 
                 aes(x = Regime, fill = Identity.auto))
figS3c <- figS3c + geom_bar(position = 'fill')
figS3c <- figS3c + scale_fill_manual(values = idcols)
figS3c <- figS3c + theme_bw() + facet_grid( ~ Treatment)
figS3c <- figS3c + coord_fixed(5)
figS3c <- figS3c + labs(x = 'Regime', y = 'Cell number', fill = 'Identity')
print(figS3c)
        
# Figure S3d

figS3d <- ggplot(FGF.sum %>% 
                         filter(Treatment != 'Littermate', 
                                Treatment != 'SU_10', 
                                Xpoint != 'xp', 
                                Regime != 'NA', 
                                Regime != 'R8', 
                                Regime != 'R9', 
                                Regime != 'R3L'), 
                 aes(x = Regime, y = Count))
figS3d <- figS3d + geom_boxplot(aes(fill = TE_ICM), color = I('black'), 
                                outlier.shape = 1)
figS3d <- figS3d + geom_jitter(aes(shape = TE_ICM), color = 'black', 
                               size = I(1.2))
figS3d <- figS3d + scale_fill_manual(values = idcols)
figS3d <- figS3d + theme_bw() + facet_grid( ~ Treatment) + coord_fixed(1/20)
figS3d <- figS3d + labs(fill = 'Identity', x = 'Regime', y = 'Cell number')
print(figS3d)

# Figure S4

# figS4 <- ggplot(FGF.all %>% 
#                         ## Filter ICM cells of end point embryos 
#                         ## for Regimes 1, 5, 3, 4 and 6;
#                         ## treated with FGF4 and PD03 against Controls
#                         filter(Treatment == 'Control' | 
#                                        Treatment == 'FGF4_1000' | 
#                                        Treatment == 'PD03_1', 
#                                Xpoint != 'xp', 
#                                TE_ICM != 'TE', 
#                                Markers == 'C2G6NG', 
#                                Regime != 'R8', 
#                                Regime != 'R9', 
#                                Regime != 'R3L'),
#                 ## Plot each treatment regime against ICM composition
#                 aes(x = Embryo_ID, fill = Identity.auto))
# figS4 <- figS4 + geom_bar(position = 'fill')
# ## Set up plot aesthetics
# figS4 <- figS4 + scale_fill_manual(values = idcols)
# figS4 <- figS4 + theme_bw() + coord_fixed(5)
# figS4 <- figS4 + labs(fill = 'Identity', x = 'Treatment regime', y = '% of ICM')
# ## Group by treatment condition
# figS4 <- figS4 + facet_grid(Treatment ~ Regime, scales = 'free')
# print(figS4)


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