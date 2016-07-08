# Saiz *et al* (2016)
# Figures 2, S3 and S4 plots

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

# Create vector for identity colors
idcols <- c('EPI' = 'red', 'PRE' = 'blue', 'DP' = 'purple', 'DN' = 'gray', 
            'TE' = 'green', 'ICM' = 'purple')

# Figure 2b
## Scatter plots showing the levels of GATA6 and NANOG in log scale
## in individual ICM cells of embryos treated with either FGF4, PD03 or AZD4547
## for the indicated regimes (stage of collection + treatment length)
## Each dot represents a single ICM cell, color coded for identity
fig2b <- ggplot(FGF.all %>% 
                        ## Filter ICM cells of end point embryos 
                        ## stained with CDX2, GATA6 and NANOG
                        ## for Regimes 1, 5, 3, 4 and 6;
                        ## treated with FGF4, PD03 or AZD4547, against Controls
                        filter(Treatment == 'Control' | 
                                       Treatment == 'FGF4_1000' | 
                                       Treatment == 'PD03_1' | 
                                       Treatment == 'AZD_1',
                               Xpoint != 'xp', 
                               TE_ICM != 'TE', 
                               Markers == 'C2G6NG', 
                               Regime != 'R8', 
                               Regime != 'R9'),
                ## Plot log(GATA6) against log(NANOG)
                aes(x = CH4.ebLogCor, y = CH5.ebLogCor))
fig2b <- fig2b + geom_point(aes(color = Identity.km), size = I(0.75))
fig2b <- fig2b + geom_density2d(color = I('orangered4'), size = 0.25)
## Set up axes limits
fig2b <- fig2b + xlim(0, 8) + ylim(0, 8)
## Set up plot aesthetics
fig2b <- fig2b + scale_color_manual(values = idcols)
fig2b <- fig2b + theme_bw() + coord_fixed()
fig2b <- fig2b + labs(color = 'Identity', x = 'log[GATA6]', y = 'log[NANOG]')
## Arrange by Treatment condition (X axis) and Treatment regime (Y axis)
fig2b <- fig2b + facet_grid(Regime ~ Treatment)
## Print plot to the screen
print(fig2b)
        
# Figure 2d
## Bar plot representing the average lineage composition 
## for each regime in Fig. 2b & c,
## grouped by treatment condition
fig2d <- ggplot(FGF.all %>% 
                        ## Filter ICM cells of end point embryos 
                        ## stained with CDX2, GATA6 and NANOG
                        ## for Regimes 1, 5, 3, 4 and 6;
                        ## treated with FGF4, PD03 or AZD4547, against Controls
                        filter(Treatment == 'Control' | 
                                       Treatment == 'FGF4_1000' | 
                                       Treatment == 'PD03_1' | 
                                       Treatment == 'AZD_1', 
                               Xpoint != 'xp', 
                               TE_ICM != 'TE', 
                               Markers == 'C2G6NG', 
                               Regime != 'R8', 
                               Regime != 'R9'),
                ## Plot each treatment regime against average ICM composition
                aes(x = Regime, fill = Identity.km))
fig2d <- fig2d + geom_bar(position = 'fill')
## Set up plot aesthetics
fig2d <- fig2d + scale_fill_manual(values = idcols)
fig2d <- fig2d + theme_bw() + coord_fixed(5)
fig2d <- fig2d + labs(fill = 'Identity', x = 'Treatment regime', y = '% of ICM')
## Group by treatment condition
fig2d <- fig2d + facet_grid( ~ Treatment)
## Print plot to the screen
print(fig2d)

# ------------------------------------------------------------------------------

# Figure S3b
## Scatter plots showing the levels of GATA6 and NANOG in log scale
## in individual ICM cells of embryos treated with SU5402 vs Controls
## for the indicated regimes (stage of collection + treatment length)
## Each dot represents a single ICM cell, color coded for identity
figS3b <- ggplot(FGF.all %>% 
                         ## Filter ICM cells of end point embryos 
                         ## stained with CDX2, GATA6 and NANOG
                         ## for Regimes 1, 3, and 4;
                         ## treated with SU5402, against Controls
                         filter(Treatment == 'Control' | 
                                        Treatment == 'SU_20', 
                                Xpoint != 'xp', 
                                TE_ICM != 'TE', 
                                Markers == 'C2G6NG', 
                                Regime == 'R1' | 
                                        Regime == 'R3' | 
                                        Regime == 'R4'),
                 ## Plot log(GATA6) against log(NANOG)
                 aes(x = CH4.ebLogCor, y = CH5.ebLogCor))
figS3b <- figS3b + geom_point(aes(color = Identity.km), size = I(0.75))
figS3b <- figS3b + geom_density2d(color = I('orangered4'), size = 0.25)
## Set up axes limits
figS3b <- figS3b + xlim(0, 8) + ylim(0, 8)
## Color-code for identity
figS3b <- figS3b + scale_color_manual(values = idcols)
## Set up aesthetics
figS3b <- figS3b + theme_bw() + coord_fixed()
figS3b <- figS3b + labs(color = 'Identity', x = 'log[GATA6]', y = 'log[NANOG]')
## Arrange by Treatment condition (X axis) and Treatment regime (Y axis)
figS3b <- figS3b + facet_grid(Treatment ~ Regime)
## Print plot to the screen
print(figS3b)

# Figure S3c
## Bar plot representing the average lineage composition 
## for embryos treated with SU5402 vs Controls
## grouped by treatment condition (SU5402 and Control)
figS3c <- ggplot(FGF.all %>% 
                         ## Filter ICM cells of end point embryos 
                         ## stained with CDX2, GATA6 and NANOG
                         ## for Regimes 1, 3, and 4;
                         ## treated with SU5402, against Controls
                         filter(Treatment == 'Control' | 
                                        Treatment == 'SU_20', 
                                Xpoint != 'xp', 
                                TE_ICM != 'TE', 
                                Markers == 'C2G6NG', 
                                Regime == 'R1' | 
                                        Regime == 'R3' | 
                                        Regime == 'R4'),
                ## Plot each treatment regime against ICM composition
                aes(x = Regime, fill = Identity.km))
figS3c <- figS3c + geom_bar(position = 'fill')
## Set up plot aesthetics
figS3c <- figS3c + scale_fill_manual(values = idcols)
figS3c <- figS3c + theme_bw() + coord_fixed(3)
figS3c <- figS3c + labs(fill = 'Identity', x = 'Treatment regime', y = '% of ICM')
## Group by treatment condition (Control vs SU5402)
figS3c <- figS3c + facet_wrap( ~ Treatment)
## Print plot to the screen
print(figS3c)

# Figure S3d
## Grid of bar plots representing ICM composition per embryo
## for embryos treated in all conditions - FGF4, PD03, AZD4547 and SU5402 - 
## arranged by treatment condition (rows) and treatment regime (column)
## (think a breakdown of Figure 2d + S2c combined)
## Each bar represents the ICM of a single embryo
figS3d <- ggplot(FGF.all %>% 
                         ## Filter ICM cells of end point embryos 
                         ## stained with CDX2, GATA6 and NANOG
                         ## for Regimes 1, 5, 3, 4 and 6;
                         ## treated with FGF4, PD03, AZD4547 or SU5402
                         filter(Treatment != 'Littermate',
                                Treatment != 'SU_10',
                                Xpoint != 'xp', 
                                TE_ICM != 'TE', 
                                Markers == 'C2G6NG', 
                                Regime != 'R8', 
                                Regime != 'R9'),
                 ## Plot each treatment regime against average ICM composition
                 aes(x = Embryo_ID, fill = Identity.km))
figS3d <- figS3d + geom_bar(position = 'fill')
## Set up plot aesthetics
figS3d <- figS3d + scale_fill_manual(values = idcols)
figS3d <- figS3d + theme_bw() + coord_fixed(100)
figS3d <- figS3d + labs(fill = 'Identity', x = 'Treatment regime', y = '% of ICM')
## Arange by treatment condition and treatment regime
figS3d <- figS3d + facet_wrap( ~ Treatment + Regime, scales = 'free')
## Print plot to the screen
print(figS3d)

# Figure S4a
## Boxplots showing the total number of cells per embryo
## for embryos in figures 2 and S3
## for each treatment condition, grouped by treatment regime
figS4a <- ggplot(FGF.all %>%
                         ## Calculate total cell number per embryo
                         group_by(Embryo_ID, 
                                  Regime, 
                                  Treatment, 
                                  Experimenter, 
                                  Xpoint, 
                                  Cellcount, 
                                  Stage) %>%
                         summarise() %>% 
                         ## Select all embryos cultured for 48h
                         ## (and the corresponding littermates)
                         ## for each treatment and regime to be plotted
                         filter(Xpoint != 'xp', 
                                Experimenter == 'NS', 
                                Treatment != 'SU_10', 
                                Regime != 'NA', 
                                Regime != 'R8', 
                                Regime != 'R9'),
                 aes(x = Treatment, y = Cellcount))
## Set up plot aesthetics
figS4a <- figS4a + geom_boxplot(fill = 'gray', color = 'black', 
                                outlier.shape = 1)
figS4a <- figS4a + geom_jitter(size = I(1.2), color = 'black')
figS4a <- figS4a + theme_bw() + facet_grid( ~ Regime)
figS4a <- figS4a + theme(axis.text.x = element_text(angle = 45, hjust = 1))
figS4a <- figS4a + labs(y = 'Total cell number', x = 'Treatment')
figS4a <- figS4a + coord_fixed(1/20)
## Print plot to the screen
print(figS4a)

# Figure S4b
## Boxplots showing the number of cells in each ICM lineage denomination
## for each treatment regime in figures 2 and S3, grouped by treatment condition
figS4b <- ggplot(FGF.ICMsum %>% 
                         ## Select all embryos cultured for 48h
                         ## stained for CDX2, GATA6 and NANOG
                         ## for each treatment and regime to be plotted
                         filter(Treatment != 'Littermate', 
                                Treatment != 'SU_10', 
                                Xpoint != 'xp', 
                                Markers == 'C2G6NG', 
                                Regime != 'NA', 
                                Regime != 'R8', 
                                Regime != 'R9'),
                 aes(x = Regime, y = Count))
## Color code for identity
figS4b <- figS4b + geom_boxplot(aes(fill = Identity.km), color = I('black'), 
                                outlier.shape = 1)
## Set up plot aesthetics
figS4b <- figS4b + scale_fill_manual(values = idcols)
figS4b <- figS4b + theme_bw() + facet_grid( ~ Treatment)
figS4b <- figS4b + coord_fixed(1/10)
figS4b <- figS4b + labs(x = 'Regime', y = 'Cell number', fill = 'Identity')
## Print plot to the screen
print(figS4b)

# Figure S4c
## Bar plots showing the average lineage composition for each treatment regime
## in figures 2 and S3, grouped by treatment condition
figS4c <- ggplot(FGF.all %>% 
                         ## Select embryos cultured for 48h
                         ## for each treatment and regime to be plotted
                         filter(Xpoint != 'xp', 
                                Treatment != 'Littermate', 
                                Treatment != 'SU_10', 
                                Markers == 'C2G6NG', 
                                Regime != 'NA', 
                                Regime != 'R8', 
                                Regime != 'R9'), 
                 aes(x = Regime, fill = Identity.km))
figS4c <- figS4c + geom_bar(position = 'fill')
## Set up plot aesthetics
figS4c <- figS4c + scale_fill_manual(values = idcols)
figS4c <- figS4c + theme_bw() + facet_grid( ~ Treatment)
figS4c <- figS4c + coord_fixed(5)
figS4c <- figS4c + labs(x = 'Regime', y = '% of total', fill = 'Identity')
## Print plot to the screen
print(figS4c)
        
# Figure S4d
## Boxplots showing number of cells in TE and ICM for each treatment regime
## in figures 2 and S3, grouped by treatment condition
figS4d <- ggplot(FGF.sum %>% 
                         ## Select embryos cultured for 48h
                         ## for each treatment and regime to be plotted
                         filter(Treatment != 'Littermate', 
                                Treatment != 'SU_10', 
                                Xpoint != 'xp', 
                                Regime != 'NA', 
                                Regime != 'R8', 
                                Regime != 'R9'), 
                 aes(x = Regime, y = Count))
## Set up plot aesthetics
figS4d <- figS4d + geom_boxplot(aes(fill = TE_ICM), color = I('black'), 
                                outlier.shape = 1)
figS4d <- figS4d + geom_jitter(aes(shape = TE_ICM), color = 'black', 
                               size = I(1.2))
figS4d <- figS4d + scale_fill_manual(values = idcols)
figS4d <- figS4d + theme_bw() + facet_grid( ~ Treatment) + coord_fixed(1/20)
figS4d <- figS4d + labs(fill = 'Identity', x = 'Regime', y = 'Cell number')
## Print plot to the screen
print(figS4d)