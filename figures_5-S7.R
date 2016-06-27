# Saiz *et al* (2016)
# Figures 4 and S6 plots

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

# Figure 5b
## Scatter plots showing the levels of GATA6 and NANOG in log scale
## in individual ICM cells of embryos collected at the 8-cell stage and 
## treated with either FGF4 or PD03 for 30h
## Each dot represents a single ICM cell, color coded for identity
fig5b <- ggplot(FGF.all %>% 
                        ## Select ICM cells from embryos 
                        ## cultured for 30h (exchange point, 'xp')
                        filter(TE_ICM != 'TE', 
                               Xpoint == 'xp', 
                               Regime == 'R9'),
                aes(x = CH4.ebLogCor, y = CH5.ebLogCor)) 
## Set up plot aesthetics
fig5b <- fig5b + geom_point(aes(color = Identity.km), size = I(1.5))
fig5b <- fig5b + geom_density2d(color = I('orangered4'), size = 0.5)
fig5b <- fig5b + xlim(-3, 8) + ylim(-3, 8)
fig5b <- fig5b + scale_color_manual(values = idcols)
fig5b <- fig5b + theme_bw() + coord_fixed()
fig5b <- fig5b + labs(color = 'Identity', x = 'log[GATA6]', y = 'log[NANOG]')
## Group by treatment condition
fig5b <- fig5b + facet_grid( ~ Treatment)
## Print plot to the screen
print(fig5b)

# Figure 5d&h
## Scatter plots showing the levels of GATA6 and NANOG in log scale
## in individual ICM cells of embryos collected at the 8-cell stage  
## treated with either FGF4 or PD03 for 30h 
## and then  cultured for a further 18h in regular media
## Each dot represents a single ICM cell, color coded for identity
fig5dh <- ggplot(FGF.all %>% 
                         ## Select ICM cells from embryos 
                         ## cultured for 30h+18h (end point, 'ep')
                         filter(TE_ICM != 'TE', 
                                #Treatment != 'FGF42PD', 
                                Xpoint == 'ep', 
                                Regime == 'R9'),
                 aes(x = CH4.ebLogCor, y = CH5.ebLogCor))
## Set up plot aesthetics
fig5dh <- fig5dh + geom_point(aes(color = Identity.km), size = I(1.5))
fig5dh <- fig5dh + geom_density2d(color = I('orangered4'), size = 0.5)
fig5dh <- fig5dh + xlim(0, 8) + ylim(0, 8)
fig5dh <- fig5dh + scale_color_manual(values = idcols)
fig5dh <- fig5dh + theme_bw() + coord_fixed()
fig5dh <- fig5dh + labs(color = 'Identity', x = 'log[GATA6]', y = 'log[NANOG]')
## Group by treatment condition
fig5dh <- fig5dh + facet_grid( ~ Treatment)
## Print plot to the screen
print(fig5dh)

# Figure 5f
## Bar plot representing the average lineage composition 
## for embryos treated in each condition (FGF4 and PD03)
## grouped by culture length (30h and 30+18h)
fig5f <- ggplot(FGF.all %>% 
                        ## Select ICM cells from embryos
                        ## in Figure 5b-e
                        filter(Regime == 'R9', 
                               TE_ICM != 'TE'),
                aes(x = Treatment, fill = Identity.km))
## Set up plot aesthetics
fig5f <- fig5f + geom_bar(position = 'fill')
fig5f <- fig5f + scale_fill_manual(values = idcols)
fig5f <- fig5f + facet_wrap( ~ Xpoint)
fig5f <- fig5f + labs(fill = 'Identity', x = 'Treatment', y = '% of ICM')
fig5f <- fig5f + theme_bw() 
## Print plot to the screen
print(fig5f)

# ------------------------------------------------------------------------------

# Figure S7b
## Scatter plots showing the levels of GATA6 and NANOG in log scale
## in individual ICM cells of embryos collected at the 8-cell stage and 
## treated with either FGF4 or PD03 for 24h
## Each dot represents a single ICM cell, color coded for identity
figS7b <- ggplot(FGF.all %>% 
                         ## Select ICM cells from embryos 
                         ## cultured for 24h (exchange point, 'xp')
                         filter(TE_ICM != 'TE', 
                                Treatment != 'FGF42PD', 
                                Regime == 'R8', 
                                Xpoint == 'xp'),
                 aes(x = CH4.ebLogCor, y = CH5.ebLogCor))
## Set up plot aesthetics
figS7b <- figS7b + geom_point(aes(color = Identity.km), size = I(1.5))
figS7b <- figS7b + geom_density2d(color = I('orangered4'), size = 0.5)
figS7b <- figS7b + xlim(-3, 8) + ylim(-3, 8)
figS7b <- figS7b + scale_color_manual(values = idcols)
figS7b <- figS7b + theme_bw() + coord_fixed()
figS7b <- figS7b + labs(color = 'Identity', x = 'log[GATA6]', y = 'log[NANOG]')
## Group by treatment condition
figS7b <- figS7b + facet_grid( ~ Treatment)
## Print plot to the screen
print(figS7b)

# Figure S7c
## Boxplots showing the total number of cells per embryo
## for embryos in figures 5 and S7 for each treatment condition indicated
## grouped by treatment regime (rows) and experimental point (columns)
figS7c <- ggplot(FGF.all %>%
                         ## Group embryos to extract cell counts
                         group_by(Embryo_ID,
                                  Regime,
                                  Treatment, 
                                  Experimenter, 
                                  Xpoint, 
                                  Cellcount, 
                                  Stage) %>% 
                         summarise() %>% 
                         ## Select embryos for Regimes 8 and 9
                         ## treated for 24h or 30h in FGF4 or PD03
                         ## and then released for a further 24h or 18h
                         ## in control media or PD03 (after FGF4 only)
                         filter(Regime == 'R8' | 
                                        Regime == 'R9', 
                                Treatment != 'FGF42PD'),
                 aes(x = Treatment, y = Cellcount))
figS7c <- figS7c + geom_boxplot(fill = 'gray', color = 'black', 
                                outlier.shape = 1)
figS7c <- figS7c + geom_jitter(size = I(1.2), color = 'black')
figS7c <- figS7c + labs(y = 'Total cell number', x = 'Treatment')
figS7c <- figS7c + coord_fixed(0.03) + theme_bw()
## Group by treatment regime (rows) and experimental point (columns)
figS7c <- figS7c + facet_grid(Regime ~ Xpoint)
## Print plot to screen
print(figS7c)

# Figure S7d
## Bar plot representing the average lineage composition 
## for embryos treated in each condition (FGF4 and PD03)
## grouped by culture length (24h and 24+24h)
figS7d <- ggplot(FGF.all %>% 
                         ## Select ICM cells from embryos
                         ## in Figure S7a, b
                         filter(Regime == 'R8', 
                                TE_ICM != 'TE'),
                 aes(x = Treatment, fill = Identity.km))
## Set up plot aesthetics
figS7d <- figS7d + geom_bar(position = 'fill')
figS7d <- figS7d + scale_fill_manual(values = idcols)
figS7d <- figS7d + facet_wrap( ~ Xpoint)
figS7d <- figS7d + labs(fill = 'Identity', x = 'Treatment', y = '% of ICM')
figS7d <- figS7d + theme_bw() 
## Print plot to the screen
print(figS7d)

# Figure S7e
## Bar plots representing ICM composition per embryo
## for embryos treated in each conditions in Figure 5
## grouped by treatment condition
## (think a breakdown of Figure 5f, per embryo)
## Each bar represents the ICM of a single embryo
figS7e <- ggplot(FGF.all %>% 
                        ## Select ICM cells from embryos
                        ## in Figure 5
                        filter(Regime == 'R9',
                               Xpoint == 'ep',
                               TE_ICM != 'TE'),
                aes(x = Embryo_ID, fill = Identity.km))
## Set up plot aesthetics
figS7e <- figS7e + geom_bar(position = 'fill')
figS7e <- figS7e + scale_fill_manual(values = idcols)
figS7e <- figS7e + facet_grid( ~ Treatment, scales = 'free')
figS7e <- figS7e + labs(fill = 'Identity', x = 'Treatment', y = '% of ICM')
figS7e <- figS7e + theme_bw() 
## Print plot to the screen
print(figS7e)