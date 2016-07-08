# Saiz *et al* (2016)
# Figure 3 plots

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

# Figure 4a
## This plot is also the first panel in Figure 2d
fig4a <- ggplot(FGF.all %>% 
                        ## Filter ICM cells of Control, end point embryos 
                        ## stained with CDX2, GATA6 and NANOG
                        ## for Regimes 1, 5, 3, 4 and 6;
                        filter(Treatment == 'Control', 
                               Xpoint != 'xp', 
                               TE_ICM != 'TE', 
                               Markers == 'C2G6NG', 
                               Regime != 'R8', 
                               Regime != 'R9'),
                ## Plot each treatment regime against average ICM composition
                aes(x = Regime, fill = Identity.km))
fig4a <- fig4a + geom_bar(position = 'fill')
## Set up plot aesthetics
fig4a <- fig4a + scale_fill_manual(values = idcols)
fig4a <- fig4a + theme_bw() + coord_fixed(6)
fig4a <- fig4a + labs(fill = 'Identity', x = 'Treatment regime', y = '% of ICM')
## Group by treatment condition
fig4a <- fig4a + facet_grid( ~ Treatment)
## Print plot to the screen
print(fig4a)

# Figure 4b
## This plot is also the first panel in Figure S4d
fig4b <- ggplot(FGF.sum %>% 
                        ## Select Control embryos cultured for 48h
                        ## for each regime to be plotted
                        filter(Treatment == 'Control', 
                               Xpoint != 'xp', 
                               Regime != 'NA', 
                               Regime != 'R8', 
                               Regime != 'R9'), 
                aes(x = Regime, y = Count))
## Set up plot aesthetics
fig4b <- fig4b + geom_boxplot(aes(fill = TE_ICM), color = I('black'), 
                                outlier.shape = 1)
fig4b <- fig4b + geom_jitter(aes(shape = TE_ICM), color = 'black', 
                               size = I(1.2))
fig4b <- fig4b + scale_fill_manual(values = idcols)
fig4b <- fig4b + theme_bw() + facet_grid( ~ Treatment) + coord_fixed(1/17)
fig4b <- fig4b + labs(fill = 'Identity', x = 'Regime', y = 'Cell number')
## Print plot to the screen
print(fig4b)

# Figure 4c
## This plot is also the first panel in Figure S4c
fig4c <- ggplot(FGF.all %>% 
                        ## Select Control embryos cultured for 48h
                        ## for each regime to be plotted
                        filter(Xpoint != 'xp', 
                               Treatment == 'Control', 
                               Markers == 'C2G6NG', 
                               Regime != 'NA', 
                               Regime != 'R8', 
                               Regime != 'R9'), 
                aes(x = Regime, fill = Identity.km))
## Set up plot aesthetics
fig4c <- fig4c + geom_bar(position = 'fill')
fig4c <- fig4c + scale_fill_manual(values = idcols)
fig4c <- fig4c + theme_bw() + facet_grid( ~ Treatment)
fig4c <- fig4c + coord_fixed(6)
fig4c <- fig4c + labs(x = 'Regime', y = '% of total', fill = 'Identity')
## Print plot to the screen
print(fig4c)

## Check whether scaling experiments data has been loaded
## and if not, run the script to load it
scaling.ok <- exists('scaling')
if (scaling.ok == FALSE) {
        source('Transformations.R')
}

# Figure 4f
## Box plots showing the total number of cells per embryo
## for embryos in Figure 3e
## for each manipulation ('Treatment') (Control, Single, Half or Double)
fig4f <- ggplot(scal.counts, 
                aes(x = Treatment, y = Cellcount))
fig4f <- fig4f + geom_boxplot(fill = I('grey'), color = I('black'), 
                              outlier.shape = 1)
fig4f <- fig4f + geom_jitter(fill = I('black'))
## Set up plot aesthetics
fig4f <- fig4f + ylim(0, 250)
fig4f <- fig4f + labs(x = 'Treatment', y = 'Total cell number')
fig4f <- fig4f + theme_bw() + coord_fixed(1/35)
## Print plot to the screen
print(fig4f)

# Figure 4g
## Box plots showing the number of cells in TE and ICM per embryo
## for embryos in Figure 3e, f
## for each manipulation ('Treatment') (Control, Single, Half or Double)
fig4g <- ggplot(scal.teicm, 
             aes(x = Treatment, y = Count))
fig4g <- fig4g + geom_boxplot(aes(fill = TE_ICM), color = I('black'), 
                        outlier.shape = 1)
fig4g <- fig4g + geom_jitter(aes(shape = TE_ICM), fill = I('black'))
## Set up plot aesthetics
fig4g <- fig4g + scale_fill_manual(values = idcols)
fig4g <- fig4g + labs(fill = 'Identity', x = 'Treatment', y = 'Cell number')
fig4g <- fig4g + theme_bw() + ylim(0, 200) + coord_fixed(1/30)
## Print plot to the screen
print(fig4g)

# Figure 4h
## Bar plots showing the average lineage composition 
## for embryos in Figure 3e, f
## for each manipulation ('Treatment') (Control, Single, Half or Double)
fig4h <- ggplot(scaling, 
                aes(x = Treatment, fill = Identity.km))
fig4h <- fig4h + geom_bar(position = 'fill')
## Set up plot aesthetics
fig4h <- fig4h + scale_fill_manual(values = idcols)
fig4h <- fig4h + labs(fill = 'Identity', x = 'Treatment', y = '% of total')
fig4h <- fig4h + theme_bw() + coord_fixed(7)
## Print plot to the screen
print(fig4h)

# Figure 4i
## Bar plots showing the average ICM composition
## for embryos in Figure 3e, f
## for each manipulation ('Treatment') (Control, Single, Half or Double)
fig4i <- ggplot(scaling %>% 
                        ## Select ICM cells only
                        filter(Identity != 'TE'), 
                aes(x = Treatment, fill = Identity.km))
fig4i <- fig4i + geom_bar(position = 'fill')
## Set up plot aesthetics
fig4i <- fig4i + scale_fill_manual(values = idcols)
fig4i <- fig4i + labs(fill = 'Identity', x = 'Treatment', y = '% of ICM')
fig4i <- fig4i + theme_bw() + coord_fixed(7)
## Print plot to the screen
print(fig4i)

# ------------------------------------------------------------------------------

# Figure S6a
## Bar plots showing the ICM composition per embryo
## for embryos in Figure 3e, f
## grouped by manipulation ('Treatment') (Control, Single, Half or Double)
figS6a <- ggplot(scaling %>% 
                        ## Select ICM cells only
                        filter(Identity != 'TE'), 
              aes(x = Embryo_ID, fill = Identity.km))
figS6a <- figS6a + geom_bar(position = 'fill')
## Set up plot aesthetics
figS6a <- figS6a + scale_fill_manual(values = idcols)
figS6a <- figS6a + facet_wrap( ~ Treatment, scales = 'free')
figS6a <- figS6a + labs(fill = 'Identity', y = '% of ICM')
figS6a <- figS6a + theme_bw() + theme(axis.text.x = element_text(angle = 45, 
                                                               hjust = 1))
## Print plot to the screen
print(figS6a)

# Figure S6b 
## Local regression curves showing the size of each ICM lineage denomination
## (as % of the ICM) for control embryos shown in Fig. 2d and S3d.
## Embryos are arranged by total cell number

## Define grid for plotting (Pre, DN, EPI+DN)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 2), mgp = c(2, 0.7, 0), lwd = 1.5, pty = 's')
## PrE as % of ICM
plot(Count/ICM.count ~ Cellcount, data = subset(FGF.ICMsum, 
                                                Treatment == "Control" & 
                                                        Xpoint == "ep" & 
                                                        Identity.km == 'PRE'), 
     pch = 16, cex = 0.8, xlim = c(65, 210), ylim = c(0, 1), 
     ylab = 'PrE/ICM', xlab = 'Total cell number')
oo1 <- locfit(Count ~ Cellcount, weights = ICM.count, family = "binomial", 
              data = subset(FGF.ICMsum, Treatment == "Control" & 
                                    Xpoint == "ep" & 
                                    Identity.km == 'PRE'))
plot(oo1, band = "local", add = TRUE, col = idcols[2], lwd = 2)
## DP as % of ICM
plot(Count/ICM.count ~ Cellcount, data = subset(FGF.ICMsum, 
                                               Treatment == "Control" & 
                                                       Xpoint == "ep" & 
                                                       Identity.km == 'DP'), 
     pch = 16, cex = 0.8, xlim = c(65, 210), ylim = c(0, 1), 
     main = 'Control', ylab = 'DP/ICM', xlab = 'Total cell number')
oo1 <- locfit(Count ~ Cellcount, weights = ICM.count, family = "binomial", 
              data = subset(FGF.ICMsum, Treatment == "Control" 
                            & Xpoint == "ep" & 
                                    Identity.km == 'DP'))
plot(oo1, band = "local", add = TRUE, col = idcols[3], lwd = 2)
## EPI+DN as % of ICM
plot(Count/ICM.count ~ Cellcount, data = subset(FGF.ICMsum, 
                                                Treatment == "Control" & 
                                                        Xpoint == "ep" &
                                                        (Identity.km == 'EPI' | 
                                                                 Identity.km == 'DN')), 
     pch = 16, cex = 0.8, xlim = c(65, 210), ylim = c(0, 1), 
     ylab = 'EPI+DN/ICM', xlab = 'Total cell number')
oo1 <- locfit(Count ~ Cellcount, weights = ICM.count, family = "binomial", 
              data = subset(FGF.ICMsum, Treatment == "Control" & 
                                    Xpoint == "ep" & 
                                    (Identity.km == 'EPI' | 
                                             Identity.km == 'DN')))
plot(oo1, band = "local", add = TRUE, col = idcols[1], lwd = 2)

# Figure S6c 
## Local regression curves showing the size of each ICM lineage denomination
## (as % of the ICM) for control (black), single (blue), half (green) and 
## double (red) embryos.
## Embryos are arranged by total cell number

## PrE as % of ICM
plot(Count/ICM.count ~ Cellcount, data = subset(scal.ICMsum, Identity.km == 'PRE'), 
     pch = 16, cex = 0.8, xlim = c(20, 240), ylim=c(0, 1), 
     col = c('black', 'blue', 'green', 'red')[Treatment], 
     ylab = 'PrE/ICM', xlab = 'Total cell number')
oo1 <- locfit(Count ~ Cellcount, weights = ICM.count, family = "binomial", 
              data = subset(scal.ICMsum, Identity.km == 'PRE'))
plot(oo1, band = "local", add = TRUE, col = idcols[2], lwd = 2)
## DP as % of ICM
plot(Count/ICM.count ~ Cellcount, data = subset(scal.ICMsum, Identity.km == 'DP'), 
     pch = 16, cex = 0.8, xlim = c(20, 240), ylim = c(0, 1), 
     col = c('black', 'blue', 'green', 'red')[Treatment], 
     main = "Scaling", ylab = 'DP/ICM', xlab = 'Total cell number')
oo1 <- locfit(Count ~ Cellcount, weights = ICM.count, family = "binomial", 
              data = subset(scal.ICMsum, Identity.km == 'DP'))
plot(oo1, band = "local", add = TRUE, col = idcols[3], lwd = 2)
## EPI+DN as % of ICM
plot(Count/ICM.count ~ Cellcount, data = subset(scal.ICMsum, 
                                                (Identity.km == 'EPI' | 
                                                         Identity.km == 'DN')), 
     pch = 16, cex = 0.8, xlim = c(20, 240), ylim = c(0, 1), 
     col = c('black', 'blue', 'green', 'red')[Treatment], 
     ylab = 'EPI+DN/ICM', xlab = 'Total cell number')
oo1 <- locfit(Count ~ Cellcount, weights = ICM.count, family = "binomial", 
              data = subset(scal.ICMsum, (Identity.km == 'EPI' | 
                                                  Identity.km == 'DN')))
plot(oo1, band = "local", add = TRUE, col = idcols[1], lwd = 2)

## Reset grid to 1x1
par(mfrow = c(1, 1)) 