# Saiz *et al* (2016)
# Figures 1, S1 and S2 plots

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

# Figure 1c
## Scatter plots showing the levels of GATA6 and NANOG in linear scale
## in individual ICM ccells of embryos collected at sequential stages
## of development as in Fig. 1b ('Littermates', blastocysts of >= 32 cells)
## Each dot represents a single ICM cell
# FGF.all$ratio <- FGF.all$CH4.ebLogCor - FGF.all$CH5.ebLogCor
fig1c <- ggplot(FGF.all %>% 
                        ## Select ICM cells of Littermates with 32 or more cells
                        ## and exclude embryos from Aug 20, 2015 experiment
                        filter(Treatment == 'Littermate',
                               Exp_date != '20150820', 
                               TE_ICM != 'TE', 
                               Stage != '<32'),
                ## Plot corrected fluorescence levels 
                ## of GATA6 (CH4) vs NANOG (CH5) on linear scale
                aes(x = exp(CH4.ebLogCor), y = exp(CH5.ebLogCor)))
## Color-code points for identity assigned by thresholding linear scale data
fig1c <- fig1c + geom_point(aes(color = Identity.lin), size = I(1.2))
fig1c <- fig1c + scale_color_manual(values = idcols)
## Draw thresholds (50 a.u. on both axes)
fig1c <- fig1c + geom_vline(xintercept = 50, linetype = 2)
fig1c <- fig1c + geom_hline(yintercept = 50, linetype = 2)
## Set limits for X and Y axes
fig1c <- fig1c + xlim(0, 255) + ylim(0, 255)
## Set aesthetis for the plot
fig1c <- fig1c + theme_bw()
fig1c <- fig1c + labs(color = 'Identity', x = '[GATA6]', y = '[NANOG]')
fig1c <- fig1c + coord_fixed()
## Bin cells by stage
fig1c <- fig1c + facet_grid( ~ Stage)
## Print the plot to the screen
print(fig1c)

# Figure 1d
## Scatter plots showing the levels of GATA6 and NANOG in log scale
## in individual ICM cells of embryos collected at sequential stages 
## of development as in Fig. 1b ('Littermates', from 32-cells onwards). 
## Each dot represents a single ICM cell.
fig1d <- ggplot(FGF.all %>% 
                        ## Select ICM cells of Littermates with 32 or more cells
                        ## and exclude embryos from Aug 20, 2015 experiment
                        filter(Treatment == 'Littermate',
                               Exp_date != '20150820', 
                               TE_ICM != 'TE', 
                               Stage != '<32'),
                ## Plot corrected log of GATA6 (CH4) vs NANOG (CH5) fluorescence
                aes(x = CH4.ebLogCor, y = CH5.ebLogCor))
## Color-code for identity, as assigned by k-means clustering on log scale data
fig1d <- fig1d + geom_point(aes(color = Identity.km), size = I(1.2))
fig1d <- fig1d + scale_color_manual(values = idcols)
## Overlay estimated density contour lines
fig1d <- fig1d + geom_density2d(color = I('orangered4'), size = 0.5)
## Set limits for X and Y axes
fig1d <- fig1d + xlim(-5, 9) + ylim(-5, 9)
## Set aesthetis for the plot
fig1d <- fig1d + theme_bw()
fig1d <- fig1d + labs(color = 'Identity', x = 'log[GATA6]', y = 'log[NANOG]')
fig1d <- fig1d + coord_fixed()
# Bin cells by stage
fig1d <- fig1d + facet_grid( ~ Stage)
## Print the plot to the screen
print(fig1d)

# Figure 1e
## Bar plot representing the lineage composition for each embryo in Fig. 1c 
## ordered by total cell number, represented as percentage of the total
fig1e <- ggplot(FGF.all %>% 
                        ## Select Littermates with 32 or more cells
                        ## and exclude embryos from Aug 20, 2015 experiment
                        filter(Treatment == 'Littermate',
                               Exp_date != '20150820', 
                               Stage != '<32'),
                ## Plot single embryos, ordered by total cell number, on the X axis
                ## and identity (% of total), assigned by k-means clustering, 
                ## on the Y axis
                aes(x = reorder(Embryo_ID, Cellcount), fill = Identity.km), 
                na.rm = TRUE)
fig1e <- fig1e + geom_bar(position = 'fill')
## Color-code for identity
fig1e <- fig1e + scale_fill_manual(values = idcols)
## Set aesthetics for the plot
fig1e <- fig1e + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
fig1e <- fig1e + labs(fill = 'Identity', x = 'Total cell number', y = '% of total')
fig1e <- fig1e + coord_fixed(25)
## Print the plot to the screen
print(fig1e)

# Figure 1f
## Bar plots representing the average lineage composition of embryos in Fig. 1c
## binned into five developmental stages 
fig1f <- ggplot(FGF.all %>% 
                        ## Select Littermates with 32 or more cells
                        ## and exclude embryos from Aug 20, 2015 experiment
                        filter(Treatment == 'Littermate',
                               Exp_date != '20150820', 
                               Stage != '<32'), 
                ## Plot stage on the X axis
                ## and identity (% of total), assigned by k-means clustering, 
                ## on the Y axis
             aes(x = Stage, fill = Identity.km))
fig1f <- fig1f + geom_bar(position = 'fill')
## Color-code for identity
fig1f <- fig1f + scale_fill_manual(values = idcols)
## Set aesthetis for the plot
fig1f <- fig1f + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
fig1f <- fig1f + labs(fill = 'Identity', x = 'Stage', y = '% of total')
fig1f <- fig1f + coord_fixed(10)
## Print the plot to the screen
print(fig1f)

# Figure 1g
## Bar plots representing the ICM composition for each embryo in Fig. 1c
## ordered by total cell number, represented as percentage of ICM cells
fig1g <- ggplot(FGF.all %>% 
                        ## Select Littermates with 32 or more cells
                        ## and exclude embryos from Aug 20, 2015 experiment
                        filter(Treatment == 'Littermate', 
                               Exp_date != '20150820', 
                               TE_ICM == 'ICM',
                               Stage != '<32'), 
                ## Plot single embryos, ordered by total cell number, on the X axis
                ## and identity (% of ICM), assigned by k-means clustering, 
                ## on the Y axis
                aes(x = reorder(Embryo_ID, Cellcount), fill = Identity.km), 
                na.rm = TRUE)
fig1g <- fig1g + geom_bar(position = 'fill')
## Color-code for identity
fig1g <- fig1g + scale_fill_manual(values = idcols)
## Set up aesthetics for the plot
fig1g <- fig1g + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
fig1g <- fig1g + labs(fill = 'Identity', x = 'Total cell number', y = '% of ICM')
fig1g <- fig1g + coord_fixed(25)
## Print the plot to the screen
print(fig1g)

# Figure 1h
## Bar plots representing the average ICM composition of embryos in Fig. 1c
## binned in five developmental stages 
fig1h <- ggplot(FGF.all %>% 
                        ## Select Littermates with 32 or more cells
                        ## and exclude embryos from Aug 20, 2015 experiment
                        filter(Treatment == 'Littermate',
                               Exp_date != '20150820', 
                               TE_ICM == 'ICM',
                               Stage != '<32'), 
                ## Plot stage on the X axis
                ## and identity (% of ICM), assigned by k-means clustering, 
                ## on the Y axis
                aes(x = Stage, fill = Identity.km))
fig1h <- fig1h + geom_bar(position = 'fill')
## Color-code for identity
fig1h <- fig1h + scale_fill_manual(values = idcols)
## Set up aesthetics for the plot
fig1h <- fig1h + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
fig1h <- fig1h + labs(fill = 'Identity', x = 'Stage', y = '% of ICM')
fig1h <- fig1h + coord_fixed(10)
## Print the plot to the screen
print(fig1h)

# ------------------------------------------------------------------------------

# Figure S1a
## Boxplot of total cell number for embryos in Fig. 1
## binned by developmental stage
figS1a <- ggplot(FGF.all %>% 
                         ## Select Littermates only with >32 cells
                         ## and exclude experiment from 08/20/2015
                         filter(Treatment == 'Littermate', 
                                Exp_date != '20150820', 
                                Stage != '<32') %>% 
                         group_by(Embryo_ID, 
                                  Treatment, 
                                  Cellcount, 
                                  Stage) %>%
                         summarise(), 
                 aes(x = Stage, y = Cellcount))
## Create boxplots for each stage
figS1a <- figS1a + geom_boxplot(fill = 'gray', color = 'black', 
                                outlier.shape = 1)
## Overlay a dot for each embryo's cell count
figS1a <- figS1a + geom_jitter(size = I(1.2), color = 'black')
## Set up Y-axis limits and aesthetics for the plot
figS1a <- figS1a + ylim(0, 200) + theme_bw() + coord_fixed(1/20)
figS1a <- figS1a + labs(x = 'Stage', y = 'Total cell number')
## Print the plot to the screen
print(figS1a)

# Figure S1b
## Boxplot of number of cells in each ICM denomination
## for embryos in Fig. 1
## binned by developmental stage
figS1b <- ggplot(FGF.ICMsum %>%
                         ## Select Littermates with more than 32 cells
                         filter(Treatment == 'Littermate', 
                                Stage != '<32'), 
                 ## Plot number of cells for each lineage on the Y axis
                 aes(x = Stage, y = Count))
## Create boxplots for each lineage, for each stage
## and color boxes for identity, assigned by k-means clustering
figS1b <- figS1b + geom_boxplot(aes(fill = Identity.km), color = I('black'), 
                                outlier.shape = 1)
## Overlay a dot for each lineage's cell count (per embryo)
#figS1b <- figS1b + geom_jitter(aes(color = Identity.auto), size = I(1.2))
## Color-code for identity
figS1b <- figS1b + scale_fill_manual(values = idcols)
## Set up aesthetics for the plot
figS1b <- figS1b + theme_bw() + coord_fixed(0.15) + ylim(0, 60)
figS1b <- figS1b + labs(fill = 'Identity', x = 'Stage', y = 'Number of cells')
## Print the plot to the screen
print(figS1b)

# Figure S1c
## Boxplot of number of cells in TE and ICM
## for embryos in Fig. 1
## binned by developmental stage
figS1c <- ggplot(FGF.sum %>% 
                         ## Select Littermates with more than 32 cells
                         filter(Treatment == 'Littermate', 
                                Stage != '<32'), 
                 ## Plot number of ICM or TE cells on the Y axis
                 aes(x = Stage, y = Count))
figS1c <- figS1c + geom_boxplot(aes(fill = TE_ICM), color = I('black'), 
                                outlier.shape = 1)
figS1c <- figS1c + geom_jitter(aes(shape = TE_ICM), color = 'black', 
                               size = I(1.2))
figS1c <- figS1c + scale_fill_manual(values = idcols)
figS1c <- figS1c + theme_bw() + coord_fixed(0.075)
figS1c <- figS1c + labs(fill = 'Identity', x = 'Stage', y = 'Number of cells')
print(figS1c)

# Figure S1d
## Bar plot representing the lineage composition for each embryo in Fig. 1c 
## (all lineages)
## ordered by total cell number, represented as cells per lineage
figs1d <- ggplot(FGF.all %>% 
                         ## Select Littermates only with >32 cells
                         ## and exclude experiment from 08/20/2015
                        filter(Treatment == 'Littermate',
                               Exp_date != '20150820', 
                               Stage != '<32'),
                ## Plot single embryos, ordered by total cell number, on the X axis
                ## and identity (% of total), assigned by k-means clustering, 
                ## on the Y axis
                aes(x = reorder(Embryo_ID, Cellcount), fill = Identity.km), 
                na.rm = TRUE)
figs1d <- figs1d + geom_bar()
## Color-code for identity
figs1d <- figs1d + scale_fill_manual(values = idcols)
## Set aesthetics for the plot
figs1d <- figs1d + theme_bw() + theme(axis.text.x = element_text(angle = 45, 
                                                                 hjust = 1))
figs1d <- figs1d + labs(fill = 'Identity', 
                        x = 'Total cell number', y = 'Cell number')
figs1d <- figs1d + coord_fixed(0.25)
## Print the plot to the screen
print(figs1d)

# Figure S1e
## Local regression curves showing the temporal progression
## in the size of each ICM lineage denomination (as % of the ICM)
## for Littermates shown in Fig. 1

## Define grid for plotting (PrE, DP, EPI+DN)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 2), mgp = c(2, 0.7, 0), lwd = 1.5, pty = 's')
## PrE as % of ICM
plot(Count/ICM.count ~ Cellcount, data = subset(FGF.ICMsum, 
                                                Treatment == "Littermate" & 
                                                        Identity.km == 'PRE'), 
     pch = 16, cex = 0.8, xlim = c(10, 220), ylim = c(0, 1), 
     ylab = 'PrE/ICM', xlab = 'Total cell number')
oo1 <- locfit(Count ~ Cellcount, weights = ICM.count, family = "binomial", 
              data = subset(FGF.ICMsum, Treatment == "Littermate" & 
                                    Cellcount > 15 & 
                                    Identity.km == 'PRE'))
plot(oo1, band = "local", add = TRUE, col = idcols[2], lwd = 2)
## DP as % of ICM
plot(Count/ICM.count ~ Cellcount, data = subset(FGF.ICMsum, 
                                               Treatment == "Littermate" & 
                                                       Identity.km == 'DP'), 
     pch = 16, cex = 0.8, xlim = c(10, 220), ylim = c(0, 1), 
     main = 'Littermate', ylab = 'DP/ICM', xlab = 'Total cell number')
oo1 <- locfit(Count ~ Cellcount, weights = ICM.count, family = "binomial", 
              data = subset(FGF.ICMsum, Treatment == "Littermate" & 
                                    Cellcount > 15 & 
                                    Identity.km == 'DP'))
plot(oo1, band = "local", add = TRUE, col = idcols[3], lwd = 2)
## EPI+DN as % of ICM
plot(Count/ICM.count ~ Cellcount, data = subset(FGF.ICMsum, 
                                               Treatment == "Littermate" & 
                                                       (Identity.km == 'EPI' | 
                                                                Identity.km == 'DN')), 
     pch = 16, cex = 0.8, xlim = c(10, 220), ylim = c(0, 1), 
     ylab = 'EPI+DN/ICM', xlab = 'Total cell number')
oo1 <- locfit(Count ~ Cellcount, weights = ICM.count, family = "binomial", 
              data = subset(FGF.ICMsum, Treatment == "Littermate" & 
                                    Cellcount > 15 & 
                                    (Identity.km == 'EPI' | 
                                             Identity.km == 'DN')))
plot(oo1, band = "local", add = TRUE, col = idcols[1], lwd = 2)

## Reset grid to 1x1
par(mfrow = c(1, 1))

# Figure S2a-c
## Boxplot for Channels 1 (Hoechst (a)), 4 (GATA6 (b)) and 5 (NANOG (c))
## after Empirical Bayes correction
## for ICM cells only, for each experiment, ordered by ascending imaging date
## Figure S2a
figS2a <- ggplot(FGF.all %>% 
                         ## Select ICM cells only
                         ## for embryos stained for CDX2, GATA6 and NANOG
                         filter(TE_ICM == 'ICM',
                                Markers == 'C2G6NG'), 
                 ## Arrange experiments by date and plot against Hoechst (CH1)
                 aes(x = reorder(Experiment, Exp_date), y = CH1.ebLogCor))
## Color grey (Hoechst)
figS2a <- figS2a + geom_boxplot(fill = I('grey'), color = I('black'), 
                                outlier.shape = 1)
## Optional: plot all cells as individual points over the box and whiskers
#figS2a <- figS2a + geom_jitter(alpha = I(0.25), size = I(0.5)) + theme_bw() 
figS2a <- figS2a + theme(axis.text.x = element_text(angle = 45, hjust = 1))
figS2a <- figS2a + labs(x = 'Experiment, by date', y = 'log[Hoechst]')
print(figS2a)

## Figure S2b
figS2b <- ggplot(FGF.all %>% 
                         ## Select ICM cells only
                         ## for embryos stained for CDX2, GATA6 and NANOG
                         filter(TE_ICM == 'ICM',
                                Markers == 'C2G6NG'), 
                 ## Arrange experiments by date and plot against GATA6 (CH4)
                 aes(x = reorder(Experiment, Exp_date), y = CH4.ebLogCor))
## Color blue (GATA6)
figS2b <- figS2b + geom_boxplot(fill = I('blue'), color = I('black'), 
                              outlier.shape = 1)
## Optional: plot all cells as individual points over the box and whiskers
#figS2b <- figS2b + geom_jitter(alpha = I(0.25), size = I(0.5)) + theme_bw()
figS2b <- figS2b + theme(axis.text.x = element_text(angle = 45, hjust = 1))
figS2b <- figS2b + labs(x = 'Experiment, by date', y = 'log[GATA6]')
print(figS2b)

## Figure S2c
figS2c <- ggplot(FGF.all %>% 
                         ## Select ICM cells only
                         ## for embryos stained for CDX2, GATA6 and NANOG
                         filter(TE_ICM == 'ICM',
                                Markers == 'C2G6NG'), 
                 ## Arrange experiments by date and plot against NANOG (CH5)
                aes(x = reorder(Experiment, Exp_date), y = CH5.ebLogCor))
## Color red (NANOG)
figS2c <- figS2c + geom_boxplot(fill = I('red'), color = I('black'), 
                              outlier.shape = 1)
## Optional: plot all cells as individual points over the box and whiskers
#figS2c <- figS2c + geom_jitter(alpha = I(0.25), size = I(0.5)) + theme_bw()
figS2c <- figS2c + theme(axis.text.x = element_text(angle = 45, hjust = 1))
figS2c <- figS2c + labs(x = 'Experiment, by date', y = 'log[NANOG]')
print(figS2c)