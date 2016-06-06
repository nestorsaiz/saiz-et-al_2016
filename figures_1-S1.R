# Saiz *et al* (2016)
# Figures 1 and S1 plots

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

# Figure 1c
## Scatter plots showing the levels of GATA6 and NANOG in log scale
## in individual ICM cells of embryos collected at sequential stages 
## of development as in Fig. 1b ('Littermates', from 32-cells onwards). 
## Each dot represents a single ICM cell.
fig1c <- ggplot(FGF.all %>% 
                        ## Select ICM cells of Littermates with 32 or more cells
                        filter(Treatment == 'Littermate',
                               Exp_date != '20150820', 
                               TE_ICM != 'TE', 
                               Stage != '<32'),
                ## Plot corrected log of GATA6 (CH4) vs NANOG (CH5) fluorescence
                aes(x = CH4.ebLogCor, y = CH5.ebLogCor))
## Color-code for identity, as assigned automatically with 'identify.R'
fig1c <- fig1c + geom_point(aes(color = Identity.auto), size = I(1.2))
## Overlay estimated density contour lines
fig1c <- fig1c + geom_density2d(color = I('orangered4'), size = 0.5)
fig1c <- fig1c + scale_color_manual(values = idcols)
## Set limits for X and Y axes
fig1c <- fig1c + xlim(-5, 9) + ylim(-5, 9)
## Set aesthetis for the plot
fig1c <- fig1c + theme_bw()
fig1c <- fig1c + labs(color = 'Identity', x = 'log[GATA6]', y = 'log[NANOG]')
fig1c <- fig1c + facet_grid( ~ Stage)
fig1c <- fig1c + coord_fixed()
## Print the plot to the screen
print(fig1c)

## Bar plot representing the composition for each embryo in Fig. 1c (all lineages)
## ordered by total cell number, represented as total number of cells
# fig1d <- ggplot(FGF.all %>% 
#                         ## Select Littermates with 32 or more cells
#                         filter(Treatment == 'Littermate',
#                                Stage != '<32'),
#                 ## Plot total cell number as a factor on the X axis
#                 ## and identity (% of ICM), assigned automatically, on the Y axis
#                 aes(x = reorder(Embryo_ID, Cellcount), fill = Identity.auto), na.rm = TRUE)
# fig1d <- fig1d + geom_bar()
# ## Color-code for identity
# fig1d <- fig1d + scale_fill_manual(values = idcols)
# ## Set aesthetics for the plot
# fig1d <- fig1d + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# fig1d <- fig1d + labs(fill = 'Identity', x = 'Total cell number')
# fig1d <- fig1d + coord_fixed(0.25)
# ## Print the plot to the screen
# print(fig1d)

# Figure 1d
## Bar plot representing the lineage composition for each embryo in Fig. 1c (all lineages)
## ordered by total cell number, represented as percentage of the total
fig1d <- ggplot(FGF.all %>% 
                        ## Select Littermates with 32 or more cells
                        filter(Treatment == 'Littermate',
                               Exp_date != '20150820', 
                               Stage != '<32'),
                ## Plot single embryos, ordered by total cell number, on the X axis
                ## and identity (% of total), assigned automatically, on the Y axis
                aes(x = reorder(Embryo_ID, Cellcount), fill = Identity.auto), 
                na.rm = TRUE)
fig1d <- fig1d + geom_bar(position = 'fill')
## Color-code for identity
fig1d <- fig1d + scale_fill_manual(values = idcols)
## Set aesthetics for the plot
fig1d <- fig1d + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
fig1d <- fig1d + labs(fill = 'Identity', x = 'Total cell number', y = '% of total')
fig1d <- fig1d + coord_fixed(25)
## Print the plot to the screen
print(fig1d)

# Figure 1e
## Bar plots representing the lineage composition of embryos in Fig. 1c
## binned into five developmental stages 
fig1e <- ggplot(FGF.all %>% 
                        ## Select Littermates with 32 or more cells
                        filter(Treatment == 'Littermate',
                               Exp_date != '20150820', 
                               Stage != '<32'), 
                ## Plot stage on the X axis
                ## and identity (% of total), assigned automatically, on the Y axis
             aes(x = Stage, fill = Identity.auto))
fig1e <- fig1e + geom_bar(position = 'fill')
## Color-code for identity
fig1e <- fig1e + scale_fill_manual(values = idcols)
## Set aesthetis for the plot
fig1e <- fig1e + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
fig1e <- fig1e + labs(fill = 'Identity', x = 'Stage', y = '% of total')
fig1e <- fig1e + coord_fixed(10)
## Print the plot to the screen
print(fig1e)

# Figure 1f
## Bar plots representing the ICM composition for each embryo in Fig. 1c
## ordered by total cell number, represented as percentage of ICM cells
fig1f <- ggplot(FGF.all %>% 
                        ## Select ICM cells of Littermates with 32 or more cells
                        filter(Treatment == 'Littermate', 
                               Exp_date != '20150820', 
                               TE_ICM == 'ICM',
                               Stage != '<32'), 
                ## Plot single embryos, ordered by total cell number, on the X axis
                ## and identity (% of ICM), assigned automaticallyl, on the Y axis
                aes(x = reorder(Embryo_ID, Cellcount), fill = Identity.auto), 
                na.rm = TRUE)
fig1f <- fig1f + geom_bar(position = 'fill')
## Color-code for identity
fig1f <- fig1f + scale_fill_manual(values = idcols)
## Set up aesthetics for the plot
fig1f <- fig1f + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
fig1f <- fig1f + labs(fill = 'Identity', x = 'Total cell number', y = '% of ICM')
fig1f <- fig1f + coord_fixed(25)
## Print the plot to the screen
print(fig1f)

# Figure 1g
## Bar plots representing the ICM composition of embryos in Fig. 1c
## binned in five developmental stages 
fig1g <- ggplot(FGF.all %>% 
                        ## Select ICM cells of Littermates with 32 or more cells
                        filter(Treatment == 'Littermate',
                               Exp_date != '20150820', 
                               TE_ICM == 'ICM',
                               Stage != '<32'), 
                ## Plot stage on the X axis
                ## and % of TE vs ICM identity on the Y axis
                aes(x = Stage, fill = Identity.auto))
fig1g <- fig1g + geom_bar(position = 'fill')
## Color-code for TE vs ICM
fig1g <- fig1g + scale_fill_manual(values = idcols)
## Set up aesthetics for the plot
fig1g <- fig1g + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
fig1g <- fig1g + labs(fill = 'Identity', x = 'Stage', y = '% of ICM')
fig1g <- fig1g + coord_fixed(10)
## Print the plot to the screen
print(fig1g)

--------------------------------------------------------------------------------

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
figS1b <- figS1b + geom_boxplot(aes(fill = Identity.auto), color = I('black'), 
                                outlier.shape = 1)
## Overlay a dot for each lineage's cell count (per embryo)
#figS1b <- figS1b + geom_jitter(aes(color = Identity.auto), size = I(1.2))
## Color-code for identity
figS1b <- figS1b + scale_fill_manual(values = idcols)
#figS1b <- figS1b + scale_color_manual(values = idcols)
## Set up aesthetics for the plot
figS1b <- figS1b + theme_bw() + coord_fixed(0.15)
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
## Bar plot representing the lineage composition for each embryo in Fig. 1c (all lineages)
## ordered by total cell number, represented as cells per lineage
figs1d <- ggplot(FGF.all %>% 
                        ## Select Littermates with 32 or more cells
                        filter(Treatment == 'Littermate',
                               Exp_date != '20150820', 
                               Stage != '<32'),
                ## Plot single embryos, ordered by total cell number, on the X axis
                ## and identity (% of total), assigned automatically, on the Y axis
                aes(x = reorder(Embryo_ID, Cellcount), fill = Identity.auto), 
                na.rm = TRUE)
figs1d <- figs1d + geom_bar()
## Color-code for identity
figs1d <- figs1d + scale_fill_manual(values = idcols)
## Set aesthetics for the plot
figs1d <- figs1d + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
figs1d <- figs1d + labs(fill = 'Identity', x = 'Total cell number', y = 'Cell number')
figs1d <- figs1d + coord_fixed(0.25)
## Print the plot to the screen
print(figs1d)

# Figure S1e
## Scatter plots showing the levels of GATA6 and NANOG in linear scale
## in individual ICM cells of embryos collected at sequential stages 
## of development as in Fig. 1b ('Littermates', from 32-cells onwards). 
## Each dot represents a single ICM cell.
FGF.all$ratio <- FGF.all$CH4.ebLogCor - FGF.all$CH5.ebLogCor
figs1e <- ggplot(FGF.all %>% 
                        ## Select ICM cells of Littermates with 32 or more cells
                        filter(Treatment == 'Littermate',
                               Exp_date != '20150820', 
                               TE_ICM != 'TE', 
                               Stage != '<32'),
                ## Plot corrected log of GATA6 (CH4) vs NANOG (CH5) fluorescence
                aes(x = exp(CH4.ebLogCor), y = exp(CH5.ebLogCor)))
figs1e <- figs1e + geom_point(aes(colour = ratio), size = I(1.2))
figs1e <- figs1e + geom_abline(slope = 1/exp(0.5), intercept = 0, linetype = 2)
figs1e <- figs1e + geom_abline(slope = 1/exp(-1.5), intercept = 0, linetype = 2)
#figs1e <- figs1e + scale_color_manual(values = idcols)
## Color with a gradient based on the ratio of GATA6 to NANOG
figs1e <- figs1e + scale_color_gradient2(low = 'red', mid = 'purple', high = 'blue')
## Set limits for X and Y axes
figs1e <- figs1e + xlim(0, 255) + ylim(0, 255)
## Set aesthetis for the plot
figs1e <- figs1e + theme_bw()
figs1e <- figs1e + labs(color = 'Identity', x = '[GATA6]', y = '[NANOG]')
figs1e <- figs1e + facet_grid( ~ Stage)
figs1e <- figs1e + coord_fixed()
## Print the plot to the screen
print(figs1e)