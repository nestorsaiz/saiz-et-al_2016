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
## Scatter plots for the levels of GATA6 and NANOG 
## in individual ICM cells of embryos collected at sequential stages 
## of development as in Fig. 1b ('Littermates', from 32-cells onwards). 
## Each dot represents a single ICM cell.
fig1c <- ggplot(FGF.all %>% 
                        ## Select ICM cells of Littermates with 32 or more cells
                        filter(Treatment == 'Littermate',
                               TE_ICM != 'TE', 
                               Stage != '<32'),
                ## Plot corrected log of GATA6 (CH4) vs NANOG (CH5) fluorescence
                aes(x = CH4.logCor, y = CH5.logCor))
## Color-code for identity, as assigned automatically with 'identify.R'
fig1c <- fig1c + geom_point(aes(color = Identity.auto))
fig1c <- fig1c + scale_color_manual(values = idcols)
# fig1c <- fig1c + geom_density2d()
## Set limits for X and Y axes
fig1c <- fig1c + xlim(-4, 7) + ylim(-4, 7)
## Set aesthetis for the plot
fig1c <- fig1c + theme_bw()
fig1c <- fig1c + labs(color = 'Identity', x = 'log[GATA6]', y = 'log[NANOG]')
fig1c <- fig1c + facet_grid( ~ Stage)
fig1c <- fig1c + coord_fixed()
## Print the plot to the screen
print(fig1c)

# Figure 1d
## Bar plots representing the ICM composition for each embryo in Fig. 1c
## ordered by total cell number, represented as percentage of ICM cells
fig1d <- ggplot(FGF.all %>% 
                        ## Select ICM cells of Littermates with 32 or more cells
                        filter(TE_ICM == 'ICM', 
                          Treatment == 'Littermate', 
                          Markers == 'C2G6NG', 
                          Stage != '<32'),
                ## Plot total cell number as a factor on the X axis
                ## and identity, assigned automatically, on the Y axis
            aes(x = factor(Cellcount), fill = Identity.auto))
fig1d <- fig1d + geom_bar(position = 'fill')
## Color-code for identity
fig1d <- fig1d + scale_fill_manual(values = idcols)
## Set aesthetics for the plot
fig1d <- fig1d + theme_bw()
fig1d <- fig1d + coord_fixed(15)
## Print the plot to the screen
print(fig1d)

# Figure 1e
## Bar plots representing ICM composition of embryos in Fig. 1c
## binned in four developmental stages 
fig1e <- ggplot(FGF.all %>% 
                        ## Select ICM cells of Littermates with 32 or more cells
                        filter(TE_ICM == 'ICM', 
                               Treatment == 'Littermate', 
                               Stage != '<32'), 
                ## Plot stage on the X axis
                ## and identity, assigned automatically, on the Y axis
             aes(x = Stage, fill = Identity.auto))
fig1e <- fig1e + geom_bar(position = 'fill')
## Color-code for identity
fig1e <- fig1e + scale_fill_manual(values = idcols)
## Set aesthetis for the plot
fig1e <- fig1e + theme_bw()
fig1e <- fig1e + coord_fixed(10)
## Print the plot to the screen
print(fig1e)

# Figure 1f
## Bar plots representing the proportion of TE vs ICM for each embryo in Fig. 1c
## ordered by total cell number, represented as percentage of the total
fig1f <- ggplot(FGF.all %>% 
                        ## Select Littermates with 32 or more cells
                        filter(Treatment == 'Littermate', 
                               Stage != '<32'), 
                ## Plot total cell number as a factor on the X axis
                ## and TE vs ICM identity on the Y axis
                aes(x = factor(Cellcount), fill = TE_ICM))
fig1f <- fig1f + geom_bar(position = 'fill')
## Color-code for TE vs ICM
fig1f <- fig1f + scale_fill_manual(values = idcols)
## Set up aesthetics for the plot
fig1f <- fig1f + theme_bw() + coord_fixed(15)
## Print the plot to the screen
print(fig1f)

# Figure 1g
## Bar plots representing the proportion of TE vs ICM of embryos in Fig. 1c
## binned in four developmental stages 
fig1g <- ggplot(FGF.all %>% 
                        ## Select Littermates with 32 or more cells
                        filter(Treatment == 'Littermate', 
                               Stage != '<32'), 
                ## Plot stage on the X axis
                ## and TE vs ICM identity on the Y axis
                aes(x = Stage, fill = TE_ICM))
fig1g <- fig1g + geom_bar(position = 'fill')
## Color-code for TE vs ICM
fig1g <- fig1g + scale_fill_manual(values = idcols)
## Set up aesthetics for the plot
fig1g <- fig1g + theme_bw()
fig1g <- fig1g + coord_fixed(10)
## Print the plot to the screen
print(fig1g)

# Figure S1a
## Boxplot of total cell number for embryos in Fig. 1
## binned by developmental stage
figS1a <- qplot(Stage, Cellcount, 
                ## Select Littermates with more than 32 cells
                data = FGF.all %>%
                        group_by(Embryo_ID, 
                                 Treatment, 
                                 Cellcount, 
                                 Stage) %>%
                        summarise() %>% 
                        filter(Treatment == 'Littermate', 
                               Stage != '<32'), 
                ## Set up type of plot - boxplot with one dot per embryo
                ## Set up Y axis limits
                geom = c('boxplot', 'jitter'), ylim = c(0, 200), 
                ## Set up aesthetics for the plot
                fill = I('gray')) + theme_bw() + coord_fixed(1/20)
## Print the plot to the screen
print(figS1a)

# Figure S1b
## Boxplot of number of cells in each ICM denomination
## for embryos in Fig. 1
## binned by developmental stage
figS1b <- qplot(Stage, Count, 
                ## Select Littermates with more than 32 cells
                data = subset(FGF.ICMsum, 
                              Treatment == 'Littermate' & 
                                      Stage != '<32'), 
                ## Plot automatically assigned identity on the Y axis
                ## and select plot type - boxplot with one dot per embryo
                fill = Identity.auto, geom = c('boxplot', 'jitter')) + 
        ## Color-code for identity
        scale_fill_manual(values = idcols) + 
        ## Set up aesthetics for the plot
        theme_bw() + coord_fixed(0.15)
## Print the plot to the screen
print(figS1b)

# Figure S1c
## Boxplot of number of cells in TE and ICM
## for embryos in Fig. 1
## binned by developmental stage
figS1c <- qplot(Stage, Count, 
                ## Select Littermates with more than 32 cells
                data = subset(FGF.sum, 
                              Treatment == 'Littermate' & 
                                      Stage != '<32'), 
                ## Plot TE vs ICM on the Y axis and select plot type -
                ## boxplot with one dot per embryo
                fill = TE_ICM, geom = c('boxplot', 'jitter')) + 
        ## Color-code for identity
        scale_fill_manual(values = idcols) + 
        ## Set up aesthetics for the plot
        theme_bw() + coord_fixed(0.075)
## Print the plot to the screen
print(figS1c)
