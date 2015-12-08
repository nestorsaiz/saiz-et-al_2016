# Saiz *et al* (2016)
# Figures 1 and S1 plots

# Load data and apply transformations
source('Transformations.R')

# Figure 1c

fig1c <- ggplot(FGF.all %>% 
                        filter(Treatment == 'Littermate',
                               TE_ICM != 'TE', 
                               Stage != '<32'),
                aes(x = CH4.logCor, y = CH5.logCor))
fig1c <- fig1c + geom_point(aes(color = Identity.auto))
fig1c <- fig1c + scale_color_manual(values = c('EPI' = 'red', 'PRE' = 'blue', 
                                               'DP' = 'purple', 'DN' = 'gray'))
# fig1c <- fig1c + geom_density2d()
fig1c <- fig1c + xlim(-4, 7) + ylim(-4, 7)
fig1c <- fig1c + theme_bw()
fig1c <- fig1c + labs(color = 'Identity', x = 'log[GATA6]', y = 'log[NANOG]')
fig1c <- fig1c + facet_grid( ~ Stage)
fig1c <- fig1c + coord_fixed()
print(fig1c)

# qplot(CH4.logCor, CH5.logCor, 
#       data = FGF.all %>% 
#               filter(Treatment == 'Littermate',
#                      TE_ICM != 'TE', 
#                      Stage != '<32'),
#       color = Identity.auto, 
#       xlim = c(-4, 7), ylim = c(-4, 7)) + 
#         facet_grid(~Stage) + 
#         scale_color_manual(values = c('EPI' = 'red', 'PRE' = 'blue', 
#                                       'DP' = 'purple', 'DN' = 'gray')) + 
#         theme_bw() + coord_fixed()

# Figure 1d

fig1d <- ggplot(subset(FGF.all, 
                   TE_ICM == 'ICM' & 
                           Treatment == 'Littermate' & 
                           Markers == 'C2G6NG' & 
                           Stage != '<32'),
            aes(x = factor(Cellcount), fill = Identity.auto))
fig1d <- fig1d + geom_bar(position = 'fill')
fig1d <- fig1d + scale_fill_manual(values = c('EPI' = 'red', 'PRE' = 'blue', 
                                      'DP' = 'purple', 'DN' = 'gray'))
fig1d <- fig1d + theme_bw()
fig1d <- fig1d + coord_fixed(15)
print(fig1d)

# qplot(factor(Cellcount), data = subset(FGF.all, 
#                                        TE_ICM == 'ICM' & 
#                                                Treatment == 'Littermate' & 
#                                                Stage != '<32'), 
#       fill = Identity.auto, geom = 'bar', position = 'fill') + 
#         scale_fill_manual(values = c('EPI' = 'red', 'PRE' = 'blue', 
#                                      'DP' = 'purple', 'DN' = 'gray')) + 
#         theme_bw() + coord_fixed(15)

# Figure 1e

fig1e <- ggplot(subset(FGF.all, 
                    TE_ICM == 'ICM' & 
                            Treatment == 'Littermate' & 
                            Stage != '<32'), 
             aes(x = Stage, fill = Identity.auto))
fig1e <- fig1e + geom_bar(position = 'fill')
fig1e <- fig1e + scale_fill_manual(values = c('EPI' = 'red', 'PRE' = 'blue', 
                                        'DP' = 'purple', 'DN' = 'gray'))
fig1e <- fig1e + theme_bw()
fig1e <- fig1e + coord_fixed(10)
print(fig1e)

# qplot(Stage, data = subset(FGF.all, 
#                            TE_ICM == 'ICM' & 
#                                    Treatment == 'Littermate' & 
#                                    Stage != '<32'), 
#       fill = Identity.auto, geom = 'bar', position = 'fill') + 
#         scale_fill_manual(values = c('EPI' = 'red', 'PRE' = 'blue', 
#                                      'DP' = 'purple', 'DN' = 'gray')) + 
#         theme_bw() + coord_fixed(10)


# Figure 1f

qplot(factor(Cellcount), pcTotal_avg, 
      data = FGF.sum %>% 
              filter(Treatment == 'Littermate', 
                     Stage != '<32') %>% 
              group_by(Cellcount, 
                       Treatment, TE_ICM) %>% 
              summarise(pcTotal_avg = mean(pcTotal)), 
      geom = 'bar', 
      fill = TE_ICM) + 
        theme_bw() + 
        scale_fill_manual(values = c('TE' = 'green', 'ICM' = 'purple')) + 
        geom_bar(stat = 'identity') + coord_fixed(0.15)

# Figure 1g

qplot(Stage, pcTotal_avg, 
      data = FGF.sum %>% 
              filter(Treatment == 'Littermate', 
                     Stage != '<32') %>% 
              group_by(Stage, 
                       Treatment, TE_ICM) %>% 
              summarise(pcTotal_avg = mean(pcTotal)), 
      geom = 'bar', 
      fill = TE_ICM) + 
        theme_bw() + 
        scale_fill_manual(values = c('TE' = 'green', 'ICM' = 'purple')) + 
        geom_bar(stat = 'identity') + coord_fixed(1/10)