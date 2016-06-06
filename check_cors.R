library('ggplot2')
#pdf(file = 'EB_cor_check.pdf', paper = 'a4r', width = 11)

## Boxplot Channels 1 (Hoechst), 4 (GATA6) and 5 (NANOG)
## after Empirical Bayes correction
## for ICM cells only, for each experiment, ordered by ascending imaging date
# Channel 1 (Hoechst)
dapi <- ggplot(subset(FGF.all, TE_ICM == 'ICM' & Markers == 'C2G6NG'), 
               aes(x = reorder(Experiment, Exp_date), y = CH1.ebLogCor))
dapi <- dapi + geom_boxplot(fill = I('grey'), color = I('black'), 
                            outlier.shape = 1)
dapi <- dapi + geom_jitter(alpha = I(0.25), size = I(0.5)) + theme_bw() 
dapi <- dapi + theme(axis.text.x = element_text(angle = 45, hjust = 1))
dapi <- dapi + labs(x = 'Experiment, by date', y = 'log[Hoechst]')
print(dapi)

# Channel 4 (GATA6)
gata6 <- ggplot(subset(FGF.all, TE_ICM == 'ICM' & Markers == 'C2G6NG'), 
                aes(x = reorder(Experiment, Exp_date), y = CH4.ebLogCor))
gata6 <- gata6 + geom_boxplot(fill = I('blue'), color = I('black'), 
                              outlier.shape = 1)
gata6 <- gata6 + geom_jitter(alpha = I(0.25), size = I(0.5)) + theme_bw()
gata6 <- gata6 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
gata6 <- gata6 + labs(x = 'Experiment, by date', y = 'log[GATA6]')
print(gata6)

# Channel 5 (NANOG)
nanog <- ggplot(subset(FGF.all, TE_ICM == 'ICM' & Markers == 'C2G6NG'), 
                aes(x = reorder(Experiment, Exp_date), y = CH5.ebLogCor))
nanog <- nanog + geom_boxplot(fill = I('red'), color = I('black'), 
                              outlier.shape = 1)
nanog <- nanog + geom_jitter(alpha = I(0.25), size = I(0.5)) + theme_bw()
nanog <- nanog + theme(axis.text.x = element_text(angle = 45, hjust = 1))
nanog <- nanog + labs(x = 'Experiment, by date', y = 'log[NANOG]')
print(nanog)

#dev.off()