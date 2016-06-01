library('ggplot2')
#pdf(file = 'EB_cor_check.pdf', paper = 'a4r', width = 11)

## Boxplot Channels 1 (Hoechst), 4 (GATA6) and 5 (NANOG)
## after Empirical Bayes correction
## for ICM cells only, for each experiment, ordered by ascending imaging date
# Channel 1 (Hoechst)
dapi <- ggplot(subset(FGF.all, TE_ICM == 'ICM' & Markers == 'C2G6NG'), 
               aes(x = factor(Exp_date), y = CH1.ebLogCor))
dapi <- dapi + geom_boxplot(fill = I('grey'))
dapi <- dapi + geom_jitter(alpha = I(0.25), size = I(0.5)) + theme_bw() 
dapi <- dapi + theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(dapi)

# Channel 4 (GATA6)
gata6 <- ggplot(subset(FGF.all, TE_ICM == 'ICM' & Markers == 'C2G6NG'), 
                aes(x = factor(Exp_date), y = CH4.ebLogCor))
gata6 <- gata6 + geom_boxplot(fill = I('blue'))
gata6 <- gata6 + geom_jitter(alpha = I(0.25), size = I(0.5)) + theme_bw()
gata6 <- gata6 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(gata6)

# Channel 5 (NANOG)
nanog <- ggplot(subset(FGF.all, TE_ICM == 'ICM' & Markers == 'C2G6NG'), 
                aes(x = factor(Exp_date), y = CH5.ebLogCor))
nanog <- nanog + geom_boxplot(fill = I('red'))
nanog <- nanog + geom_jitter(alpha = I(0.25), size = I(0.5)) + theme_bw()
nanog <- nanog + theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(nanog)

#dev.off()