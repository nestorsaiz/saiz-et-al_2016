scaling <- readRDS('scaling_all.rds')
scaling$TE_ICM <- factor(scaling$TE_ICM, levels = c('ICM', 'TE'))
scaling$Regime <- as.factor(scaling$Regime)
scaling$Experiment <- as.factor(scaling$Experiment)
scaling$Genotype <- as.factor(scaling$Genotype)
scaling$Litter <- as.factor(scaling$Litter)
scaling$Identity <- factor(scaling$Identity, 
                           levels = c('DN', 'EPI', 'DP', 'PRE', 'TE'))
scaling$Identity.km <- factor(scaling$Identity.km, 
                              levels = c('DN', 'EPI', 'DP', 'PRE', 'ICM', 'TE'))
scaling$Treatment <- factor(scaling$Treatment, levels = c('Control', 'Single', 'Half', 
                                                          'Double', 'FGF4_1000', 
                                                          'PD03_1', 'AZD_1', 'SU_10', 'SU_20',
                                                          'FGF42PD', 'Littermate'))

# Make summary tables for cellcounts only
scal.counts <- scaling %>% 
        group_by(Embryo_ID, Treatment, Cellcount, Regime) %>% 
        summarise()
scal.teicm <- scaling %>%
        group_by(Embryo_ID, Treatment, Cellcount, TE_ICM, Regime) %>% 
        summarise(Count = n()) %>% 
        mutate(pcTotal = Count / Cellcount * 100)

# Make summary table with N numbers
scal.n <- scaling %>% 
        group_by(Treatment, Embryo_ID) %>% 
        summarise() %>% 
        group_by(Treatment) %>% 
        summarise(N.embryos = n())