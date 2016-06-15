## TABLES

# Summary tables:

# Count all embryos analyzed, 
# group by Regime and treatment 
# (and experimental point)
FGF.allembryos <- FGF.all %>% 
        filter(Exp_date != '20150820') %>% 
        group_by(Embryo_ID, Regime, 
                 Treatment, 
                 Xpoint) %>%
        summarise(Cells = n()) %>%
        group_by(Regime, Treatment, 
                 Xpoint) %>%
        summarise(Embryos = n())

# Only Littermates, 
# grouped by Stage
FGF.lm.embryos <- FGF.all %>%
        filter(Treatment == 'Littermate', 
               Exp_date != '20150820') %>%
        group_by(Embryo_ID, Stage) %>%
        summarise(Cells = n()) %>%
        group_by(Stage) %>%
        summarise(Embryos = n())

# FGF.lm.cells <- FGF.all %>% 
#   filter(Treatment == 'Littermate') %>% 
#   group_by(Embryo_ID, Stage,
#            Identity.auto, 
#            Cellcount, 
#            Experimenter) %>% 
#   summarise(Cells = n())

# Count all cells in all embryos,
# group by Regime and treatment
# (and X point and treatment length)
FGF.allcells <- FGF.all %>%
        group_by(Regime, Treatment, Tt_length, 
                 Xpoint) %>%
        summarise(Cells = n())

# Count ICM cells in all embryos
# group by Regime and treatment
# (and X point and treatment length)
FGF.all.ICMcells <- FGF.all %>% 
        filter(Exp_date != '20150820') %>% 
        group_by(Regime, Treatment, Tt_length, 
                 Xpoint, TE_ICM) %>%
        filter(TE_ICM == 'ICM') %>%
        summarise(ICMcells = n())

# Count ICM cells in Littermates
# group by Stage
FGF.lm.ICMcells <- FGF.all %>%
        filter(TE_ICM == 'ICM', 
               Treatment == 'Littermate', 
               Exp_date != '20150820') %>%
        group_by(Stage, Treatment, TE_ICM) %>%
        summarise(ICMcells = n())

# Table for N numbers in Figure 1c
FGF.lms <- merge(FGF.lm.embryos, FGF.lm.ICMcells)

# Table for N numbers in Figure 2b & S2b
FGF.F2.icm <- FGF.all %>% filter(Treatment == 'Control' | 
                                         Treatment == 'FGF4_1000' | 
                                         Treatment == 'PD03_1' | 
                                         Treatment == 'AZD_1' | 
                                         Treatment == 'SU_20',
                               Xpoint != 'xp', 
                               TE_ICM == 'ICM', 
                               Markers == 'C2G6NG', 
                               Regime != 'R8', 
                               Regime != 'R9', 
                               Regime != 'R3L') %>% 
        group_by(Regime, Treatment) %>% summarise(ICMcells = n())
FGF.F2.emb <- FGF.all %>% filter(Treatment == 'Control' | 
                                         Treatment == 'FGF4_1000' | 
                                         Treatment == 'PD03_1' | 
                                         Treatment == 'AZD_1' | 
                                         Treatment == 'SU_20',
                                 Xpoint != 'xp', 
                                 TE_ICM == 'ICM', 
                                 Markers == 'C2G6NG', 
                                 Regime != 'R8', 
                                 Regime != 'R9', 
                                 Regime != 'R3L') %>% 
        group_by(Regime, Treatment, Embryo_ID) %>% 
        summarise() %>% group_by(Regime, Treatment) %>%
        summarise(N = n())
FGF.F2.N <- merge(FGF.F2.icm, FGF.F2.emb)
rm(FGF.F2.icm, FGF.F2.emb)

# Count TE and ICM cells for each embryo
# and calculate absolute count and % of total
FGF.sum <- FGF.all %>% 
        filter(Exp_date != '20150820') %>%
        group_by(Embryo_ID, Experiment, Experimenter, 
                 Regime, Stage, Treatment, Tt_length, 
                 Markers, Xpoint, Cellcount, TE_ICM) %>%
        summarise(Count = n()) %>%
        mutate(pcTotal = Count / Cellcount * 100)

# Count ICM cells only for each embryo 
# calculate count for each identity
# and % of ICM for each identity
# and calculate average levels for each channel
FGF.ICMsum <- FGF.all %>%
        filter(TE_ICM == 'ICM', 
               Exp_date != '20150820') %>%
        group_by(Embryo_ID, Experiment, Regime, 
                 Stage, Treatment, Tt_length, 
                 Cellcount, Identity.km, Xpoint, 
                 Markers) %>%
        summarise(Count = n(), 
                  CH1 = mean(CH1.ebLogCor, na.rm = TRUE),
                  CH4 = mean(CH4.ebLogCor, na.rm = TRUE), 
                  CH5 = mean(CH5.ebLogCor, na.rm = TRUE))

# Build equivalent table to FGF.ICMsum, 
# only for embryos stained with OCT4, GATA4 and NANOG
FGF.ICMsumO4 <- FGF.all %>%
        filter(TE_ICM == 'ICM', Markers == 'O4G4NG') %>%
        group_by(Embryo_ID, Experiment, Regime, 
                 Stage, Treatment, Tt_length, 
                 Cellcount, Identity.auto, Xpoint, 
                 Identity, Markers) %>%
        summarise(Count = n(), 
                  CH1 = mean(CH1.ebLogCor, na.rm = TRUE),
                  CH2 = mean(CH2.logCor, na.rm = TRUE),
                  CH4 = mean(CH4.ebLogCor, na.rm = TRUE), 
                  CH5 = mean(CH5.ebLogCor, na.rm = TRUE))

# # FGFRi summary
# Ri <- FGF_all %>%
#   filter(Treatment == 'AZD_1' | Treatment == 'SU_20') %>% 
#   group_by(Embryo_ID, Experiment, Treatment, Regime, 
#            Tt_length, Cellcount, Markers) %>% 
#   summarise(Count = n())
# head(Ri)