# Table creation

## Load data and apply transformations if not loaded yet
data.ok <- exists('FGF.all') 
if (data.ok == FALSE) {
        source('Transformations.R')
}
rm(data.ok)

## Count all embryos analyzed, 
## group by Regime and treatment 
## (and experimental point)
table.allembryos <- FGF.all %>% 
        filter(Exp_date != '20150820') %>% 
        group_by(Embryo_ID, Regime, 
                 Treatment, 
                 Xpoint) %>%
        summarise(Cells = n()) %>%
        group_by(Regime, Treatment, 
                 Xpoint) %>%
        summarise(Embryos = n())

## Table for N numbers in Figure 1c
## Calculate the number of Littermates (fixed upon collection)
## with 32 or more cells 
## grouped by Stage
FGF.lm.embryos <- FGF.all %>%
        ## Select Littermates, with 32+ cells
        ## excluding experiment from August 20, 2015
        filter(Treatment == 'Littermate', 
               Stage != '<32',
               Exp_date != '20150820') %>%
        group_by(Embryo_ID, Stage) %>%
        summarise(Cells = n()) %>%
        group_by(Stage) %>%
        summarise(Embryos = n())
## Count ICM cells in Littermates (fixed upon collection)
## with 32 or more cells 
## grouped by Stage
FGF.lm.ICMcells <- FGF.all %>%
        ## Select ICM cells of Littermates with 32+ cells
        ## excluding experiment from August 20, 2015
        filter(TE_ICM == 'ICM', 
               Stage != '<32',
               Treatment == 'Littermate', 
               Exp_date != '20150820') %>%
        group_by(Stage, Treatment) %>%
        summarise(ICMcells = n())
table.n.figure1 <- merge(FGF.lm.embryos, FGF.lm.ICMcells)
table.n.figure1 <- table.n.figure1[order(table.n.figure1$Stage),]
rm(FGF.lm.embryos, FGF.lm.ICMcells)

## Table for N numbers in Figure 2b & S3b
FGF.F2.icm <- FGF.all %>%
        ## Select treatments, treatment length and regimes as appropriate
        ## select ICM cells of embryos labelled with CDX2, GATA6 and NANOG
        filter(Treatment == 'Control' | 
                       Treatment == 'FGF4_1000' | 
                       Treatment == 'PD03_1' | 
                       Treatment == 'AZD_1' | 
                       Treatment == 'SU_20',
               Xpoint != 'xp', 
               TE_ICM == 'ICM', 
               Markers == 'C2G6NG', 
               Regime != 'R8', 
               Regime != 'R9') %>% 
        group_by(Regime, Treatment) %>% summarise(ICMcells = n())
FGF.F2.emb <- FGF.all %>% 
        ## Select treatments, treatment length and regimes as appropriate
        ## select embryos labelled with CDX2, GATA6 and NANOG
        filter(Treatment == 'Control' | 
                       Treatment == 'FGF4_1000' | 
                       Treatment == 'PD03_1' | 
                       Treatment == 'AZD_1' | 
                       Treatment == 'SU_20',
               Xpoint != 'xp', 
               TE_ICM == 'ICM', 
               Markers == 'C2G6NG', 
               Regime != 'R8', 
               Regime != 'R9') %>% 
        group_by(Regime, Treatment, Embryo_ID) %>% 
        summarise() %>% group_by(Regime, Treatment) %>%
        summarise(N = n())
## Combine both tables into one
table.n.figure2 <- merge(FGF.F2.icm, FGF.F2.emb)
rm(FGF.F2.icm, FGF.F2.emb)

## Table for N numbers in Figure 3
O4.icm <- FGF.all %>% 
        ## Select ICM cells of embryos labeled for OCT4, GATA4 and NANOG
        filter(Markers == 'O4G4NG', 
               TE_ICM == 'ICM') %>% 
        group_by(Treatment) %>% summarise(ICMcells = n())
O4.emb <- FGF.all %>% 
        ## Select embryos labeled for OCT4, GATA4 and NANOG
        filter(Markers == 'O4G4NG') %>% 
        group_by(Treatment, Embryo_ID) %>% 
        summarise() %>% group_by(Treatment) %>% 
        summarise(N = n())
table.n.figure3 <- merge(O4.icm, O4.emb)
rm(O4.icm, O4.emb)

## Table for N numbers in Figure 4
table.n.figure4 <- scaling %>% 
        group_by(Treatment, Embryo_ID) %>% 
        summarise() %>% 
        group_by(Treatment) %>% 
        summarise(N.embryos = n())

## Table for N numbers in Figures 5 and S7
FGF.F5.icm <- FGF.all %>%
        ## Select ICM cells of embryos for Regimes 8 and 9
        ## treated for 24h or 30h in FGF4 or PD03
        ## and then released for a further 24h or 18h
        ## in control media or PD03 (after FGF4 only)
        filter(Regime == 'R8' | 
                       Regime == 'R9', 
               TE_ICM == 'ICM') %>% 
        group_by(Regime, Treatment, Xpoint) %>% 
        summarise(ICMcells = n())
FGF.F5.emb <- FGF.all %>% 
        ## Select embryos for Regimes 8 and 9
        ## treated for 24h or 30h in FGF4 or PD03
        ## and then released for a further 24h or 18h
        ## in control media or PD03 (after FGF4 only)
        filter(Regime == 'R8' | 
                       Regime == 'R9') %>% 
        group_by(Embryo_ID, Regime, Treatment, Xpoint) %>% 
        summarise() %>% 
        group_by(Regime, Treatment, Xpoint) %>% 
        summarise(N = n())
table.n.figure5 <- merge(FGF.F5.icm, FGF.F5.emb)
rm(FGF.F5.emb, FGF.F5.icm)

# ------------------------------------------------------------------------------

# Tables used to generate plots

## Count TE and ICM cells for each embryo
## and calculate absolute count and % of total they represent
FGF.sum <- FGF.all %>% 
        ## Exclude experiment from August 20 2015
        filter(Exp_date != '20150820') %>%
        group_by(Embryo_ID, Experiment, Experimenter, 
                 Regime, Stage, Treatment, Tt_length, 
                 Markers, Xpoint, Cellcount, TE_ICM) %>%
        summarise(Count = n()) %>%
        mutate(pcTotal = Count / Cellcount * 100)

## Count ICM cells only for each embryo 
## calculate count for each identity
## and % of ICM for each identity
## and calculate average levels for each channel
FGF.ICMsum <- FGF.all %>%
        ## Select ICM cells only, for all experiments
        ## Exclude experiment from August 20 2015
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
## Extract the number of ICM cells per embryo from FGF.sum
ICMcounts <- FGF.sum %>% filter(TE_ICM == 'ICM') %>%
        group_by(Embryo_ID, Count) %>%
        summarize()
ICMcounts <- rename(ICMcounts, ICM.count = Count)
## Add ICM cell number (ICM.count) to FGF.ICMsum
FGF.ICMsum <- merge(FGF.ICMsum, ICMcounts)
rm(ICMcounts)

## Build equivalent table to FGF.ICMsum, 
## only for embryos stained with OCT4, GATA4 and NANOG
FGF.ICMsumO4 <- FGF.all %>%
        ## Select ICM cells only
        ## for all embryos stained for OCT4, GATA4 and NANOG
        filter(TE_ICM == 'ICM', 
               Markers == 'O4G4NG') %>%
        group_by(Embryo_ID, Experiment, Regime, 
                 Stage, Treatment, Tt_length, 
                 Cellcount, Identity, Xpoint, Markers) %>%
        summarise(Count = n(), 
                  CH1 = mean(CH1.ebLogCor, na.rm = TRUE),
                  CH2 = mean(CH2.logCor, na.rm = TRUE),
                  CH4 = mean(CH4.ebLogCor, na.rm = TRUE), 
                  CH5 = mean(CH5.ebLogCor, na.rm = TRUE))

## Tables for scaling experiments (Figures 4 & S6a)
scal.counts <- scaling %>% 
        group_by(Embryo_ID, Treatment, Cellcount, Regime) %>% 
        summarise()
scal.teicm <- scaling %>%
        group_by(Embryo_ID, Treatment, Cellcount, TE_ICM, Regime) %>% 
        summarise(Count = n()) %>% 
        mutate(pcTotal = Count / Cellcount * 100)

## Count cells in each ICM lineage for scaling experiments (Figures 4 and S6c)
## Equivalent to FGF.ICMsum
scal.ICMsum <- scaling %>% 
        group_by(Embryo_ID, Experiment, Regime, 
                 Treatment, Tt_length, 
                 Cellcount, Identity.km, Xpoint, 
                 Markers) %>% 
        summarise(Count = n())
## Extract the number of ICM cells per embryo from scal.teicm
ICMcounts <- scal.teicm %>% filter(TE_ICM == 'ICM') %>%
        group_by(Embryo_ID, Count) %>% 
        summarize()
ICMcounts <- rename(ICMcounts, ICM.count = Count)
## Add ICM cell number (ICM.count) to scal.ICMsum
scal.ICMsum <- merge(scal.ICMsum, ICMcounts)
rm(ICMcounts)
