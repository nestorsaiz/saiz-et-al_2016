# Load dplyr
library(dplyr)

# Generate experimental log
# from raw table
# with analyzed embryos and experiments
FGF.log <- FGF.all %>% 
        group_by(Experiment, 
                 Embryo_ID, 
                 Cellcount, 
                 Experimenter, 
                 Regime, 
                 Treatment, 
                 Tt_length) %>%
        summarize(Cells = n()) %>%
        group_by(Experiment, 
                 Experimenter, 
                 Regime, 
                 Treatment, 
                 Tt_length) %>%
        summarize(Embryos = n(), 
                  Avg.count = mean(Cellcount))
# Write 'Experimental_log.csv' file
write.csv(FGF.log, file = 'Experimental_log.csv', row.names = FALSE)

