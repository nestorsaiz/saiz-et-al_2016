stage <- function(dataset){
        # Given a certain dataset, evaluate the cell count of each embryo
        # and assign values to a new 'Stage' columns accordingly
        # >= 120 cells, Stage = '>120'; 
        # <120 cells & >= 90 cells, Stage = '90-120'; 
        # etc
        dataset$Stage <- ifelse(dataset$Cellcount >= 150, 
                                '>150',
                                ifelse(dataset$Cellcount >= 120 &
                                               dataset$Cellcount < 150, 
                                       '120_150', 
                                       ifelse(dataset$Cellcount >= 90 & 
                                                      dataset$Cellcount < 120, 
                                              '90_120',
                                              ifelse(dataset$Cellcount < 90 & 
                                                             dataset$Cellcount >= 64, 
                                                     '64_90',
                                                     ifelse(dataset$Cellcount < 64 & 
                                                                    dataset$Cellcount >= 32,
                                                            '32_64', '<32')))))
        # Convert 'Stage' into a factor with the levels ordered
        # in increasing number of cells
        dataset$Stage <- factor(dataset$Stage, levels = c('<32', '32_64', 
                                                          '64_90', '90_120', 
                                                          '120_150', '>150'))
        return(dataset)
}

stage2 <- function(dataset){
        # Given a certain dataset, evaluate the cell count of each embryo
        # and assign values to a new 'S2' columns accordingly
        # etc
        dataset$S2 <- ifelse(dataset$Cellcount >= 150, 
                             '150', 
                             ifelse(dataset$Cellcount >= 130 & 
                                            dataset$Cellcount < 150, 
                                    '130_150', 
                                    ifelse(dataset$Cellcount >= 110 &
                                                   dataset$Cellcount < 130, 
                                           '110_130',
                                           ifelse(dataset$Cellcount >= 90 &
                                                          dataset$Cellcount < 110, 
                                                  '90_110', 
                                                  ifelse(dataset$Cellcount >= 70 & 
                                                                 dataset$Cellcount < 90, 
                                                         '70_90',
                                                         ifelse(dataset$Cellcount < 70 & 
                                                                        dataset$Cellcount >= 50, 
                                                                '50_70',
                                                                ifelse(dataset$Cellcount < 50 & 
                                                                               dataset$Cellcount >= 32,
                                                                       '32_50', '<32')))))))
        # Convert 'S2' into a factor with the levels ordered
        # in increasing number of cells
        dataset$S2 <- factor(dataset$S2, levels = c('<32', '32_50', '50_70',  
                                                    '70_90', '90_110', 
                                                    '110_130', '130_150', '>150'))
        return(dataset)
}