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