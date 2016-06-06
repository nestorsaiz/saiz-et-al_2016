library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
explore <- function(dataset, channel = 2) { 
        # Eliminate unnecessary variables
        dataset$CH1.Sum <- NULL
        dataset$CH2.Sum <- NULL
        dataset$CH3.Sum <- NULL
        dataset$CH4.Sum <- NULL
        dataset$CH5.Sum <- NULL
        # Make a list of possible channels
        Channels <- c('CH1.Avg', 'CH2.Avg', 'CH3.Avg', 'CH4.Avg', 'CH5.Avg')
        # Melt dataset to 'skinny' format with one column for Channel (1-5) 
        # and one for the corresponding level
        keepers <- colnames(dataset)[!colnames(dataset) %in% Channels]
        dataset <- melt(dataset, id.vars = keepers, 
                        variable.name = 'Channel', value.name = 'Level')
        # Check if channel entered is numeric and within the list (1-5)
        channel.ok <- is.numeric(channel) & channel <= length(Channels)
        if (channel.ok == FALSE) {
                stop('Incorrect channel. Enter only numbers 1-5 for channel')
        }
        # Subset the dataset to only values for the channel queried
        dataset <- subset(dataset, Channel == Channels[channel])
        dataset$Level <- dataset$Level + 0.0001
        # Create vector with colors for each identity
        idcols <<- c('EPI' = 'red', 'PRE' = 'blue', 'DP' = 'purple', 
                    'DN' = 'gray', 'TE' = 'green', 'ICM' = 'purple', 'ESC' = 'red4')
        # Plot log(Levels) over Z and color-code for identity
        raw <- ggplot(dataset, aes(x = Z, y = log(Level)))
        raw <- raw + geom_point(aes(color = Identity), size = I(1))
        # Add lines with the slope to see the effect of the correction
        raw <- raw + geom_smooth(data = subset(dataset, Identity == 'TE'), 
                                 method = 'lm', aes(color = Identity))
        raw <- raw + geom_smooth(data = subset(dataset, Identity == 'TE'), 
                                 method = 'loess')
        raw <- raw + geom_smooth(data = subset(dataset, Identity == 'PRE'), 
                                 method = 'lm', aes(color = Identity))
        raw <- raw + geom_smooth(data = subset(dataset, Identity == 'EPI'), 
                                 method = 'lm', aes(color = Identity))
        raw <- raw + labs(title = 'Raw log(data)') + theme_bw()
        raw <- raw + scale_color_manual(values = idcols)
        # Calculate slope of single model for all data points
        # And write it to memory (won't work otherwise, why?)
        all.model <<- abs(coef(lm(log(Level)~Z, data = dataset))['Z'])
        # Plot log(Levels) corrected with single model over Z
        # and color-code for identity
        single <- ggplot(dataset, aes(x = Z, y = (log(Level) + Z * all.model)))
        single <- single + geom_point(aes(color = Identity), size = I(1))
        # Add lines with the slope to see the effect of the correction
        single <- single + geom_smooth(data = subset(dataset, Identity == 'TE'), 
                                 method = 'lm', aes(color = Identity))
        single <- single + geom_smooth(data = subset(dataset, Identity == 'PRE'), 
                                 method = 'lm', aes(color = Identity))
        single <- single + geom_smooth(data = subset(dataset, Identity == 'EPI'), 
                                 method = 'lm', aes(color = Identity))
        single <- single + labs(title = 'Single lm') + theme_bw()
        single <- single + scale_color_manual(values = idcols)
        # Calculate slope of model using TE cells only
        te.model <<- abs(coef(lm(log(Level)~Z, 
                                 data = subset(dataset, Identity == 'TE')))['Z'])
        # Calculate slope of model using non-TE (ICM) cells only
        icm.model <<- abs(coef(lm(log(Level)~Z, 
                                  data = subset(dataset, Identity != 'TE')))['Z'])
        # Correct values for TE and ICM separately with each slope
        dataset$Level.logCor <- ifelse(dataset$Identity == 'TE', 
                                     log(dataset$Level) + dataset$Z * te.model, 
                                     log(dataset$Level) + dataset$Z * icm.model)
        # Plot log(Levels) corrected separately for TE and ICM over Z
        # and color-code for identity
        separate <- ggplot(dataset, aes(x = Z, y = Level.logCor))
        separate <- separate + geom_point(aes(color = Identity), size = I(1))
        # Add lines with the slope to see the effect of the correction
        separate <- separate + geom_smooth(data = subset(dataset, Identity == 'TE'), 
                                       method = 'lm', aes(color = Identity))
        separate <- separate + geom_smooth(data = subset(dataset, Identity == 'PRE'), 
                                       method = 'lm', aes(color = Identity))
        separate <- separate + geom_smooth(data = subset(dataset, Identity == 'EPI'), 
                                       method = 'lm', aes(color = Identity))
        separate <- separate + labs(title = 'Separate TE and ICM models') + theme_bw()
        separate <- separate + scale_color_manual(values = idcols)
        # Print plots
        print(raw)
        print(single)
        print(separate)
}

# Same as explore but will facet graphs per stage
explore.s <- function(dataset, channel = 2) { 
        # Eliminate unnecessary variables
        dataset$CH1.Sum <- NULL
        dataset$CH2.Sum <- NULL
        dataset$CH3.Sum <- NULL
        dataset$CH4.Sum <- NULL
        dataset$CH5.Sum <- NULL
        # Make a list of possible channels
        Channels <- c('CH1.Avg', 'CH2.Avg', 'CH3.Avg', 'CH4.Avg', 'CH5.Avg')
        # Melt dataset to 'skinny' format with one column for Channel (1-5) 
        # and one for the corresponding level
        keepers <- colnames(dataset)[!colnames(dataset) %in% Channels]
        dataset <- melt(dataset, id.vars = keepers, 
                        variable.name = 'Channel', value.name = 'Level')
        # Check if channel entered is numeric and within the list (1-5)
        channel.ok <- is.numeric(channel) & channel <= length(Channels)
        if (channel.ok == FALSE) {
                stop('Incorrect channel. Enter only numbers 1-5 for channel')
        }
        # Subset the dataset to only values for the channel queried
        dataset <- subset(dataset, Channel == Channels[channel])
        dataset$Level <- dataset$Level + 0.0001
        # Create vector with colors for each identity
        idcols <<- c('EPI' = 'red', 'PRE' = 'blue', 'DP' = 'purple', 
                     'DN' = 'gray', 'TE' = 'green')
        # Plot log(Levels) over Z and color-code for identity
        raw <- ggplot(dataset, aes(x = Z, y = log(Level)))
        raw <- raw + geom_point(aes(color = Identity), size = I(1))
        # Add lines with the slope to see the effect of the correction
        raw <- raw + geom_smooth(data = subset(dataset, Identity == 'TE'), 
                                 method = 'lm', aes(color = Identity))
        raw <- raw + geom_smooth(data = subset(dataset, Identity == 'TE'), 
                                 method = 'loess')
        raw <- raw + geom_smooth(data = subset(dataset, Identity == 'PRE'), 
                                 method = 'lm', aes(color = Identity))
        raw <- raw + geom_smooth(data = subset(dataset, Identity == 'EPI'), 
                                 method = 'lm', aes(color = Identity))
        raw <- raw + labs(title = 'Raw log(data)') + theme_bw()
        raw <- raw + scale_color_manual(values = idcols) 
        raw <- raw + facet_wrap( ~ Stage)
        # Calculate slope of single model for all data points
        # And write it to memory (won't work otherwise, why?)
        all.model <<- abs(coef(lm(log(Level)~Z, data = dataset))['Z'])
        # Plot log(Levels) corrected with single model over Z
        # and color-code for identity
        single <- ggplot(dataset, aes(x = Z, y = (log(Level) + Z * all.model)))
        single <- single + geom_point(aes(color = Identity), size = I(1))
        # Add lines with the slope to see the effect of the correction
        single <- single + geom_smooth(data = subset(dataset, Identity == 'TE'), 
                                       method = 'lm', aes(color = Identity))
        single <- single + geom_smooth(data = subset(dataset, Identity == 'PRE'), 
                                       method = 'lm', aes(color = Identity))
        single <- single + geom_smooth(data = subset(dataset, Identity == 'EPI'), 
                                       method = 'lm', aes(color = Identity))
        single <- single + labs(title = 'Single lm') + theme_bw()
        single <- single + scale_color_manual(values = idcols)
        single <- single + facet_wrap( ~ Stage)
        # Calculate slope of model using TE cells only
        te.model <<- abs(coef(lm(log(Level)~Z, 
                                 data = subset(dataset, Identity == 'TE')))['Z'])
        # Calculate slope of model using non-TE (ICM) cells only
        icm.model <<- abs(coef(lm(log(Level)~Z, 
                                  data = subset(dataset, Identity != 'TE')))['Z'])
        # Correct values for TE and ICM separately with each slope
        dataset$Level.logCor <- ifelse(dataset$Identity == 'TE', 
                                       log(dataset$Level) + dataset$Z * te.model, 
                                       log(dataset$Level) + dataset$Z * icm.model)
        # Plot log(Levels) corrected separately for TE and ICM over Z
        # and color-code for identity
        separate <- ggplot(dataset, aes(x = Z, y = Level.logCor))
        separate <- separate + geom_point(aes(color = Identity), size = I(1))
        # Add lines with the slope to see the effect of the correction
        separate <- separate + geom_smooth(data = subset(dataset, Identity == 'TE'), 
                                           method = 'lm', aes(color = Identity))
        separate <- separate + geom_smooth(data = subset(dataset, Identity == 'PRE'), 
                                           method = 'lm', aes(color = Identity))
        separate <- separate + geom_smooth(data = subset(dataset, Identity == 'EPI'), 
                                           method = 'lm', aes(color = Identity))
        separate <- separate + labs(title = 'Separate TE and ICM models') + theme_bw()
        separate <- separate + scale_color_manual(values = idcols)
        separate <- separate + facet_wrap( ~ Stage)
        # Print plots
        print(raw)
        print(single)
        print(separate)
}