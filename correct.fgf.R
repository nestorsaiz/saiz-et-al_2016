correct.fgf <- function(dataset){
        ## Fit linear models (lm) to the log of each channel over Z
        model1 <- lm(log(CH1.Avg + 0.001)~Z, data = dataset)
        model2 <- lm(log(CH2.Avg + 0.001)~Z, 
                     data = subset(dataset, Identity == 'TE'))
        model3 <- lm(log(CH3.Avg + 0.001)~Z, data = dataset)
        model4 <- lm(log(CH4.Avg + 0.001)~Z, 
                     data = subset(dataset, Markers == 'C2G6NG'))
        model4.1 <- lm(log(CH4.Avg + 0.001)~Z, 
                       data = subset(dataset, Markers == 'O4G4NG' & 
                                             Identity == 'PRE'))
        model5 <- lm(log(CH5.Avg + 0.001)~Z, 
                     data = subset(dataset, Identity == 'EPI'))
        ## Extract the coefficients (Intercept and slope(Z)) 
        ## and save them in vectors for each model
        coef1 <- coef(model1)
        coef2 <- coef(model2)
        coef3 <- coef(model3)
        coef4 <- coef(model4)
        coef4.1 <- coef(model4.1)
        coef5 <- coef(model5)
        ## Save the slopes only ('Z') as positive values
        slope1 <- -coef1['Z']
        slope2 <- -coef2['Z']
        slope3 <- -coef3['Z']
        slope4 <- -coef4['Z']
        slope4.1 <- -coef4.1['Z']
        slope5 <- -coef5['Z']
        ## Add a new column to the dataset for each channel
        ## with corrected log values using the corresponding slope
        dataset['CH1.logCor'] <- log(dataset$CH1.Avg) + dataset$Z * slope1
        dataset['CH2.logCor'] <- log(dataset$CH2.Avg) + dataset$Z * slope2
        dataset['CH3.logCor'] <- log(dataset$CH3.Avg) + dataset$Z * slope3
        dataset['CH4.logCor'] <- ifelse(dataset$Markers == 'C2G6NG', 
                                        log(dataset$CH4.Avg) + 
                                                dataset$Z * slope4, 
                                        log(dataset$CH4.Avg) + 
                                                dataset$Z * slope4.1)
        dataset['CH5.logCor'] <- log(dataset$CH5.Avg) + dataset$Z * slope5
        return(dataset)
}