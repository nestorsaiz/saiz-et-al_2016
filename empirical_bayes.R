# This script performs an Empirical Bayes correction of fluorescence values
# on a per-embryo basis basis to compensate for the decay along the Z axis

## Load data and apply transformations if not loaded yet
data.ok <- exists('FGF.all') 
if (data.ok == FALSE) {
        source('Transformations.R')
}
rm(data.ok)

## eb.cor is the function that performs the Empirical Bayes correction of fluorescence values
## on a per-embryo basis basis to compensate for the decay along the Z axis
## The data needs to have the fluorescence values in variables named 'CHn.Avg',
## where n is 1-5, as per MINS output 
## and the embryo IDs in a variable named 'Embryo_ID'
## For the time being it works if GATA6 is in Channel 4 and NANOG in Channel 5
eb.cor <- function(dataset) {
        ## Write vector with all embryo IDs
        embryos <- unique(dataset$Embryo_ID)
        ## Calculate regression coefficients by embryo
        coefs <- matrix(0, length(embryos), 6)
        for(i in 1:length(embryos)) {
                xxi <- dataset[dataset$Embryo_ID == embryos[i],]
                coefs[i,1:2] <- summary(lm(log(CH1.Avg+0.0001) ~ Z + (Identity=="TE"), 
                                           data=xxi))$coefficients[2,1:2]
                coefs[i,3:4] <- summary(lm(log(CH4.Avg+0.0001) ~ Z + (Identity=="TE"), 
                                           data=xxi))$coefficients[2,1:2]
                coefs[i,5:6] <- summary(lm(log(CH5.Avg+0.0001) ~ Z + (Identity=="TE"), 
                                           data=xxi))$coefficients[2,1:2]
        }
        ## Check how many values Regime and Marker variables take 
        mk.length <- length(unique(dataset$Markers))
        rg.length <- length(unique(dataset$Regime))
        ## If Regime and/or Marker are present (length > 0)
        ## and either takes more than one value (length > 1)
        if (mk.length > 1 | rg.length > 1) {
                ## Establish the combinations of Markers and Regime in the table
                markers <- tapply(dataset$Markers, dataset$Embryo_ID, function(x) {unique(x)[1]})
                regimes <- tapply(dataset$Regime, dataset$Embryo_ID, function(x) {unique(x)[1]})
                regmark <- 10*markers + regimes
                regmark[is.na(regmark)] <- 0
                ## Empirical Bayes (EB) by regime-marker combination
                ebcoefs1 <- ebcoefs2 <- ebcoefs3 <- rep(0, length(embryos))
                for(i in unique(regmark)) {
                        ebcoefs1[regmark==i] <- mean(coefs[regmark==i,1]) + 
                                (1 - coefs[regmark==i,2]^2/(coefs[regmark==i,2]^2 + 
                                                                    var(coefs[regmark==i,1]))) *
                                (coefs[regmark==i,1] - mean(coefs[regmark==i,1]))
                        ebcoefs2[regmark==i] <- mean(coefs[regmark==i,3]) + 
                                (1 - coefs[regmark==i,4]^2/(coefs[regmark==i,4]^2 + 
                                                                    var(coefs[regmark==i,3]))) *
                                (coefs[regmark==i,3] - mean(coefs[regmark==i,3]))
                        ebcoefs3[regmark==i] <- mean(coefs[regmark==i,5]) + 
                                (1 - coefs[regmark==i,6]^2/(coefs[regmark==i,6]^2 + 
                                                                    var(coefs[regmark==i,5]))) *
                                (coefs[regmark==i,5] - mean(coefs[regmark==i,5]))
                } 
        } 
        ## Else, if they are not present or take only one value
        else {
                # EB shrunk coefficients
                ebcoefs1 <- mean(coefs[,1]) + 
                        (1 - coefs[,2]^2/(coefs[,2]^2 + var(coefs[,1]))) * 
                        (coefs[,1] - mean(coefs[,1]))
                ebcoefs2 <- mean(coefs[,3]) + 
                        (1 - coefs[,4]^2/(coefs[,4]^2 + var(coefs[,3]))) * 
                        (coefs[,3] - mean(coefs[,3]))
                ebcoefs3 <- mean(coefs[,5]) + 
                        (1 - coefs[,6]^2/(coefs[,6]^2 + var(coefs[,5]))) *
                        (coefs[,5] - mean(coefs[,5])) 
        }
        ## Write EB corrected log intensities
        dataset$CH1.ebLogCor <- rep(NA, nrow(dataset))
        dataset$CH4.ebLogCor <- rep(NA, nrow(dataset))
        dataset$CH5.ebLogCor <- rep(NA, nrow(dataset))
        for(i in 1:length(embryos)) {
                ii <- dataset$Embryo_ID == embryos[i]
                dataset$CH1.ebLogCor[ii] <- 
                        log(0.0001+dataset$CH1.Avg[ii]) - 
                        ebcoefs1[i] * dataset$Z[ii]
                dataset$CH4.ebLogCor[ii] <- 
                        log(0.0001+dataset$CH4.Avg[ii]) - 
                        ebcoefs2[i] * dataset$Z[ii]
                dataset$CH5.ebLogCor[ii] <- 
                        log(0.0001+dataset$CH5.Avg[ii]) - 
                        ebcoefs3[i] * dataset$Z[ii]
        }
        return(dataset)
}