# This script performs an Empirical Bayes correction of fluorescence values
# on a per-embryo basis basis to compensate for the decay along the Z axis

## Load data and apply transformations if not loaded yet
data.ok <- exists('FGF.all') 
if (data.ok == FALSE) {
        source('Transformations.R')
}
rm(data.ok)

# ------------------------------------------------------------------------------

# Correction of embryos cultured and treated with FGF4/MEKi/FGFRi and Littermates

## Write vector with all embryo IDs
embryos <- unique(FGF.all$Embryo_ID)

## Calculate regression coefficients by embryo
coefs <- matrix(0, length(embryos), 6)
for(i in 1:length(embryos)) {
        xxi <- FGF.all[FGF.all$Embryo_ID==embryos[i],]
        coefs[i,1:2] <- summary(lm(log(CH1.Avg+0.0001) ~ Z + (Identity=="TE"), 
                                   data=xxi))$coefficients[2,1:2]
        coefs[i,3:4] <- summary(lm(log(CH4.Avg+0.0001) ~ Z + (Identity=="TE"), 
                                   data=xxi))$coefficients[2,1:2]
        coefs[i,5:6] <- summary(lm(log(CH5.Avg+0.0001) ~ Z + (Identity=="TE"), 
                                   data=xxi))$coefficients[2,1:2]
}

## Establish the combinations of Markers and Regime in the table
markers <- tapply(FGF.all$Markers, FGF.all$Embryo_ID, function(x) {unique(x)[1]})
regimes <- tapply(FGF.all$Regime, FGF.all$Embryo_ID, function(x) {unique(x)[1]})
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

## Write EB corrected log intensities
FGF.all$CH1.ebLogCor <- rep(NA, nrow(FGF.all))
FGF.all$CH4.ebLogCor <- rep(NA, nrow(FGF.all))
FGF.all$CH5.ebLogCor <- rep(NA, nrow(FGF.all))

for(i in 1:length(embryos)) {
        ii <- FGF.all$Embryo_ID==embryos[i]
        FGF.all$CH1.ebLogCor[ii] <- log(0.0001+FGF.all$CH1.Avg[ii]) - 
                ebcoefs1[i]*FGF.all$Z[ii]
        FGF.all$CH4.ebLogCor[ii] <- log(0.0001+FGF.all$CH4.Avg[ii]) - 
                ebcoefs2[i]*FGF.all$Z[ii]
        FGF.all$CH5.ebLogCor[ii] <- log(0.0001+FGF.all$CH5.Avg[ii]) - 
                ebcoefs3[i]*FGF.all$Z[ii]
}

# ------------------------------------------------------------------------------

# Correction of scaling data (double and half embryos) using same approach

## Write vector with all embryo IDs
s.embryos <- unique(scaling$Embryo_ID)

## Calculate regression coefficients by embryo
coefs <- matrix(0, length(s.embryos), 6)
for(i in 1:length(s.embryos)) {
        xxi <- scaling[scaling$Embryo_ID==s.embryos[i],]
        coefs[i,1:2] <- summary(lm(log(CH1.Avg+0.0001) ~ Z + 
                                           (Identity=="TE"), data=xxi))$coefficients[2,1:2]
        coefs[i,3:4] <- summary(lm(log(CH4.Avg+0.0001) ~ Z + 
                                           (Identity=="TE"), data=xxi))$coefficients[2,1:2]
        coefs[i,5:6] <- summary(lm(log(CH5.Avg+0.0001) ~ Z + 
                                           (Identity=="TE"), data=xxi))$coefficients[2,1:2]
}

# EB shrunk coefficients
ebcoefs1 <- mean(coefs[,1]) + (1 - coefs[,2]^2/(coefs[,2]^2 + var(coefs[,1]))) * 
        (coefs[,1] - mean(coefs[,1]))
ebcoefs2 <- mean(coefs[,3]) + (1 - coefs[,4]^2/(coefs[,4]^2 + var(coefs[,3]))) * 
        (coefs[,3] - mean(coefs[,3]))
ebcoefs3 <- mean(coefs[,5]) + (1 - coefs[,6]^2/(coefs[,6]^2 + var(coefs[,5]))) *
        (coefs[,5] - mean(coefs[,5]))

# EB corrected log intensities
scaling$CH1.ebLogCor <- rep(NA, nrow(scaling))
scaling$CH4.ebLogCor <- rep(NA, nrow(scaling))
scaling$CH5.ebLogCor <- rep(NA, nrow(scaling))

for(i in 1:length(s.embryos)) {
        ii <- scaling$Embryo_ID==s.embryos[i]
        scaling$CH1.ebLogCor[ii] <- log(0.0001+scaling$CH1.Avg[ii]) - 
                ebcoefs1[i]*scaling$Z[ii]
        scaling$CH4.ebLogCor[ii] <- log(0.0001+scaling$CH4.Avg[ii]) - 
                ebcoefs2[i]*scaling$Z[ii]
        scaling$CH5.ebLogCor[ii] <- log(0.0001+scaling$CH5.Avg[ii]) - 
                ebcoefs3[i]*scaling$Z[ii]
}