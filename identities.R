## This script assigns identities to individual cells 
## using two alternative approaches with similar results

## Load data and apply transformations if not loaded yet
data.ok <- exists('FGF.all') 
if (data.ok == FALSE) {
        source('Transformations.R')
}
rm(data.ok)

# ------------------------------------------------------------------------------

# Asign identities to embryos cultured and treated with FGF4/MEKi/FGFRi (+ Littermates)

## Assign identities using a thresholding method (threshold = 50)
## for GATA6 (Channel 4) and NANOG (Channel 5) levels
## Identities thus assigned will appear in Identity.lin column
## Only reliable for embryos where Treatment == 'Littermate'
FGF.all <- id.linear(FGF.all)

## Assign identity to ICM cells
## using a k-means clustering approach based on GATA6 and NANOG expression
## Using only embryos stained for CDX2, GATA6 and NANOG

## Scan experimental dates
FGF.all$Embryo_ID <- as.character(FGF.all$Embryo_ID)
edates <- sapply(FGF.all$Embryo_ID, function(x) {strsplit(x, "_")[[1]][1]})

## Keep embryos where Markers == C2G6NG (CDX2, GATA6, NANOG)
## and remove experiments with edate 082015 (see Figure S2 and legend)
jj <- FGF.all$Markers == "C2G6NG" & edates != "082015"
## Subset FGF.all to jj=TRUE
xx0 <- FGF.all[jj,]

## k-means of these embryos using Channel4 (GATA6), and Channel5 (NANOG) data 
## for Littermate embryos only (fixed upon collection, Figure 1 & S1-2)
xx2 <-  subset(xx0, Treatment == "Littermate" & 
                       ((Cellcount >= 32 & Cellcount < 64) | 
                                (Cellcount >= 120 & Cellcount < 150)) & 
                       (Identity != "TE"), 
               select = c(CH4.ebLogCor, CH5.ebLogCor, Cellcount))

## Perform k-means clustering with 3 means
set.seed(20160606)
oo <- kmeans(xx2[,1:2], 3)
## oo$centers should look like this:
#  CH4.ebLogCor CH5.ebLogCor
#1     4.809746     5.693890
#2     5.075773     2.096386
#3     2.302035     5.415014

## Assign identity to all cells based on the centers obtained
centers <- rbind(oo$centers, c(2.302035, 2.096386))
ii <- xx0$Identity != "TE"

dkm <- matrix(0, sum(ii), 4)
for(i in 1:4) dkm[,i] <- (xx0$CH4.ebLogCor[ii] - centers[i,1])^2 + 
        (xx0$CH5.ebLogCor[ii] - centers[i,2])^2

idkm <- apply(dkm, 1, which.min)

xx0$Identity.km <- rep(NA, nrow(xx0))
xx0$Identity.km[!ii] <- "TE"
xx0$Identity.km[ii] <- c("DP", "PRE", "EPI", "DN")[idkm]


## Littermates (Figure 1) and embryos cultured for 48h (Figures 2, 5)
## have different scales because of different acquisition parameters

## Thus, assign identities to cultured embryos using the same k-means approach
## but where cluster centers are calculated based 
## on the data distribution of GATA6 and NANOG levels of cells in Control embryos 
xx2 <- subset(xx0, Treatment == "Control" & 
                      (Identity != "TE"), 
              select = c(CH4.ebLogCor, CH5.ebLogCor))
set.seed(20160609)
oo <- kmeans(xx2[,1:2], 3)
#> oo$centers
#  CH4.ebLogCor CH5.ebLogCor
#1     3.716422     3.455725
#2     4.719526     5.860470
#3     5.718721     3.355891

centers <- rbind(c(5.718721, 5.860470), oo$centers)
ii <- xx0$Identity != "TE" & xx0$Treatment != "Littermate" & xx0$Xpoint != "xp"

dkm <- matrix(0, sum(ii), 4)
for(i in 1:4) dkm[,i] <- (xx0$CH4.ebLogCor[ii] - centers[i,1])^2 + 
        (xx0$CH5.ebLogCor[ii] - centers[i,2])^2

idkm <- apply(dkm, 1, which.min)
xx0$Identity.km[ii] <- c("DP", "DN", "EPI", "PRE")[idkm]

## Integrate the CDX2, GATA6, NANOG subset (xx0) with rest of data
FGF.all <- rbind.fill(xx0, subset(FGF.all, Markers != 'C2G6NG' | 
                                          Exp_date == '20150820'))

# ------------------------------------------------------------------------------

# Asign identities to half and double embryos (scaling data) using same approach

## Control embryos belong to the FGF.all dataset
## however, for Double, Half and Single embryos 
## we detected a shift in expression levels
## possibly due to these experiments being done months later 
## (although acquired in the same system, with same parameters)

## Correct this shift by median centering the controls and the rest by imaging date
## Extract imaging dates (Img_date) from scaling data
uidates <- unique(scaling$Img_date)

## Calculate median of controls for channels 4 & 5 (GATA6 and NANOG) in ICM cells
ii <- scaling$TE_ICM == "ICM"
cmed4 <- quantile(scaling$CH4.ebLogCor[ii & scaling$Treatment == "Control"], 3/4)
cmed5 <- quantile(scaling$CH5.ebLogCor[ii & scaling$Treatment == "Control"], 3/4)

for(i in uidates) {
        scaling$CH4.ebLogCor[scaling$Img_date == i] <- 
                scaling$CH4.ebLogCor[scaling$Img_date == i] + 
                cmed4 - quantile(scaling$CH4.ebLogCor[ii & scaling$Img_date == i],3/4)
        scaling$CH5.ebLogCor[scaling$Img_date == i] <- 
                scaling$CH5.ebLogCor[scaling$Img_date == i] + 
                cmed5 - quantile(scaling$CH5.ebLogCor[ii & scaling$Img_date == i],3/4)
}

# Set two centers
set.seed(20160610)
oo <- kmeans(scaling[ii, 32:33], 2)
oo$centers
#  CH4.ebLogCor CH5.ebLogCor
#1     5.764162     3.608910
#2     4.060173     5.884474

ii <- scaling$TE_ICM == "ICM"
centers <- rbind(c(5.764162, 5.884474), oo$centers[2:1,], c(4.060173,3.608910))
#centers <- matrix(c(6,6,3.5,6,6,3.5,3.5,3.5), ncol=2, byrow=TRUE)

dkm <- matrix(0, sum(ii), 4)
for(i in 1:4) dkm[,i] <- (scaling$CH4.ebLogCor[ii] - centers[i,1])^2 + 
        (scaling$CH5.ebLogCor[ii] - centers[i,2])^2

idkm <- apply(dkm, 1, which.min)

scaling$Identity.km <- scaling$Identity
scaling$Identity.km[ii] <- c("DP", "PRE", "EPI", "DN")[idkm]
