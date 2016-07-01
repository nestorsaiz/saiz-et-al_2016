# This function assigns identities automatically using a thresholding method
# based on the distribution of [GATA6] and [NANOG] values for ICM cells 
# of embryos fixed upon collection ('Littermates'), on linear scale. 
# The threshold determined for these embryos is 50 for both markers
# This threshold WILL NOT apply to cultured embryos (non-Littermates), 
# therefore Identity.lin for non littermates WILL NOT be correct

id.linear <- function(dataframe) {
        ## Create a vector to store the automatically assigned identity
        dataframe$Identity.lin <- 'NA'
        ## Assign identity to embryos based on a fixed threshold
        ## for GATA6 (Channel 4) and NANOG (Channel 5) on linear scale
        ## (after inverting the logarithm, EB-corrected values)
        dataframe$Identity.lin <- ifelse(dataframe$Identity == 'TE', 
                                                'TE', 
                                                ifelse((exp(dataframe$CH4.ebLogCor) < 50 &
                                                                exp(dataframe$CH5.ebLogCor) < 50), 
                                                       'DN', 
                                                       ifelse((exp(dataframe$CH4.ebLogCor) > 50 &
                                                                       exp(dataframe$CH5.ebLogCor) < 50), 
                                                              'PRE', 
                                                              ifelse((exp(dataframe$CH4.ebLogCor) < 50 &
                                                                              exp(dataframe$CH5.ebLogCor) > 50), 
                                                                     'EPI', 'DP'))))
        # Make Identity.lin a factor
        dataframe$Identity.lin <- factor(dataframe$Identity.lin, 
                                         levels = c('DN', 'EPI', 'DP', 'PRE', 'ICM', 'TE'))
        return(dataframe)
}