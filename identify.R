# identify <- function(dataframe) {
#         ## Calculate the ratio of log(GATA6) to log(NANOG) levels 
#         dataframe$G6toNG <- dataframe$CH4.ebLogCor - dataframe$CH5.ebLogCor
#         ## Create a vector to store the automatically assigned identity
#         dataframe$Identity.auto <- 'NA'
#         ## Assign identity to untreated embryos ('littermates')
#         dataframe$Identity.auto <- ifelse(dataframe$Treatment == 'Littermate', 
#                                           ifelse(dataframe$Identity == 'TE', 
#                                                  'TE', 
#                                                  ifelse((dataframe$CH4.ebLogCor < 2.5 &
#                                                                  dataframe$CH5.ebLogCor < 2.5), 
#                                                         'DN', 
#                                                         ifelse(dataframe$G6toNG > 0.5, 'PRE', 
#                                                                ifelse(dataframe$G6toNG < -1.5, 'EPI', 'DP')))), 
#                                           ## Assign identity to treated embryos (Regimes 1-9)
#                                           ifelse(dataframe$Identity == 'TE', 'TE',
#                                                  ifelse((dataframe$CH4.ebLogCor < 4 &
#                                                                  dataframe$CH5.ebLogCor < 4), 
#                                                         'DN', 
#                                                         ifelse(dataframe$G6toNG > 0.5, 'PRE', 
#                                                                ifelse(dataframe$G6toNG < -0.5, 'EPI', 'DP')))))
#         # Make Identity.auto a factor
#         dataframe$Identity.auto <- as.factor(dataframe$Identity.auto)
#         return(dataframe)
# }

id.linear <- function(dataframe) {
        ## Create a vector to store the automatically assigned identity
        dataframe$Identity.lin <- 'NA'
        ## Assign identity to untreated embryos ('littermates')
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
        # Make Identity.auto a factor
        dataframe$Identity.lin <- factor(dataframe$Identity.lin, 
                                         levels = c('DN', 'EPI', 'DP', 'PRE', 'ICM', 'TE'))
        return(dataframe)
}