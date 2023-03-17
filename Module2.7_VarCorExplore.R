##========== Initial exploration of variables =========##

#### Create a new df for exploration
##Select the predictors to explore
var_ex<- c('Updated_Latitude','annual_mean_temp')

##Add the predictors to line-up predictors
var_pd2<- c(var_pd,var_ex)

##Create the df
data_ex<- merge(add[,c("Species3",var_ex)],
                 data, by = "Species3", all.y = TRUE)
data_ex<-data_ex[,var_pd2]

#### Convert character to numeric
##Flight_style_index (Most relevant to AL version)
table(data_ex$Flight_style_index)
data_ex$Flight_style_index[data_ex$Flight_style_index == 'flap'] <- 1 
data_ex$Flight_style_index[data_ex$Flight_style_index == 'soar'] <- 2 
data_ex$Flight_style_index[data_ex$Flight_style_index == 'hover'] <- 3 
##Flight_style_index (Most make-sense version)
data_ex$Flight_style_index[data_ex$Flight_style_index == 'soar'] <- 1 
data_ex$Flight_style_index[data_ex$Flight_style_index == 'flap'] <- 2 
data_ex$Flight_style_index[data_ex$Flight_style_index == 'hover'] <- 3 

#### Check the correlation between variables
## Make the df numeric
data_ex<-data_ex[complete.cases(data_ex), ]
data_ex <- apply(data_ex, 2, as.numeric)

##Calculated and visualized
cor_mat <- cor(data_ex, use = "pairwise.complete.obs", method = 'spearman')
cor_mat_2 <- round(cor_mat, 2)
heatmap(cor_mat_2,cexRow = 0.5, cexCol = 0.5, )

##Try the 煒哥 chart (If want to get more information)
library(PerformanceAnalytics)
chart.Correlation(data_ex, method = 'spearman',histogram=TRUE, pch=20, col = 'grey35')
#If you therefore want to change the variables, go back to module1

##=================== Module 2.7 end ==================##