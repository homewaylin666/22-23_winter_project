##==================Check colinearity==================##

#Reminder: There are 'var_pd', 'var_rp', 'var_sp' in data
#Only var_pd is needed for this module

#### Making a df for checking colinearity
##Pick only predictor variables to calculate VIFs
data_pd <-data[,var_pd]

##Delete the NA value before calculation
data_pd <-data_pd[complete.cases(data_pd), ]

#### Convert character to numeric
##Flight_style_index (Most relevant to AL version)
table(data_pd$Flight_style_index)
data_pd$Flight_style_index[data_pd$Flight_style_index == 'flap'] <- 1 
data_pd$Flight_style_index[data_pd$Flight_style_index == 'soar'] <- 2 
data_pd$Flight_style_index[data_pd$Flight_style_index == 'hover'] <- 3 
#For character var., you may convert them to dummy variables to calculate VIFs, but not this time

#### Calculate VIFs
library(car)
##Create a vector to store the VIFs
vifs <- numeric(ncol(data_pd))

##Loop through each variable in data_pd
for (i in 1:ncol(data_pd)){
  fit <- lm(data_pd[,i] ~ ., data = data_pd[,-i])
  vifs[i] <- 1/(1 - summary(fit)$r.squared)
}

##Print the VIFs
vifs #each VIF: 1.123976 2.235231 1.768476 1.721282 1.688005 2.270814 1.652593 1.552311
mean(vifs) #mean VIF: 1.751586
# All less than 3 is well

##==================== Module3 end ====================##