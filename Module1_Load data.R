##================== Load trait data ==================##

rm(list=ls())
getwd()
setwd('/Users/homeway/Desktop/Winter Project/data')

#### Load trait data
row_data<- read.csv("wp_data_np.csv")

##Check which columns have lots of NA
apply(row_data, 2, function(x) sum(is.na(x)))

##Only want columns with less than 100 NA
data <- row_data[, colSums(is.na(row_data)) < 100]

#### Add in the data we've previously consolidated
##mix elevation
elev<- read.csv("Mix_Elev.csv")
data<- cbind(data, elev)

##additional data
add<- read.csv('additional eco data.csv')
data<- merge(add[,c("Species3","Territory","temp_seasonality")],
             data, by = "Species3", all.y = TRUE)

##Optional: leave the data with certainty score bigger than 2.
data<- data[data$AL_uncertainty == 1 | data$AL_uncertainty == 2, ]
data<- data[data$FS_uncertainty == 1 | data$FS_uncertainty == 2, ]

#data<- data[data$Flight_style_index == 'flap', ]

#### Check out what it looks like
View(data)

##==================== Module1 end ====================##