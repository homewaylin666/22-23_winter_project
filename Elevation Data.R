rm(list=ls())
getwd()
setwd('/Users/homeway/Desktop/Winter Project/data')
dir()
#導入data
wp_data_np<- read.csv("wp_data_np.csv", row.names = 1)
#讓我清靜點，先只要海拔data
library(dplyr)
data<- select(wp_data_np,Upper_limit,Lower_limit,Sp_max_elev_Quintero,Sp_min_elev_Quintero,max_wb,min_wb,new_max,new_min)
#取各种source（也就是除了iucn的三種，也就是最前面的)的最高值作Max，最低值作MIN。
data<- mutate(data, max.all= pmax(Sp_max_elev_Quintero, max_wb, new_max,na.rm = T))
data<- mutate(data, min.all= pmin(Sp_min_elev_Quintero, min_wb, new_min,na.rm = T)) 
#因為發現居然有一個最低分布是小於零的，由於才一個，我懷疑是錯誤，直接查了一下，發現一定是錯誤，手動改成了0。
#檢查一下有多少NA
sum(is.na(data$max.all))
sum(is.na(data$min.all))
#把這些NA用IUCN的數據（upper/lower limit)填上。
data$max.all[which(is.na(data$max.all))]<-data$Upper_limit[which(is.na(data$max.all))] #用which找出NA的索引值然後替代
data$min.all[which(is.na(data$min.all))]<-data$Lower_limit[which(is.na(data$min.all))] #用which找出NA的索引值然後替代
#再檢查一下有多少NA
sum(is.na(data$max.all))
sum(is.na(data$min.all))
#很好，再做一列最終的range吧！
data<- mutate(data, range.all= max.all- min.all)
#最後，我們做一列平均elevation
data<- mutate(data, mean.elev.all= (max.all + min.all)/2)
#搞定，讓我們只留下all系列然後保存吧，為了清淨～
ultimate_elev <- select(data, max.all, min.all, range.all,mean.elev.all)
write.csv(ultimate_elev, "Mix_Elev.csv") 



