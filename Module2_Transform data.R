##=============== Check & Transform data ==============##

#### Create a new df with only the variables currently of interest
##Create a vector of predictor variables that you want to use
#The predictor variables to be used can be determined according to module 2.7
var_pd<- c('max.all', 'AL.index','Flight_style_index','temp_seasonality','Migration','Mass','Habitat.Density','Territory')
##response variables
var_rp<- c("Hand.Wing.Index","Wing.Length","Secondary1")
##taxonomy data
var_sp<- c("Species3","Family3","Order3")
##Combine them into a new df
data<-data[,c(var_pd,var_rp,var_sp)]

#### Check data distribution
##categorical variable 
table(data$AL.index)                        #0:1:2:3 = 45:1425:1502:1055，該把0刪掉
table(data$Flight_style_index)              #f:f/s:l:h:s = 3230:31:43:334:380，該把f/s合併掉！並且把l刪掉
table(data$Migration)                       #1:2:3 = 2994:583:439
table(data$Habitat.Density)                 #1:2:3 = 1634:1165:1225
table(data$Territory)                       #N:W:S = 1916:1106:1005，它們有明顯方向，為了後續方便，改成0,1,2吧，並且經過後續測試，決定將1和2合併

##certainty scores
table(data$AL_uncertainty)                  #1:2:3 = 1185:2121:721
table(data$FS_uncertainty)                  #1:2:3 = 312:3630:85

##Check the two categorical variables that I think will be interacted with
#It should have more than 2 samples of each interaction
#not this time

##continuous variable
hist(data$Hand.Wing.Index)          #不是正態分佈但挺像的
hist(data$Wing.Length)              #數據右偏，該取根號！
hist(data$Secondary1)               #數據右偏，該取根號！
hist(data$max.all)                  #數據右偏，該取根號！
hist(data$temp_seasonality)         #數據超右偏，該取對數！
hist(data$Mass)                     #數據超右偏，該取對數！

#### Transforming based on the above results
##categorical variable 
data<- data[data$AL.index != 0, ] #只需刪掉AL=0，FS=flightless全部包含在內, n=3982(所有會飛的鳥) | 3413(所有高確定的飛鳥)
data$Flight_style_index[data$Flight_style_index == 'flap/soar'] <- 'flap' 
data$Territory[data$Territory == 'None'] <- 0 
data$Territory[data$Territory == 'Weak'|data$Territory == 'Strong'] <- 1
table(data$Flight_style_index)              #f:l:h:s = 3261:43:343:380
table(data$Territory)                       #0:1:2 = 1916:1106:1005

##continuous vairable
data$Secondary1<- sqrt(data$Secondary1)
data$Wing.Length<- sqrt(data$Wing.Length)
data$max.all<- sqrt(data$max.all)
data$temp_seasonality<- log(data$temp_seasonality)
data$Mass<- log(data$Mass)

##Visualizing the distribution of continuous variables
par(mfrow = c(3,2),mar = c(3,3,2,1))
con_var_col<- c('Hand.Wing.Index','Wing.Length','Secondary1','max.all','temp_seasonality','Mass')
con_var_ind <- which(names(data) %in% con_var_col)
for (i in con_var_ind){
  hist(data[,i], main = colnames(data)[i])     
} 
#All continuous variable looks closer to ND.

##Standardisation of continuous variables
data$Hand.Wing.Index<- scale(data$Hand.Wing.Index)         
data$Wing.Length<- scale(data$Wing.Length)           
data$Secondary1<- scale(data$Secondary1)              
data$max.all<- scale(data$max.all)           
data$temp_seasonality<- scale(data$temp_seasonality)         
data$Mass<- scale(data$Mass)  

##Visualizing the distribution of continuous variables
con_var_ind <- which(names(data) %in% con_var_col)
for (i in con_var_ind){
  hist(data[,i], main = colnames(data)[i])     
} 

##=====================Module2 end=====================##

