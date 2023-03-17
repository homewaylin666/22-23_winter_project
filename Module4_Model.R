##================= Fitting PGLS model ================##

#### Change species name to a format that matches tree and make it as row name
data$Species3 <- gsub(" ", "_", data$Species3)
row.names(data) <- data$Species3

#### Make categorical variables as factor or numeric
data$AL.index <- as.numeric(data$AL.index) #作為因子會各級同向，考慮變numeric
data$Flight_style_index <- as.factor(data$Flight_style_index)
data$Migration <- as.numeric(data$Migration) #作為因子會各級等差，考慮變numeric
data$Habitat.Density <- as.numeric(data$Habitat.Density)#作為因子會各級等差，考慮變numeric
data$Territory  <- as.factor(data$Territory )

library(ape)
library(caper)
library(phylolm)
library(rr2)

#### Load the tree and the file for tip names matching
birdtree <- read.tree("BirdzillaHackett5.tre")
ran_tree <- sample(birdtree, size=50) #這裡的size就是讀幾棵樹


##Create a function that can fit both full & reduce models and give 3 types of R2 for response variable 
##And it also contains the results of the use of 50 trees.
Mod.Var.50T <- function(rv){
  #### Prepare formulas for full & reduce models
  fml.full <- formula(paste(rv, '~', 'max.all','*','Flight_style_index','+','AL.index','+','temp_seasonality', 
                            '+', 'Migration','+','Mass','+','Habitat.Density','+','Territory'))
  fml.reduce <- formula(paste(rv, '~', 1))
  
  ## Create empty dataframes
  model_coef <- data.frame()
  r2 <- data.frame(Full.model.R2 = NA, Phylogeny.R2 = NA, Predictors.R2 = NA)
  
  #### PGLS (lambda) models & get the three types of R2
  ##For every tree   ####左下方這個數字要記得隨著你要跑的樹而改
  for (i in 1:50){
    
    ##Delete the species which we don't have in our data
    one_tree <- ran_tree[[i]]
    tree <- drop.tip(one_tree, setdiff(one_tree$tip.label, data$Species3))
    
    ## Combine data and tree!
    phy_data <- comparative.data(phy = tree, data = data, names.col = "Species3", 
                                 vcv = T, na.omit = T)
    
    ## 1) Full model & Get the full model R2 & Gent the summary 
    bm <- phylolm(fml.full,
                  data = phy_data$data, phy = phy_data$phy, model = 'lambda')
    r2$Full.model.R2 <- R2_lik(bm)  
    summary_bm<-summary(bm)   
    
    ## 2) Remove phylo structure to get phylogeny R2 (lambda = 0) 
    nop <- data.frame(lambda = 0)
    bmP <- phylolm(fml.full,
                   data = phy_data$data, phy = phy_data$phy, model = 'lambda',
                   starting.value = nop, lower.bound = nop$lambda, upper.bound = nop$lambda)
    r2$Phylogeny.R2 <- R2_lik(bm, bmP)    
  
    ## 3) Use Intercept-only model to get predictors R2
    nov <- data.frame(lambda = bm$optpar)
    bmV <- phylolm(fml.reduce, 
                 data = phy_data$data, phy = phy_data$phy, model = 'lambda', 
                 starting.value = nov, lower.bound = nov$lambda, upper.bound = nov$lambda) 
    r2$Predictors.R2 <- R2_lik(bm, bmV)   
    ###########至此，已經擁有一個叫r2的df包含三種r2，還有summary_bm，名如其義。
    ##Select the coefficients and r2, then append to the data frame.
    lambda_coef <- cbind(rownames(summary_bm$coefficients), summary_bm$coefficients, bm$optpar, r2)
    model_coef <- rbind(model_coef, lambda_coef) # model_coef會隨著循環越來越長
    ###########上面這兩行的原作者CY把bm$optpar打成bm$lambda了，真是的。我沒改動，只是加上了r2
  }
  return(model_coef) #是一個df，前面幾列是Model的各種系數，然後有一列lambda，一列應變量名和三列R2
}
    
##Use the function
HWI <- Mod.Var.50T("Hand.Wing.Index")
WL <- Mod.Var.50T("Wing.Length")
S1 <- Mod.Var.50T("Secondary1") 

## Clean up the data
library(dplyr)
#### Make a function to do it
Avg.Mod <- function(mv){
  colnames(mv)[1] <- "Variables"
  colnames(mv)[6] <- "lambda"
  avg <- mv %>% group_by(Variables) %>% summarise_all(mean)
  return(avg)
}

##Use the function
HWI_avg <- Avg.Mod(HWI) 
WL_avg <- Avg.Mod(WL) 
S1_avg <- Avg.Mod(S1) 

##Export results
write.csv(HWI_avg, "PGLS model result (HWI).csv") 
write.csv(WL_avg, "PGLS model result (WL).csv") 
write.csv(S1_avg, "PGLS model result (S1).csv") 

##===================== Module4 end ===================##