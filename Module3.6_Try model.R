##=================== Try PGLS model ==================##

#### Make categorical variables as factor or numeric
phy_data$data$AL.index <- as.numeric(phy_data$data$AL.index) #作為因子會各級同向，考慮變numericphy_data$data$Flight_style_index <- as.factor(phy_data$data$Flight_style_index)
phy_data$data$Migration <- as.numeric(phy_data$data$Migration) #作為因子會各級等差，考慮變numeric
phy_data$data$Habitat.Density <- as.numeric(phy_data$data$Habitat.Density)#作為因子會各級等差，考慮變numeric
phy_data$data$Territory  <- as.factor(phy_data$data$Territory )

#### Get PGLS models and variance partition
library(phylolm)
library(rr2)
##Create a function that can fit both full & reduce models and give 3 types of R2 for response variable 
Mod.Var <- function(rv){
  ##Prepare formulas for full & reduce models
  #When making this formula, check var_pd first
  fml.full <- formula(paste(rv, '~','*','Flight_style_index','max.all','+','AL.index','+','temp_seasonality', 
                            '+','Migration','+','Habitat.Density','+', 'Mass','+','Territory')) 
  fml.reduce <- formula(paste(rv, '~', 1)) #
  
  ##PGLS (lambda) models & get the three types of R2
  r2 <- data.frame(Respond_Variable = rv, Full.model.R2 = NA, Phylogeny.R2 = NA, Predictors.R2 = NA)
  
  # 1) Full model -- to get the full model R2.
  bm <- phylolm(fml.full,
                data = phy_data$data, phy = phy_data$phy, model = 'lambda')
  r2$Full.model.R2 <- R2_lik(bm)          
  
  # 2) Remove phylo structure (lambda = 0) -- to get phylogeny R2.
  nop <- data.frame(lambda = 0)
  bmP <- phylolm(fml.full,
                 data = phy_data$data, phy = phy_data$phy, model = 'lambda',
                 starting.value = nop, lower.bound = nop$lambda, upper.bound = nop$lambda)
  r2$Phylogeny.R2 <- R2_lik(bm, bmP)    
  
  # 3) Intercept-only model (no predictors) -- to get predictors R2.
  nov <- data.frame(lambda = bm$optpar)
  bmV <- phylolm(fml.reduce, 
                 data = phy_data$data, phy = phy_data$phy, model = 'lambda', 
                 starting.value = nov, lower.bound = nov$lambda, upper.bound = nov$lambda) 
  r2$Predictors.R2 <- R2_lik(bm, bmV)   
  
  ## save the model results and R2 values.
  res <- list(bm, r2) # bm: Table S2-4.  r2: Table S5.
  return(res)
  
}

##Use the function
HWI <- Mod.Var("Hand.Wing.Index")
WL <- Mod.Var("Wing.Length")
S1<- Mod.Var("Secondary1")

##Read the fitting result
summary(HWI[[1]])
summary(WL[[1]])
summary(S1[[1]])

##==================== Module3.6 end ==================##