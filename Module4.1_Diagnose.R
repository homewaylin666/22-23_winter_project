#3.7 Model Validation

##Normality of the residuals
resid <- residuals(HWI[[1]])
qqnorm(resid)
qqline(resid)
resid <- residuals(WL[[1]])
qqnorm(resid)
qqline(resid)
resid <- residuals(S1[[1]])
qqnorm(resid)
qqline(resid)

##Homogeneity of the residuals
library(ggplot2)
resid_fitted <- data.frame(resid = resid(HWI[[1]]), fitted = fitted(HWI[[1]]))
# Create a scatterplot of residuals against fitted values
ggplot(resid_fitted, aes(x = fitted, y = resid)) +
  geom_point() +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs. Fitted Values")

library(car)
spreadLevelPlot(HWI[[1]]) 

