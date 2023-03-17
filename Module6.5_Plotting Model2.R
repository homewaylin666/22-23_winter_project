##================== Plotting Model 2 =================##

#Before import, please make sure the names of variables are proper
WL_avg <- read.csv("PGLS model result (WL).csv")
S1_avg <- read.csv("PGLS model result (S1).csv")
WL_avg <- WL_avg[,c(0:10)]
S1_avg <- S1_avg[,c(0:10)]
#Delete Mass in the data
library(dplyr)
WL_avg <- WL_avg[WL_avg$Variables != "Body Mass", ]
S1_avg <- S1_avg[S1_avg$Variables != "Body Mass", ]

##Assign the order of rows
my_var_order<- c("Elevation", "FS (Hover)", "FS (Soar)", "Temp. var.", "Aerial Lifestyle", 
                 "Migration", "Habitat Density", "Territoriality", 
                 "Elevation : FS (Hover)", "Elevation : FS (Soar)")
##In this graph, the order will be reverse
my_var_order<- rev(my_var_order)

library(ggplot2)
library(ggthemes)
##Create a function that can make a forest plot of the model coefficients
FP <- function(md, range){
  md$lower <- md$Estimate - 1.96 * md$StdErr  
  md$upper <- md$Estimate + 1.96 * md$StdErr
  md$Significance <- ifelse(md$lower * md$upper > 0, 'Significant', 'Non-significant')
  md$Effect <- ifelse(md$Estimate > 0, 'Positive', 'Negative')
  md$Variables <- factor(md$Variables, levels = my_var_order)
  
  ggplot(md[-1, ], aes(Variables, Estimate)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_pointrange(aes(ymin = lower, ymax = upper, shape = Effect, col = Significance)) +
    ylim(-range, range) + labs(y = 'Estimates') +
    coord_flip() +
    theme_minimal() #minimal also looks good
}


##Use the function
WL_p1 <- FP(WL_avg, 0.25) + ggtitle("Wing Length")+theme(plot.title = element_text(hjust = 0.5))
S1_p1 <- FP(S1_avg, 0.25) + ggtitle("Secondary Length")+theme(plot.title = element_text(hjust = 0.5))

##The tree plot of WL & S1
pl <- WL_p1 +  theme(legend.position = "none") 
pr <- S1_p1 + theme(axis.title.y=element_blank(), axis.text.y=element_blank())
cowplot::plot_grid(pl, pr, ncol = 2, rel_widths = c(1, 1))

##==================== Module6.5 end ==================##