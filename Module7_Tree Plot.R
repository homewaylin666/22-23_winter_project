##================== Evolutionary Tree ================##

####請直接從Module1跳到這裡的phy_data
data<- data[data$AL.index != 0, ] 
####只挑我要的列，以免有多餘的帶NA行等下被刪掉
data<- data[,c("Species3", "Family3", "Order3", "Hand.Wing.Index", "max.all")]
####然後參考Module3.5，做一個phy_data
data$Species3 <- gsub(" ", "_", data$Species3)
row.names(data) <- data$Species3
library(ape)
birdtree <- read.tree("BirdzillaHackett5.tre")[[321]]
tree <- drop.tip(birdtree, setdiff(birdtree$tip.label, data$Species3)) 
library(caper)
phy_data <- comparative.data(phy = tree, data = data, names.col = "Species3", vcv = T, na.omit = T)
length(phy_data$phy$tip.label) #n=3982




#################開始做圖，上面的基本沒什麼好改動的
library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(phylobase)
library(ggplot2)
library(ggtree)
library(cowplot)

#####下面這一段就是Hungwei's plot的代碼
## #############Tree plot 
##Let's make different orders have different colours.
##But the orders are too many, I only want 15 largest orders, let's make other order become one category.
freq_table <- table(phy_data$data$Order3)
sorted_freq_table <- sort(freq_table, decreasing = TRUE)
top_orders <- names(sorted_freq_table[1:15])
phy_data$data$Order3[!(phy_data$data$Order3 %in% top_orders)] <- "Other"
##Let's plot the central tree
od <- data.frame(sp=row.names(phy_data$data), od= phy_data$data$Order3)
row.names(od) <- row.names(phy_data$data)
od_mat <- as.matrix(od)[,c(1,2)]
phy_od <- phylo4d(phy_data$phy, od_mat) 
P.tree <- ggtree(phy_od, aes(col = od), layout = 'circular', size=0.1 ) #+theme(legend.position = "none")
P.tree
###############################目的分色效果我不滿意，之後可以問問Sheard那張是怎麼做的！









###I want HWI colours from '#330000' to '00FFFF'





####################下面都是Jingyi的代碼，我還沒學，沒啥用

## Add heatmap-1 (inner circle): incubation roles.
i4p <- as.data.frame(ant$data$Incubation.roles)    
i4p[i4p == '0'] <- 'Female only'
i4p[i4p == '2'] <- 'Biparental'
rownames(i4p) <- rownames(ant$data)

tp2 <- gheatmap(tp1, i4p, 
                offset = 0.1, width = 0.05, colnames = FALSE) 


## Add heatmap-2 (middle circle): visibility.
v4p <- as.data.frame(ant$data$Nest.invisibility)    
v4p[v4p == '0'] <- 'Visible'
v4p[v4p == '1'] <- 'Invisible'
rownames(v4p) <- rownames(ant$data)

tp3 <- gheatmap(tp2, v4p,
                offset = 3.6, width = 0.05, colnames = FALSE) +
  scale_fill_manual(values = c('lightblue1', 'skyblue3', 'coral2', 'peachpuff1')) +
  labs(fill = 'Natural Selection')  # need its legend.


## Add heatmap-3 (outer circle): mating system.
s4p <- as.data.frame(ant$data$ss)
s4p[s4p == '1.Monogamous'] <- 'Monogamous'
s4p[s4p == '2.Polygynous'] <- 'Polygynous'
rownames(s4p) <- rownames(ant$data)

tp4 <- gheatmap(tp1, s4p,
                offset = 3.6, width = 0.05, colnames = FALSE) +
  scale_fill_manual(values = c('olivedrab', 'olivedrab1')) + 
  labs(fill = 'Sexual Selection')  # need its legend.
#########Jin1加的那圈是heatmap，但我不是，不知道能不能參考


### Final plot with the tree and 3 heatmaps (Figure 2):

gheatmap (tp3, 
          s4p, 
          legend_title = '',
          offset = 7.2, width = 0.05, colnames = FALSE) +
  scale_fill_manual(values = c('lightblue1', 'skyblue3', 'coral2',
                               'olivedrab', 'olivedrab1', 'peachpuff1')) +
  theme(legend.position = "none")


### retrieve legends of previous layers.

plot(get_legend(tp3))      # << heatmap 1-2 (Natural Selection).

plot(get_legend(tp4))      # << heatmap 3   (Sexual Selection).

#####圖例看這個
d4p <- as.data.frame(ant$data$Dichro_total)   
rownames(d4p) <- rownames(ant$data)
gheatmap (tp1,               # << the tree (total Dichromatism).
          d4p, 
          low = 'navy', high = 'gold',
          legend_title = 'Total Dichromatism',
          offset = 7.2, width = 0.05, colnames = FALSE) 






