#!/usr/bin/Rscript
#### load data #####
load("./phyData.Rdata")
#################開始做圖，上面的基本沒什麼好改動的
library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(phylobase)
library(ggplot2)
library(ggtree)
library(ggtreeExtra)
library(cowplot)
library(ggnewscale)
library("RColorBrewer")   
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
#### Nothing changed above #####

##Alter base plot to black plot
P.tree.black <- ggtree(phy_od,layout = 'circular', size=0.2 ) #+theme(legend.position = "none")
P.tree.black

#select the first row in node labels that matches the Order3 name
#### function ####
first_match <- function(df, col_index, keyword){
    for (i in 1:dim(df[col_index])[1]){
        if (df[i,col_index] == keyword) {
            return (df[i,])
            break
        } 
    }
}

#### data wrangling ####
od_nodes <- data.frame(od.names = rep(NA,3982), od.nodes = rep(NA,3982))
od_nodes[1] <- phy_data$data$Order3
od_nodes[2] <- c(phy_data$phy$node.label, -1)
od_unique <- unique(od_nodes[1])
od_unique <- subset(od_unique, od.names != "Other")
od_unique
od_top_nodes <- sapply(1:dim(od_unique)[1], function(x) first_match(od_nodes, "od.names", od_unique[[x,1]]))

od_top_nodes <- t(od_top_nodes)                       
# od_top_nodes
#fine-tuning the nodes
node_tuned <- c(4032,4322,4480,4857,5173,5323,5736,6052,6858,6409,7017,7325,7490,7640,7787)                       
#create clean dataframe for visualization
od_nodes_vis <- data.frame(node=node_tuned, fill=unlist(od_top_nodes[,1], use.names=FALSE)  )                       
od_nodes_vis   

# sub-data preparation
elevation <- sqrt(phy_data$data$max.all)
length(elevation)
elevation_df <- data.frame(
    id= phy_data$phy$tip.label,
    value=elevation, fill=rep("elevation",length(elevation)))
hwi <- phy_data$data$Hand.Wing.Index
hwi_df <- data.frame(hwi=hwi)
rownames(hwi_df) <- phy_data$phy$tip.label

hwi_df

# create color scheme #
Order_color <- brewer.pal(12,"Paired")
Order_color <- append(Order_color,'#48AAAD',after=2)
Order_color <- append(Order_color,'#EBADD6',after=5)
Order_color <- append(Order_color,'#800000')

# plotting new tree #


p <- gheatmap(P.tree.black, hwi_df, low="#021861", high="#D8FDFF", #HWI heatmap
              offset=0, width=.1, colnames=FALSE, legend_title = 'HWI') +
    new_scale_fill() + #order masking
                geom_hilight(data=od_nodes_vis, mapping=aes(node=node, fill=fill), alpha = 0.3) +
                theme(legend.title=element_text(size=16), 
                      legend.key.height=unit(.6, 'cm'), legend.key.width=unit(.6, 'cm'), 
                      legend.text=element_text(size=14), 
                      # legend.background=element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth=.05),
                      legend.position= "right") +
                  scale_fill_manual(values = Order_color) +
   new_scale_fill() + #elevation plot
      geom_fruit(
          data=elevation_df,
          geom=geom_col,
           mapping=aes(y=id, x=value,col="", fill= "Elevation"), # use col to color the outline so that it is thick enough
          orientation="y",
          # stat="identity",
          # show.legend=FALSE,
          # position = "bottom",
          offset = 0.2
      ) +
    scale_color_manual(breaks=NULL, values="#98D1DC") +
    scale_fill_manual(name= NULL, values="#98D1DC")

p
# save plot #
png("./treePlot230309_test.png", width=60, height=60, units="cm", res=400)
p
dev.off()