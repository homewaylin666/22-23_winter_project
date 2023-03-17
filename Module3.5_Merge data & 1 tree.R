##================ Merge data & 1 tree ================##

##Change species name to a format that matches tree
data$Species3 <- gsub(" ", "_", data$Species3)

##Make Species as row name
row.names(data) <- data$Species3

#### Load the tree and the file for tip names matching
library(ape)
##just one tree
birdtree <- read.tree("BirdzillaHackett5.tre")[[321]]
#takes around 6 min

##Delete the species which we don't have in our data
#setdiff() find the difference set, and drop.tip() delete it）
tree <- drop.tip(birdtree, setdiff(birdtree$tip.label, data$Species3)) 

#### Combine data and tree!
library(caper)
phy_data <- comparative.data(phy = tree, data = data, names.col = "Species3", 
                            vcv = T, na.omit = T)
#takes around 5 min

#### Check the length
length(phy_data$phy$tip.label) #3360 species
# if set na.omit = F -> ???? species

##=================== Module3.5 end ===================##