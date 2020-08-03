# Part 0: Data Retrieval

library(repr)
options(repr.plot.width=16, repr.plot.height=12)

library(igraph)
library(anocva)
set.seed(2020)

setwd('Documents/SocialNetworks/data')

# read the marvel data
nodes <- read.csv("got-nodes.csv", header=T, as.is=T)
edges <- read.csv("got-edges.csv", header=T, as.is=T)

# Part 1: Data Overview

# examine the data:
head(nodes)
head(edges)

# convert to net
net <- graph_from_data_frame(d=edges, vertices=nodes, directed=F) 

# explore the data
E(net) # edges
V(net) # nodes
# specific variables in each
V(net) %>% names()
E(net)$Weight # weights of relationship
V(net)$gender # gender of each character
V(net)[family.name=="targaryen"] # characters with last name taragaryen

# first plot
#plot(net)

# simplify the net 
net <- simplify(net, remove.multiple = F, remove.loops = T) 

# second, cleaner plot
# par(mfrow=c(1,1))
# plot(net, edge.arrow.size=.1, edge.curved=0.1,
#      vertex.color="tomato", vertex.frame.color="white",
#      vertex.label=names(V(net)), vertex.label.color="black",
#      vertex.label.cex=.5) 


# compute size of nodes based on number of degrees
deg <- degree(net, mode="all", normalized = F)
V(net)$size <- deg/3
E(net)$width <- E(net)$Weight/10

# with width edges, size vertex (Figure 1)
par(mfrow=c(1,1))
set.seed(2020)
plot(net, edge.arrow.size=.1, edge.curved=0.1,
     vertex.color="red",
     vertex.label=names(V(net)), vertex.label.color="black",
     vertex.label.cex=.7, edge.width=E(net)$width, 
     size=V(net)$size) 

# weights
# hist(E(net)$Weight)
# mean(E(net)$Weight) 
# sd(E(net)$Weight)

# average degree of the graph:
mean(deg) 

sort(closeness(net, mode="all", weights=NA, normalized=T), decreasing = T)[1:15]
sort(betweenness(net, directed=F, weights=NA, normalized = T), decreasing = T)[1:15]

# deep dive on Daenerys
d.path <- shortest_paths(net, 
                            from = V(net)[Label=="Daenerys"], 
                            to  = V(net)[Label=="Robb"],
                            output = "both") # both path nodes and edges

# Generate edge color variable to plot the path:
ecol <- rep("gray80", ecount(net))
ecol[unlist(d.path$epath)] <- "orange"
# Generate edge width variable to plot the path:
ew <- rep(2, ecount(net))
ew[unlist(d.path$epath)] <- 4
# Generate node color variable to plot the path:
vcol <- rep("gray40", vcount(net))
vcol[unlist(d.path$vpath)] <- "gold"

vnames <- rep("", vcount(net))
vnames[unlist(d.path$vpath)] <- names(unlist(d.path$vpath))

set.seed(2020)
plot(net, vertex.color=vcol, edge.color=ecol, 
     edge.width=ew, edge.arrow.mode=0, vertex.label=vnames)

# sparsify by cut off mean(deg)
cut.off <- mean(deg)
net.sp <- delete_edges(net, E(net)[Weight<cut.off])
plot(net.sp) 

# distances from tyrion (top degree character)
dist.from.T <- distances(net, v=V(net)[Label=="Tyrion"], to=V(net), weights=NA)

# Set colors to plot the distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.T)+1)
col <- col[dist.from.T +1]

plot(net, vertex.color=col, vertex.label=dist.from.T, edge.arrow.size=.3, 
     vertex.label.color="black", cex = 0.3)

# let's look at the distribution of the degrees
par(mfrow=c(3,1))

hist(deg, col = 'tomato', xlab = 'node degree', ylab='frequency', main= 'Overall Degree Distribution for GOT Network')
# power law

deg_data<-data.frame(deg)
deg_data$gender<-V(net)$gender
hist(deg_data[deg_data$gender=='m', 'deg'], col='darkorange', xlab = 'node degree', ylab='frequency', main='Degree Distribution for GOT Network Male Characters')
hist(deg_data[deg_data$gender=='f', 'deg'], col='darkorchid', xlab = 'node degree', ylab='frequency', main='Degree Distribution for GOT Network Female Characters')

deg.dist <- degree_distribution(net, cumulative=T, mode="all")
plot( x=0:max(degree(net)), y=1-deg.dist, pch=19, cex=1.4, col="tomato", 
      xlab="Degree", ylab="Cumulative Frequency")
title('Cumulative Degree Distribution for GOT Network')

# Part 2: Which nodes are the most important in the network?

# let's examine who top nodes are:
top_15<-V(net)[order(V(net)$size, decreasing = T)][1:15]$Label
print(top_15)
# top_15 by gender indicates that hub nodes tend to be male by 2:1 ratio (10:5).

# reduces to 4:1 ratio amongst all characters
V(net)$gender[names(V(net)) %in% top_15] %>% table()
V(net)$gender %>% table() # total gender ratio
# stark is the most popular surname and political lean
V(net)$family.name[names(V(net)) %in% top_15] %>% table()
V(net)$politics[names(V(net)) %in% top_15] %>% table()
# kings landing is the most popular geography
V(net)$geography[names(V(net)) %in% top_15] %>% table()

# subgraph of hub nodes to see how connected they are to each other
V(net)$selected <- 0
V(net)$selected[V(net)$Label %in% top_15] <- 1
subnet <- induced_subgraph(graph=net, v = which(V(net)$selected==1))

# recompute the degree
deg2 <- degree(subnet, mode="all", normalized = F)
V(subnet)$size <- deg2
E(subnet)$width <- E(subnet)$Weight/5

par(mfrow=c(1,2),mar=c(1,1,1,1))
# total plot 
colrs <- c("darkorchid", "darkorange")
V(net)$color <- colrs[as.factor(V(net)$gender)]
set.seed(2020)
plot(net, vertex.label.cex=0.7, vertex.label.color='black')

# subplot
colrs <- c("darkorchid", "darkorange")
V(subnet)$color <- colrs[as.factor(V(subnet)$gender)]
set.seed(2020)
plot(subnet, vertex.label.cex=1, vertex.label.color='black') 
legend(x=-0.5, y=1, c("female","male"), pch=21,
       col="black", pt.bg=colrs, pt.cex=1, bty="n", ncol=1, cex=1)


# Part 3: Can you provide a visualization with a nice layout?

# let's try to review the family name now

unique(V(net)$family.name) %>% length

colrs <- c("tomato", "gray50", rainbow(28))
V(net)$color <- colrs[as.factor(V(net)$family.name)]

par(mfrow=c(1,1),mar=c(1,1,1,1))
set.seed(2020)
plot(net, vertex.label.cex=1, vertex.label.color='black') 
legend(x=1, y=1, as.factor(V(net)$family.name) %>% levels(), pch=21,
       col="black", pt.bg=colrs, pt.cex=1, bty="n", ncol=1, cex = 0.8)
text('family name', x = 1.1, y=1, cex = 1, col='black')


# geography

unique(V(net)$geography) %>% length
colrs <- c("gray50", rainbow(11))

V(net)$color <- colrs[as.factor(V(net)$geography)]
set.seed(2020)
plot(net, vertex.label.cex=1, vertex.label.color='black') 
legend(x=1, y=1, as.factor(V(net)$geography) %>% levels(), pch=21,
       col="black", pt.bg=colrs, pt.cex=1, bty="n", ncol=1, cex=0.8)
text('geography', x = 1.1, y=1, cex = 1, col='black')


# try with other layout - other layouts are not as easy distinguish as the default (layout_with_fr)
# source: https://dshizuka.github.io/networkanalysis/03_plots.html

set.seed(2020)
layouts = c("layout_with_fr", "layout_with_kk", "layout_with_dh", "layout_with_gem", "layout_as_star", "layout_as_tree", "layout_in_circle", "layout_on_grid")
par(mfrow=c(2,4), mar=c(1,1,1,1))
for(layout in layouts){
   l=do.call(layout, list(net))
   plot(net, layout=l, edge.color="black", vertex.label=NA, main=layout)
}

# politics is murky in the center, but well defined at the outskirts
unique(V(net)$politics) %>% length
colrs <- c(rainbow(19))

V(net)$color <- colrs[as.factor(V(net)$politics)]
set.seed(2020)
plot(net, vertex.label.cex=1, vertex.label.color='black') 
legend(x=1, y=1, as.factor(V(net)$politics) %>% levels(), pch=21,
       col="black", pt.bg=colrs, pt.cex=1, bty="n", ncol=1, cex=1)
text('politics', x = 1.1, y=1, cex = 1, col='black')

wc <- cluster_walktrap(net, weights = E(net)$Weight)

modularity(wc)
membership(wc)

V(net)$community <- membership(wc)
cols <- rainbow(7)
V(net)$color <- cols[V(net)$community]

E(net)$color <- 'gray50'
new_cols <- cols[membership(wc)]
set.seed(2020)
plot(wc, net, new_cols, edge.color=E(net)$color)

# Part 4: Are there any community structures? If so, can you provide any interpretation?

#wc <- cluster_walktrap(net, weights = E(net)$Weight)
fg <- cluster_fast_greedy(net, weights = E(net)$Weight)

modularity(fg)
membership(fg)

V(net)$community <- membership(fg)
cols <- rainbow(7)
V(net)$color <- cols[V(net)$community]

E(net)$color <- 'gray50'
new_cols <- cols[membership(fg)]
set.seed(2020)
plot(fg, net, new_cols, edge.color=E(net)$color)

fg <- cluster_fast_greedy(net, weights = E(net)$Weight)

modularity(fg)
membership(fg)

V(net)$community <- membership(fg)
cols <- rainbow(7)
V(net)$color <- cols[V(net)$community]

E(net)$color <- 'gray50'

#unique(V(net)$geography) %>% length
#colrs <- c("gray50", rainbow(11))

#colors correspond roughtly to show geography
colrs <- c("azure2", 'firebrick', 'tomato', 'darkorange', 
           'darkcyan','goldenrod1', 'floralwhite', 'burlywood1', 'tan', 'springgreen4','bisque4','deepskyblue')

new_cols <- colrs[as.factor(V(net)$geography)]
set.seed(2020)
plot(fg, net, new_cols, edge.color=E(net)$color, vertex.label.color='black')
legend(x=1, y=1, as.factor(V(net)$geography) %>% levels(), pch=21,
       col="black", pt.bg=colrs, pt.cex=1, bty="n", ncol=1, cex=0.8)
text('geography', x = 1.1, y=1, cex = 1, col='black')

# simple community detection

nodes <- read.csv("got-nodes-original.csv", header=T, as.is=T)
edges <- read.csv("got-edges.csv", header=T, as.is=T)

net <- graph_from_data_frame(d=edges, vertices=nodes, directed=F) 


fg <- cluster_fast_greedy(net, weights = E(net)$Weight)

modularity(fg)
membership(fg)

V(net)$community <- membership(fg)
cols <- rainbow(7)
V(net)$color <- cols[V(net)$community]

E(net)$color <- 'gray50'

#unique(V(net)$geography) %>% length
colrs <- c("gray50", rainbow(17))

new_cols <- cols[membership(fg)]

plot(fg, net, new_cols, edge.color=E(net)$color)


