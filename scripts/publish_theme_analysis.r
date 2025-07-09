source("~/GitHub/adroseHelperScripts/R/afgrHelpFunc.R")
library(igraph)
data <- read.csv("~/Documents/adrose.github.io/files/booleanNetwork.csv")
names(data) <- tolower(names(data))
## Now I need to melt the data
Adj <- as.matrix(data[,5:25])
adj_matrix <- crossprod(Adj)
#adj_matrix <- apply(adj_matrix, 1, function(x) range01(x))
## Now make a matrix?
graph_obj <- igraph::graph_from_adjacency_matrix(adj_matrix, diag = FALSE, mode = "undirected", weighted = TRUE)
s <- strength(graph_obj)
w <- E(graph_obj)$weight
E(graph_obj)$width <- range01(w)*3
V(graph_obj)$size <- (range01(s)+1)*10
louvain_clusters <- cluster_louvain(graph_obj, resolution = 1.3)
plot(louvain_clusters, graph_obj, layout = layout_in_circle)
plot(louvain_clusters, graph_obj, layout = layout_as_star)
png("./Documents/adrose.github.io/images/rosenResearchThemeNetwork.png", width = 12, height = 12, units="in", res=300)
plot(louvain_clusters, graph_obj, main="Rosen Research Themes Network")
dev.off()
