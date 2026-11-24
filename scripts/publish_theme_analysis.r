source("~/GitHub/adroseHelperScripts/R/afgrHelpFunc.R")
library(igraph)
library(ggnetwork)
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
V(graph)$name <- c("Node A", "Node B", "Node C", "Node D", "Node E")
louvain_clusters <- cluster_louvain(graph_obj, resolution = 1.3)
plot(louvain_clusters, graph_obj, layout = layout_in_circle)
plot(louvain_clusters, graph_obj, layout = layout_as_star)
plot(louvain_clusters, graph_obj, layout = layout_with_fr)
png("~/Documents/adrose.github.io/images/rosenResearchThemeNetwork.png", width = 12, height = 12, units="in", res=300)
plot(louvain_clusters, graph_obj, layout = layout_with_fr)
dev.off()

# 2. Run Louvain clustering
clusters <- cluster_louvain(graph_obj, resolution = 1.3)

# 3. Add cluster membership as a node attribute
V(graph_obj)$community <- as.factor(membership(clusters))

# 4. Plot using ggraph (the ggplot2 interface for networks)
ggraph(graph_obj, layout = 'graphopt') + 
  geom_edge_link(alpha = 0.2) + 
  geom_node_point(aes(color = community), size = 5) +
  geom_node_label(aes(label = class), repel = TRUE) +
  theme_graph() +
  labs(title = "Louvain Community Detection", color = "Cluster") +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white')



library(visNetwork)
nodes <- data.frame(id = V(graph_obj))
edges <- as.data.frame(as_edgelist(graph_obj))
names(edges) <- c("from", "to")

# Create the interactive network
forWeb <- visIgraph(graph_obj, layout = "layout_in_circle")
visSave(forWeb, file = "~/Documents/adrose.github.io/files/network.html", background = "white", selfcontained = TRUE)
## Now add the clusters
dummy <- toVisNetworkData(graph_obj)
my.edges <- dummy$edges
my.nodes <- dummy$nodes
my.nodes$groups <- louvain_clusters$membership
my.nodes$color.background <- c("red", "blue", "green" ,"yellow", "purple")[my.nodes$groups]
my.nodes$color.border <- c("red", "blue", "green" ,"yellow", "purple")[my.nodes$groups]
my.edges$color <- "black"
my.edges$width <- (range01(my.edges$width)+.05)*3
forWeb <- visNetwork(my.nodes, my.edges)%>%
  visIgraphLayout(layout = "layout_with_sugiyama",physics = FALSE, smooth = TRUE) %>% 
  visOptions(highlightNearest = list(enabled = T, hover = T), 
             nodesIdSelection = T)
visSave(forWeb, file = "~/Documents/adrose.github.io/files/network.html", background = "white", selfcontained = TRUE)
