library(igraph)
library(ggraph)
library(graphlayouts)

g <- sample_gnp(20,0.3)

ggraph(ngram_graph,layout = "stress")+
  geom_edge_link0()+
  geom_node_point(size=8,shape=21,fill="white")+
  geom_node_text(label=1:vcount(g))+
  theme_graph(base_family = "Arial",base_size = 12)+
  labs(title="test")

