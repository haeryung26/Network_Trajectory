# install.packages("ggraph")
# rm(list = ls())
library(visNetwork)
# library(networkD3)
# library(network)
library(tidygraph)
library(ggraph)
library(readxl)

# node setting
dir<-getwd()
disease<-"Hodgkin" # Hodgkin, Lymphoid, Myelomonocytic_under12, Myelomonocytic_over12, Non-Hodgkin, total
limit=100

# subgroup 
sublab<-read.csv(paste(dir,"/data/210811_case,control_subgroup_summary.csv",sep=""),header=T)
sublab<-sublab %>% arrange(case_first_med,subgroup) %>% 
  group_by(subgroup) %>% mutate(rank=1:n(), label_rank=paste(label," [",rank,"]",sep=""))
sublab<-sublab %>% filter(subgroup==disease) %>% select(diag3,label_rank)

nodes<-read.csv(paste(dir,"/data/node_210611_",disease,".csv",sep=""),header=T)
# nodes<-read.csv(paste(dir,"/node_210611_Myelomonocytic_over12.csv",sep=""),header=T)
# nodes<-read.csv(paste(dir,"/node_210611_Myelomonocytic_under12.csv",sep=""),header=T)

nodes<-nodes[,c("Id","label")]
nodes<-merge(nodes,sublab,by.x=c("label"),by.y=c("label_rank"),all.x=T)

# first_diag total에 대해서는 데이터 없음!
nodesinfo<-read.csv(paste(dir,"/data/210615_case_survival_within_subgroup.csv",sep=""),header=T)
nodesinfo<-nodesinfo[,c("disease","node","significant","cand_n")]
names(nodesinfo)<-c("subgroup","node","group","value")
nodesinfo<-nodesinfo %>% filter(subgroup==disease) %>% select(-subgroup) # select(diag3,subgroup,label_rank)

##
nodes2<-merge(nodes,nodesinfo,by.x=c("diag3"),by.y=c("node"),all.x=T) %>% arrange(Id)
nodes2<-nodes2[,c("Id","label","group","value","diag3")]
nodes2$font.size = 30
#nodes2$group<-as.factor(substr(nodes2$diag3,1,1))

nodeval <- nodes2 %>% distinct(Id,value) %>% 
  rename(count=value)
#nodes<-nodes2

# Edges
edges<-read.csv(paste(dir,"/data/edge_210611_",disease,".csv",sep=""),header=T)
# edges<-read.csv(paste(dir,"/edge_210611_Myelomonocytic_over12.csv",sep=""),header=T)
# edges<-read.csv(paste(dir,"/edge_210611_Myelomonocytic_under12.csv",sep=""),header=T)
edges<-edges[,c("Source","Target","Weight")]

names(edges)<-c("from","to","weight")
edges$value<-exp(edges$weight)

#edges<-edges %>% filter(from %in% nodesId$Id|to %in% nodesId$Id)
nodeval<-nodeval %>% filter(count>=limit)
edges_m<-merge(edges,nodeval,by.x=c("to"),by.y=c("Id"),all.x = T) %>% rename(to_count=count)
edges<-merge(edges_m,nodeval,by.x=c("from"),by.y=c("Id"),all.x = T) %>% 
  rename(from_count=count) %>% filter(!is.na(to_count)&!is.na(from_count)) %>%
  select(-to_count,-from_count)

# count limit until 1000
# edges<-edges %>% filter(to_count>=limit|from_count>=limit) %>%
#   select(-to_count,-from_count)
uniq_node<-unique(c(edges$from,edges$to)); uniq_node

nodes2<-nodes2 %>% filter(Id %in% uniq_node) 
names(nodes2)[1]<-c("id")
#group<-unique(nodes2$group); length(group)

# nodes2$value=ifelse(is.na(nodes2$value),1,nodes2$value)
# nodes$group=ifelse(is.na(nodes$group!="significant"&nodes$group!="Notsig"),"Notsig",nodes$group)

# edges<-edges %>% filter(from!=10) # Non-Hodgkin에서 Id 10인 것이 문제됨!
# edges<-edges %>% filter(to!=18) # Myelomonocytic_under12에서 Id 18인 것이 문제됨!
# edges<-edges %>% filter(to!=c(6,16,18)) # Hodgkin에서 Id 6,16,18인 것이 문제됨!

#routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

# https://rdrr.io/cran/visNetwork/src/inst/examples/all_examples.R
set.seed(123456)
# network<-visNetwork(nodes2, edges, idToLabel=T,width="100%", height = "1000px") %>%
#   visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -10)) %>%
#   #visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE) %>% #freeze
#   visIgraphLayout(layout = 'layout_with_fr',type="full") %>% #visIgraphLayout(layout = "layout_with_sugiyama")
#   visNodes(color = list(highlight = "yellow",label_context="red",
#                         border = "grey", background = "lightblue"),borderWidth = 2, size=100,
#            scaling = list(label = list(enabled = TRUE))) %>% ##border = "grey", background = "lightblue",
#   visGroups(groupname = group[1]) %>% #,shadow = list(enabled = TRUE)
#   visGroups(groupname = group[2]) %>%
#   visGroups(groupname = group[3]) %>%
#   visGroups(groupname = group[4]) %>%
#   visGroups(groupname = group[5]) %>%
#   visGroups(groupname = group[6]) %>%
#   visGroups(groupname = group[7]) %>%
#   visGroups(groupname = group[8]) %>%
#   visGroups(groupname = group[9]) %>%
#   # visGroups(groupname = "Notsig", color = "lightblue", #shape = "square", 
#   #           shadow = list(enabled = TRUE)) %>% 
#   # visGroups(groupname = "significant", color = "yellow", shape = "triangle") %>% 
#   visLegend() %>%      
#   visEdges(arrows = "to", color=list(highlight="yellow"))
# network
#visSave(network, file = paste(dir,"/",disease,"_morecount_",limit,"freq_network.html",sep=""))
visSave(network, file = paste(dir,"/",disease,"_morecount_",limit,"freq_network_highlight_210623.html",sep=""))

network<-visNetwork(nodes2, edges, idToLabel=T,width="100%", height = "1000px") %>%
  visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -10)) %>%
  #visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE) %>% #freeze
  visIgraphLayout(layout = 'layout_with_fr',type="full") %>% #visIgraphLayout(layout = "layout_with_sugiyama")
  visNodes(color = list(highlight = "yellow",label_context="red"),borderWidth = 2, size=100,
           scaling = list(label = list(enabled = TRUE))) %>% ##border = "grey", background = "lightblue",
  visGroups(groupname = "J", color = "cadetblue", shape="triangle") %>% #,shadow = list(enabled = TRUE)
  visGroups(groupname = "H", color = "mistyrose") %>%
  visGroups(groupname = "S", color = "coral") %>%
  visGroups(groupname = "R", color = "mediumpurple") %>%
  visGroups(groupname = "K", color = "dodgerblue") %>%
  visGroups(groupname = "L", color = "darkseagreen") %>%
  visGroups(groupname = "M", color = "gold") %>%
  visGroups(groupname = "D", color = "darkorange") %>%
  visGroups(groupname = "I", color = "indianred") %>%
  #addFontAwesome() %>%
  visLegend() %>%
  visEdges(arrows = "to")#, color=list(highlight="yellow"))

network