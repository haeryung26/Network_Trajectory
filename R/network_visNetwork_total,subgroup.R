############################################# total graph 
library(visNetwork)
# library(networkD3); library(network)
library(tidygraph)
library(ggraph)
library(readxl)

# node setting
dir<-getwd()
disease<-"total" # Hodgkin, Lymphoid, Myelomonocytic_under12, Myelomonocytic_over12, Non-Hodgkin, total
# [1] Hodgkin                 Myelomonocytic_over_12  Lymphoid_over_6         Non-Hodgkin            
# [5] Myelomonocytic_under_12 total                   Lymphoid_under_6    

# subgroup 
sublab<-read.csv(paste(dir,"/data/adjusted/210811_case,control_subgroup_summary.csv",sep=""),header=T)
sublab<-sublab %>% arrange(case_first_med,subgroup) %>% 
  group_by(subgroup) %>% mutate(rank=1:n(), label_rank=paste(label," [",rank,"]",sep=""))
sublab<-sublab %>% filter(subgroup==disease) %>% select(diag3,label_rank)

# node - total
nodes<-read.csv(paste(dir,"/data/adjusted/node_total_RRmorethan10_210811.csv",sep=""),header=T)

nodes<-nodes[,c("Id","label")]
nodes<-merge(nodes,sublab,by.x=c("label"),by.y=c("label_rank"),all.x=T)

# first_diag total에 대해서는 데이터 없음!
# # total diagnosis
nodesinfo<-read.csv(paste(dir,"/data/adjusted/210729_diag_tot_diff_summary.csv",sep=""),header=T) %>%
  select(diag3,n) %>% mutate(group=substr(diag3,1,1)) %>% rename(value=n)

nodes2<-merge(nodes,nodesinfo,by=c("diag3"),all.x=T) %>% arrange(Id)
nodes2<-nodes2[,c("Id","label","group","value","diag3")]
nodes2$font.size = 50

nodeval <- nodes2 %>% distinct(Id,value) %>% 
  rename(count=value)

# Edges
edges<-read.csv(paste(dir,"/data/adjusted/edge_total_RRmorethan10_210811.csv",sep=""),header=T)
edges<-edges[,c("Source","Target","logWeight")]

names(edges)<-c("from","to","weight")
edges$value<-exp(edges$weight)

# count limit until 1000
limit=1000 # 300 -> 80nodes; 500 -> 65nodes; 1000 -> 39nodes
nodeval2<-nodeval %>% filter(count>=limit)#; dim(nodeval2)[[1]]
edges_m<-merge(edges,nodeval2,by.x=c("to"),by.y=c("Id"),all.x = T) %>% rename(to_count=count)
edges<-merge(edges_m,nodeval2,by.x=c("from"),by.y=c("Id"),all.x = T) %>% 
  rename(from_count=count) %>% filter(!is.na(to_count)&!is.na(from_count)) %>%
  select(-to_count,-from_count)

uniq_node<-unique(c(edges$from,edges$to))
uniq_node; length(uniq_node)

names(nodes2)[1]<-"Id"
nodes2<-nodes2 %>% filter(Id %in% uniq_node) 
names(nodes2)[1]<-"id"


# https://rdrr.io/cran/visNetwork/src/inst/examples/all_examples.R
set.seed(123456)

network<-visNetwork(nodes2, edges, idToLabel=T,width="100%", height = "1000px") %>%
  visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -10)) %>%
  visIgraphLayout(layout = 'layout_with_fr',type="full") %>% #visIgraphLayout(layout = "layout_with_sugiyama")
  visNodes(color = list(highlight = "yellow",label_context="red"),borderWidth = 2, size=100,
           scaling = list(label = list(enabled = TRUE))) %>% ##border = "grey", background = "lightblue",
  visOptions(highlightNearest = list(enabled=TRUE, labelOnly = TRUE, hover = TRUE,degree = list(from = 1, to = 1)),
           nodesIdSelection = FALSE, selectedBy ="group") %>%
  visLegend() %>%
  visEdges(arrows = "to", color=list(color="lightgrey",highlight="yellow"))
network

visSave(network, file = paste(dir,"/html/",disease,"_morecount_",limit,"freq_network_highlight_210813.html",sep=""))
#dim(nodes2)
# 300, 500, 1000

#################################################### subgroup graph 
library(visNetwork)
library(tidygraph)
library(ggraph)
library(readxl)

# node setting
dir<-getwd()
disease<-"Lymphoid_under_6"
# [1] Hodgkin                 Myelomonocytic_over_12  Lymphoid_over_6         Non-Hodgkin            
# [5] Myelomonocytic_under_12 total                   Lymphoid_under_6    

gr<-"Lymphoid"
# [1] Hodgkin                Lymphoid               Myelomonocytic         Myelomonocytic_over12 
# [5] Myelomonocytic_under12 Non-Hodgkin   

# subgroup 
sublab<-read.csv(paste(dir,"/data/adjusted/210811_case,control_subgroup_summary.csv",sep=""),header=T)
sublab<-sublab %>% arrange(case_first_med,subgroup) %>% 
  group_by(subgroup) %>% mutate(rank=1:n(), label_rank=paste(label," [",rank,"]",sep=""))
sublab<-sublab %>% filter(subgroup==disease) %>% select(diag3,label_rank)

# node - subgroup
nodes<-read.csv(paste(dir,"/data/adjusted/node_210811_",disease,".csv",sep=""),header=T)
nodes<-nodes[,c("Id","label")]
nodes<-merge(nodes,sublab,by.x=c("label"),by.y=c("label_rank"),all.x=T)

nodesinfo<-read.csv(paste(dir,"/data/210615_case_survival_within_subgroup.csv",sep=""),header=T) %>%
  rename(subgroup=disease) %>%
  filter(subgroup==gr) %>%
  select(node,cand_n) %>% mutate(group=substr(node,1,1)) %>% rename(value=cand_n)

##
nodes2<-merge(nodes,nodesinfo,by.x=c("diag3"),by.y=c("node"),all.x=T) %>% arrange(Id)
#nodes2<-merge(nodes,nodesinfo,by=c("diag3"),all.x=T) %>% arrange(Id)
nodes2<-nodes2[,c("Id","label","group","value","diag3")]
nodes2$font.size = 50

nodeval <- nodes2 %>% distinct(Id,value) %>% 
  rename(count=value) %>% 
  mutate(count=ifelse(is.na(count),1,count))

# Edges
edges<-read.csv(paste(dir,"/data/adjusted/edge_210811_",disease,".csv",sep=""),header=T)
edges<-edges[,c("Source","Target","Weight")]

names(edges)<-c("from","to","weight")
edges$value<-exp(edges$weight)

# count limit until 1000
limit=0 # 300 -> 80 nodes; 500 -> 65 nodes; 1000 -> 39nodes
nodeval
nodeval2<-nodeval %>% filter(count>=limit); dim(nodeval2)[[1]]
edges_m<-merge(edges,nodeval2,by.x=c("to"),by.y=c("Id"),all.x = T) %>% rename(to_count=count)
edges2<-merge(edges_m,nodeval2,by.x=c("from"),by.y=c("Id"),all.x = T) %>% 
  rename(from_count=count) %>% filter(!is.na(to_count)&!is.na(from_count)) %>%
  select(-to_count,-from_count)

uniq_node<-unique(c(edges2$from,edges2$to))
uniq_node; length(uniq_node)

names(nodes2)[1]<-"Id"
nodes2<-nodes2 %>% filter(Id %in% uniq_node) 
names(nodes2)[1]<-"id"

nodes2 <- nodes2 %>%
  mutate(value=ifelse(is.na(value),10,value), group=ifelse(is.na(group),substr(diag3,1,1),group))

# https://rdrr.io/cran/visNetwork/src/inst/examples/all_examples.R
# https://www.rpubs.com/gregt95/dataviz2
set.seed(123456)
network<-visNetwork(nodes2, edges2, idToLabel=T,width="100%", height = "1000px") %>%
  visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -10)) %>%
  visIgraphLayout(layout = 'layout_with_fr',type="full") %>% #visIgraphLayout(layout = "layout_with_sugiyama")
  visNodes(color = list(highlight = "yellow",label_context="red"),borderWidth = 2, size=100,
           scaling = list(label = list(enabled = TRUE))) %>% ##border = "grey", background = "lightblue",
  visOptions(highlightNearest = list(enabled=TRUE, labelOnly = TRUE, hover = TRUE,degree = list(from = 1, to = 1)),
             nodesIdSelection = FALSE, selectedBy ="group") %>%
  visLegend() %>%
  visEdges(arrows = "to", color=list(color="lightgrey",highlight="yellow"))
network
visSave(network, file = paste(dir,"/html/",disease,"_morecount_",limit,"freq_network_highlight_210813.html",sep=""))
