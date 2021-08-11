# For total
library(dplyr)
library(readxl)
dir<-getwd()

mydata<-read.csv(paste(dir,"/data/adjusted/210729_firstdiag_tot_RR_chi_significant.csv",sep=""),header=T)
sublab<-read.csv(paste(dir,"/data/adjusted/210811_case,control_subgroup_summary.csv",sep=""),header=T)
sublab<-sublab %>% filter(subgroup=="total") %>% arrange(case_first_med,subgroup) %>% 
  group_by(subgroup) %>% mutate(rank=1:n(), label_rank=paste(label," [",rank,"]",sep="")) %>%
  select(diag3,case_first_med,label_rank,rank,-subgroup)

mydata<-merge(mydata,sublab,by.x=c("seq2"),by.y = c("diag3"),all.x = T) %>% 
  rename(seq2_median=case_first_med,seq2_label_rank=label_rank)
mydata<-merge(mydata,sublab,by.x=c("seq1"),by.y = c("diag3"),all.x = T) %>% 
  rename(seq1_median=case_first_med,seq1_label_rank=label_rank)

names(mydata)[c(3,5,11,14)]<-c("caserec","contrec","seq2_median","seq1_median")
mydata<-mydata %>% mutate(timeseq=ifelse(seq1_median<=seq2_median,"TRUE","FALSE"))

# Full node label (nodelab)
# seq1<-mydata[,c("seq1","seq1_label")]; names(seq1)<-c("seq","label")
# seq2<-mydata[,c("seq2","seq2_label")]; names(seq2)<-c("seq","label")
# nodelab<-rbind(seq1,seq2) %>% arrange(seq,label) %>% distinct(seq,label, .keep_all = TRUE) 

# make a node (timeseq=="TRUE"&RR>=10)
mydata <- mydata %>% 
  filter(timeseq=="TRUE"&RR>=1) %>% #filter(timeseq=="TRUE"&contrec>=100)
  select(seq1,seq2,seq1_label_rank,seq2_label_rank,seq1_median,seq2_median,RR)

seq1<-mydata[,c("seq1","seq1_label_rank")]; names(seq1)<-c("seq","label")
seq2<-mydata[,c("seq2","seq2_label_rank")]; names(seq2)<-c("seq","label")

normalize <- function(x, na.rm = T) (x  / max(x, na.rm = T))

node<-rbind(seq1,seq2) %>% arrange(seq,label) %>% 
  distinct(seq,label, .keep_all = TRUE) 

### export node
node<-merge(node,sublab,by.x=c("seq"),by.y = c("diag3"),all.x = T) %>% 
  #select(-diag_timediff_med_index) %>% 
  mutate_at('rank',normalize) %>% 
  arrange(rank) %>%
  mutate(Id = 1:n(),Interval="",Category="Disease") %>% 
  rename(ClossnessCentrality=rank)

node2<-node[,c("Id","label","Interval","Category","ClossnessCentrality")]
write.csv(node2,paste(dir,"/data/adjusted/node_total_RRmorethan10_210811.csv",sep=""))

# make an edge
mydata <- merge(mydata,node,by.x=c("seq1"),by.y=c("seq"),all.x=T)
mydata <- merge(mydata,node,by.x=c("seq2"),by.y=c("seq"),all.x=T)

edge <-mydata %>% select(Id.x,Id.y,RR) %>% 
  rename(Source=Id.x,Target=Id.y,Weight=RR) %>% 
  mutate(Type="Directed",Label="",Interval="",logWeight=log(Weight)) %>% 
  mutate(Id=1:n(), Id=Id-1)

edge <- edge[,c("Source","Target","Type","Id","Label","Interval","logWeight")] 
write.csv(edge,paste(dir,"/data/adjusted/edge_total_RRmorethan10_210811.csv",sep=""))

#################################### subgroup #############################
# subgroup
disease<-"Hodgkin" # Hodgkin, Lymphoid, Myelomonocytic_under12, Myelomonocytic_over12, Non-Hodgkin, total
limit=100

disease<-"Non-Hodgkin" #Hodgkin, Lymphoid, Myolomonocytic_under12, Myolomonocytic_over12, Non-Hodgkin, first_total

sub<-as.data.frame(read_excel("/home/haereong/ATLAS/Network_traj/210611_subgroup_RR_chi_significant_preprocess.xlsx",sheet = disease)) 
sub<-sub %>% select(-tsite)
sublab<-read.csv("/home/haereong/ATLAS/Network_traj/210611_case,control_subgroup_summary.csv",header = T)
sublab<-sublab %>% filter(subgroup==disease) %>% select(-X,-label,-subgroup,-case_first_med)#select(diag3,subgroup,label_rank)

# make a node (contrec>=100; RR>=10)
mydata <- merge(sub,sublab,by.x=c("seq2"),by.y=c("diag3"),all.x = T) %>%
  rename(seq2_rank=rank,seq2_label_rank=label_rank)
mydata <-merge(mydata,sublab,by.x=c("seq1"),by.y=c("diag3"),all.x = T) %>% 
  rename(seq1_rank=rank,seq1_label_rank=label_rank) %>%
  mutate(timeseq=ifelse(seq1_rank<=seq2_rank,"TRUE","FALSE")) %>% 
  filter(timeseq=="TRUE"&RR>=1) %>% #&corr_pval<0.05) %>% #filter(timeseq=="TRUE"&contrec>=100)
  select(seq1,seq2,seq1_label_rank,seq2_label_rank,seq1_rank,seq2_rank,RR)

seq1<-mydata[,c("seq1","seq1_label_rank","seq1_rank")]; names(seq1)<-c("seq","label","rank")
seq2<-mydata[,c("seq2","seq2_label_rank","seq2_rank")]; names(seq2)<-c("seq","label","rank")

node<-rbind(seq1,seq2) %>% arrange(seq,label) %>% 
  distinct(seq,label,rank, .keep_all = TRUE) 

### export node
normalize <- function(x, na.rm = T) (x  / max(x, na.rm = T))

node<-node %>% #merge(node,timerank,by.x=c("seq"),by.y = c("diag3"),all.x = T) %>% select(-X,-diag_timediff_med_index) %>% 
  mutate(timerank=normalize(rank)) %>% 
  arrange(timerank) %>%
  mutate(Id = 1:n(),Interval="",Category=disease) %>% 
  rename(ClossnessCentrality=timerank)

node2<-node[,c("Id","label","Interval","Category","ClossnessCentrality")]
write.csv(node2,paste("/home/haereong/ATLAS/Network_traj/node_210611_",disease,".csv",sep=""),row.names = FALSE)

# make an edge
node <- node %>% select(seq,Id)
mydata <- merge(mydata,node,by.x=c("seq1"),by.y=c("seq"),all.x=T)
mydata <- merge(mydata,node,by.x=c("seq2"),by.y=c("seq"),all.x=T)

edge <-mydata %>% 
  select(Id.x,Id.y,RR) %>% 
  rename(Source=Id.x,Target=Id.y,Weight=RR) %>% 
  mutate(Type="Directed",Label="",Interval="",Weight=log(Weight)) %>% 
  mutate(Id=1:n(), Id=Id-1)

edge <- edge[,c("Source","Target","Type","Id","Label","Interval","Weight")] 
write.csv(edge,paste("/home/haereong/ATLAS/Network_traj/edge_210611_",disease,".csv",sep=""),row.names = FALSE)

head(node2); dim(node2)[[1]]
head(edge); dim(edge)[[1]]
