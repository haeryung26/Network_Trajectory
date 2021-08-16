# install.packages("pivottabler", repos="http://10.1.61.87:8080/cran", type="win.binary")
# install.packages("ggplot2", repos="http://10.1.61.87:8080/cran", type="win.binary")
# install.packages("reshape2", repos="http://10.1.61.87:8080/cran", type="win.binary")
# install.packages("dplyr", repos="http://10.1.61.87:8080/cran", type="win.binary")
# install.packages("viridis", repos="http://10.1.61.87:8080/cran", type="win.binary")
# install.packages("hrbrthemes", repos="http://10.1.61.87:8080/cran", type="win.binary")
# install.packages("tidyverse", repos="http://10.1.61.87:8080/cran", type="win.binary")
# install.packages("ggpurb", repos="http://10.1.61.87:8080/cran", type="win.binary")
# install.packages("rstatix", repos="http://10.1.61.87:8080/cran", type="win.binary")
# install.packages("table1", repos="http://10.1.61.87:8080/cran",type="win.binary")
# install.packages("scatterplot3d", repos="http://10.1.61.87:8080/cran",type="win.binary")
# install.packages("Rtsne", repos="http://10.1.61.87:8080/cran",type="win.binary")
# install.packages("cowplot", repos="http://10.1.61.87:8080/cran",type="win.binary")
# install.packages("ggpubr", repos="http://10.1.61.87:8080/cran", type="win.binary")
# install.packages("gtable", repos="http://10.1.61.87:8080/cran", type="win.binary")
# install.packages("grid", repos="http://10.1.61.87:8080/cran", type="win.binary")

library(ggplot2); library(viridis); library(hrbrthemes); library(pivottabler)
library(reshape2); library(dplyr); library(tidyverse); library(rstatix)
library(stats); library(table1); library(scatterplot3d); library(Rtsne); library(grid)
library(readxl); library(tidyr); library(cowplot); library(ggpubr); library(gtable)


### stacked density plot example (Just follow this code)
set.seed(1234)
wdata = data.frame(
  sex = factor(rep(c("F", "M"), each=200)),
  weight = c(rnorm(200, 55), rnorm(200, 58)))
head(wdata)

# 1. Create the histogram plot
phist <- gghistogram(
  wdata, x = "weight", 
  add = "median", rug = TRUE, lwd = 1, #legend="right",
  fill = "sex"#, palette = c("#00AFBB", "#E7B800")
)

# 2. Create the density plot with y-axis on the right
# Remove x axis elements
pdensity <- ggdensity(
  wdata, x = "weight", 
  color= "sex", lwd = 1, #palette = c("#00AFBB", "#E7B800"), 
  alpha = 0
) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), position = "right")  +
  theme_half_open(11, rel_small = 1) +
  rremove("x.axis")+
  rremove("xlab") +
  rremove("x.text") +
  rremove("x.ticks") +
  rremove("legend")

# 3. Align the two plots and then overlay them.
aligned_plots <- align_plots(phist, pdensity, align="hv", axis="tblr")
aligned_plot2 <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])


##########################################################################
data(diamonds)
p2<-ggplot(data=diamonds, aes(x=price, group=cut, fill=cut))+
  geom_density(adjust=1.5, alpha=.4)+
  theme_ipsum()

annot <- data.frame(
  text=c("Fair","Good","Very Good","Premium","Ideal"),
  x=c(100,500,1000,1200,1500),
  y=c(0.15,0.4,0.06,0.1,0.2)
)

############ Apply to our data with stacked plot####
case_person<-read.csv("Z:/-+raw/case_beforediag_uniqdiag_per_person.csv",header=T) 
cont_person<-read.csv("Z:/-+raw/cont_beforediag_uniqdiag_per_person.csv",header=T) 
subgroup<-read.csv("Z:/-+preproc/casecont_subgroup.csv",header=T)
names(subgroup)[4]<-"onset_age"


##### Find top10 by group
stackeddat<- merge(subgroup,case_person,by=c("INDI_DSCM_NO"),all.x=T) %>% 
  filter(!is.na(match_id)) %>%
  select(cat,onset_age,diag3,diff) #%>% mutate(diag1=substr(diag3,1,1)) 
stackeddat$cat2<-ifelse(stackeddat$cat=="Myelomonocytic"&stackeddat$onset_age<12,"Myelomonocytic_under12",
                 ifelse(stackeddat$cat=="Myelomonocytic"&stackeddat$onset_age>=12,"Myelomonocytic_morethan12",
                 ifelse(stackeddat$cat=="Lymphoid"&stackeddat$onset_age<6,"Lymphoid_under6",
                 ifelse(stackeddat$cat=="Lymphoid"&stackeddat$onset_age>=6,"Lymphoid_morethan6",
                        ifelse(stackeddat$cat=="Non-Hodgkin","Non-Hodgkin","Hodgkin")))))

stat_ca <- as.data.frame(stackeddat %>%
                         group_by(diag3,cat2) %>% 
                         summarise(n=n())) %>% arrange(cat2,desc(n))

# top10 by group
stat_ca2 <-as.data.frame(stat_ca %>%
                           arrange_(~desc(n)) %>%
                           group_by_(~cat2) %>% 
                           slice(1:10) %>% mutate(cand="Y") %>% 
                           select(-n))
stackeddat<-merge(stat_ca2,stackeddat,by=c("cat2","diag3"),all.x = TRUE)


### Stacked graph (Lymphoid_morethan6)
# 0. make data
cat<-unique(stackeddat$cat2)
# [1] "Hodgkin"                   "Lymphoid_morethan6"        "Lymphoid_under6"          
# [4] "Myelomonocytic_morethan12" "Myelomonocytic_under12"    "Non-Hodgkin" 
dat<-stackeddat %>% filter(stackeddat$cat2=="Lymphoid_morethan6"&
                             diag3 %in% c("J03","J20","J06","J02","J01",
                                          "J30","J00","H10","J45","A09"))

# 1. make gghistogram
phist<-gghistogram(dat,
                   x="diff",
                   add="median",rug=TRUE, lwd=1, fill="diag3")

# 2. Create the density plot with y-axis on the right
pdensity <- ggdensity(
  dat, x="diff", 
  color= "diag3", lwd = 1, #palette = c("#00AFBB", "#E7B800"), 
  alpha = 0
) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), position = "right")  +
  theme_half_open(11, rel_small = 1) +
  rremove("x.axis")+
  rremove("xlab") +
  rremove("x.text") +
  rremove("x.ticks") +
  rremove("legend")

# 3. Align the two plots and then overlay them.
aligned_plots <- align_plots(phist, pdensity, align="hv", axis="tblr")
Lymphoid_more6_aligned <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])


##### stacked graph by row
g2 <- ggplotGrob(Lymphoid_under6_aligned) 
#g3 <- ggplotGrob(phist)
#g4 <- ggplotGrob(pdensity)
g <- rbind(g2, g3, g4, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths,g4$widths)
grid.newpage()
grid.draw(g)