
#library(igraph) #you can use igraph if you prefer for data processing
setwd("/Users/priestleyfernandes/Downloads")
library(sna)
library(ergm)
library(tidyverse)

#####################################################
##Read files

#Connections
g1 <- read.csv(file = "freq_com_new_2015_2.csv", sep = ",", header=TRUE) #graph modelling language
g2 <- read.csv(file = "freq_com_new_2016_2.csv", sep = ",",  header=TRUE) #graph modelling language
g3 <- read.csv(file = "freq_com_new_2017_2.csv", sep = ",",  header=TRUE) #graph modelling language

#Anxiety
a1 <- read.csv(file = "anxiety_com_new_2015_2.csv", sep = ",",  header=TRUE) #graph modelling language
a2 <- read.csv(file = "anxiety_com_new_2016_2.csv", sep = ",",  header=TRUE) #graph modelling language
a3 <- read.csv(file = "anxiety_com_new_2017_2.csv", sep = ",",  header=TRUE) #graph modelling language

#Displaying the data
print(retrieval)
print(allocation)

######################################################
#View data 
#Conversion into matrices
######################################################
#Connections
#2015
g1
g1_mat <- as.matrix(g1[,2:3])
g1_mat
mediag1<-graph_from_edgelist(g1_mat,directed=T)
g1_mat <- as.matrix(mediag1)
g1_mat
class(mediag1)
g1_mat_sorted <- g1_mat[order(rownames(g1_mat)),]
g1_mat_sorted


#2016
g2
nrow(g2)
g2_mat <- as.matrix(g2[,2:3])
g2_mat
mediag2<-graph_from_edgelist(g2_mat,directed=T)
mediag2 <- add_vertices(mediag2,1,name = 'badkarma')
V(mediag2)
g2_mat <- as.matrix(mediag2)
g2_mat

g2_mat_sorted <- g2_mat[order(rownames(g2_mat)),]
g2_mat_sorted

#2017
g3
g3_mat <- as.matrix(g3[,2:3])
g3_mat
mediag3<-graph_from_edgelist(g3_mat,directed=T)
mediag3 <- add_vertices(mediag3,1,name = 'badkarma')
write_graph(mediag3, 'connection_2017.gml', format = 'gml')
g3_mat <- as.matrix(mediag3)
g3_mat

g3_mat_sorted <- g3_mat[order(rownames(g3_mat)),]
g3_mat_sorted

#Anxiety
#2015

a1
a1_mat <- as.matrix(a1[,2:3])
a1_mat
mediaga1<-graph_from_edgelist(a1_mat,directed=T)
write_graph(mediaga1, 'anxiety_2015.gml', format = 'gml')
plot(mediaga1)

a1_mat <- as.matrix(mediaga1)
a1_mat

a1_mat_sorted <- a1_mat[order(rownames(a1_mat)),]
a1_mat_sorted

#2016
a2
a2_mat <- as.matrix(a2[,2:3])
a2_mat
mediaga2<-graph_from_edgelist(a2_mat,directed=T)
write_graph(mediaga2, 'anxiety_2016.gml', format = 'gml')
a2_mat <- as.matrix(mediaga2)
a2_mat

a2_mat_sorted <- a2_mat[order(rownames(a2_mat)),]
a2_mat_sorted

#2017
a3_mat <- as.matrix(a3[,2:3])
a3_mat
mediaga3<-graph_from_edgelist(a3_mat,directed=T)
mediaga3 <- add_vertices(mediaga3,2,name = c('badkarma','sixers'))
write_graph(mediaga3, 'anxiety_2017.gml', format = 'gml')
a3_mat <- as.matrix(mediaga3)
a3_mat

a3_mat_sorted <- a3_mat[order(rownames(a3_mat)),]
a3_mat_sorted

#Creation on node sets for comparison
node_set_1 <- V(mediag1)
length(node_set_1)
node_set_2 <- V(mediag2)
length(node_set_2)
node_set_3 <- V(mediag3)
length(node_set_3)
node_set_4 <- V(mediaga1)
length(node_set_4)
node_set_5 <- V(mediaga2)
length(node_set_5)
node_set_6 <- V(mediaga3)
length(node_set_6)

#Noxde check
x <- Reduce(intersect, list(node_set_1, node_set_2, node_set_3, node_set_4, node_set_5, node_set_6))     # Identify common elements
# "A" "D"
length(x)

############################################################
#ERGM model

#Creation of vafriables
#Conection
frequency_data_2015 <- g1_mat_sorted
#frequency_data_2015_net <- as.network(frequency_data_2015)
frequency_data_2016 <- g2_mat_sorted
#frequency_data_2016_net <- as.network(frequency_data_2016)
frequency_data_2017 <- g3_mat_sorted
#frequency_data_2017_net <- as.network(frequency_data_2017)
frequency_data_2015
#Anxiety
anxiety_data_2015 <- a1_mat_sorted
anxiety_data_2016 <- a2_mat_sorted
anxiety_data_2017 <- a3_mat_sorted


#Converting into a compatible matrix object

frequency_data_2015 <- matrix(frequency_data_2015, nrow = 372, ncol = 372)
frequency_data_2016 <- matrix(frequency_data_2016, nrow = 372, ncol = 372)
frequency_data_2017 <- matrix(frequency_data_2017, nrow = 372, ncol = 372)

anxiety_data_2015_mat <- matrix(anxiety_data_2015, nrow = 372, ncol = 372)
anxiety_data_2016_mat <- matrix(anxiety_data_2016, nrow = 372, ncol = 372)
anxiety_data_2017_mat <- matrix(anxiety_data_2017, nrow = 372, ncol = 372)

dim
###################################################

##ERGM Analysis

###2015

library(parallel)
detectCores()
#ERGM ANALYSIS

#############
#2015
con_anx_2015 <- ergm(frequency_data_2015 ~ edges + gwidegree(cutoff = 200) + gwesp(0.25, fixed = T) + edgecov(anxiety_data_2015_mat), control = control.ergm(seed = 1, parallel = 6, parallel.type = "PSOCK"))       
con_anx_2015
coef(con_anx_2015)
summary(con_anx_2015)


con_anx_2015_1 <- ergm(frequency_data_2015 ~ edges + edgecov(anxiety_data_2015_mat), control = control.ergm(seed = 1, parallel = 6, parallel.type = "PSOCK"))       
con_anx_2015_1
coef(con_anx_2015_1)
summary(con_anx_2015_1)

gof_con_anx_2015_1 <- gof(con_anx_2015_1)
gof_con_anx_2015_1


con_anx_2015_2 <- ergm(frequency_data_2015 ~ edges + gwidegree(cutoff = 200) + edgecov(anxiety_data_2015_mat), control = control.ergm(seed = 1, parallel = 6, parallel.type = "PSOCK"))       
con_anx_2015_2
summary(con_anx_2015_2)

gof_con_anx_2015_2 <- gof(con_anx_2015_2)
gof_con_anx_2015_2


con_anx_2015_3 <- ergm(frequency_data_2015 ~ edges + gwesp(0.25, fixed = T) + edgecov(anxiety_data_2015_mat), control = control.ergm(seed = 1, parallel = 6, parallel.type = "PSOCK"))       
con_anx_2015_3
summary(con_anx_2015_3)

gof_con_anx_2015_3 <- gof(con_anx_2015_3)
gof_con_anx_2015_3


###################
#2016
con_anx_2016_2_1 <- ergm(frequency_data_2016 ~ edges + gwidegree(cutoff = 200) + edgecov(anxiety_data_2015_mat), control = control.ergm(seed = 1, parallel = 6, parallel.type = "PSOCK"))       
con_anx_2015_2
summary(con_anx_2015_2)


##########################
##Analysis of 2017 data 

con_anx_2017_2_1 <- ergm(frequency_data_2017 ~ edges + gwidegree(cutoff = 200) + edgecov(anxiety_data_2015_mat) + edgecov(anxiety_data_2016_mat), control = control.ergm(seed = 1, parallel = 6, parallel.type = "PSOCK"))       
con_anx_2017_2_1
summary(con_anx_2017_2_1)


con_anx_2017_2_2 <- ergm(frequency_data_2017 ~ edges + gwidegree(cutoff = 200) + edgecov(anxiety_data_2015_mat) + edgecov(anxiety_data_2016_mat) + edgecov(anxiety_data_2017_mat), control = control.ergm(seed = 1, parallel = 6, parallel.type = "PSOCK"))       
con_anx_2017_2_2
summary(con_anx_2017_2_2) 


con_anx_2017_2_3 <- ergm(frequency_data_2017 ~ edges + gwidegree(cutoff = 200) + edgecov(anxiety_data_2015_mat) + edgecov(anxiety_data_2016_mat) + edgecov(anxiety_data_2017_mat) + edgecov(frequency_data_2016) , control = control.ergm(seed = 1, parallel = 6, parallel.type = "PSOCK"))       
con_anx_2017_2_3
summary(con_anx_2017_2_3) 


con_anx_2017_2_4 <- ergm(frequency_data_2017 ~ edges + + gwidegree(cutoff = 200) + edgecov(anxiety_data_2015_mat) + edgecov(anxiety_data_2016_mat) + edgecov(anxiety_data_2017_mat) + edgecov(frequency_data_2016) + edgecov(frequency_data_2015), control = control.ergm(seed = 1, parallel = 6, parallel.type = "PSOCK"))       
con_anx_2017_2_4
summary(con_anx_2017_2_4) 
gofcon_anx_2017_2_4 <- gof(con_anx_2017_2_4)
gofcon_anx_2017_2_4


con_anx_2017_2_5 <- ergm(frequency_data_2017 ~ edges + + gwidegree(cutoff = 200) + edgecov(anxiety_data_2015_mat) + edgecov(anxiety_data_2016_mat) + edgecov(anxiety_data_2017_mat) + edgecov(frequency_data_2016) + edgecov(frequency_data_2015), control = control.ergm(seed = 1, parallel = 6, parallel.type = "PSOCK"))       
con_anx_2017_2_5
summary(con_anx_2017_2_5) 
gof_con_anx_2017_2_5 <- gof(con_anx_2017_2_5)
gof_con_anx_2017_2_5


con_anx_2017_2_6 <- ergm(frequency_data_2017 ~ edges + gwesp(0.25, fixed = T) + gwidegree(cutoff = 200) + edgecov(anxiety_data_2015_mat) + edgecov(anxiety_data_2016_mat) + edgecov(anxiety_data_2017_mat) + edgecov(frequency_data_2015) + edgecov(frequency_data_2016), control = control.ergm(seed = 1, parallel = 6, parallel.type = "PSOCK"))       
con_anx_2017_2_6
summary(con_anx_2017_2_6) 


con_anx_2017_2_7 <- ergm(frequency_data_2017 ~ edges + gwidegree(cutoff = 200) + edgecov(anxiety_data_2015_mat) + edgecov(anxiety_data_2016_mat) + edgecov(anxiety_data_2017_mat) + edgecov(frequency_data_2015) , control = control.ergm(seed = 1, parallel = 6, parallel.type = "PSOCK"))       
con_anx_2017_2_7
summary(con_anx_2017_2_7) 

gof_con_anx_2017_2_7 <- gof(con_anx_2017_2_7)
summary(gof_con_anx_2017_2_7)
gof_con_anx_2017_2_7

par(mfrow=c(3,2))
par(mar = c(1, 1, 1, 1))
plot(gof_con_anx_2017_2_7) \







