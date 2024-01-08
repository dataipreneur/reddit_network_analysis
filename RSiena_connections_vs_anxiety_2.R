install.packages("RSiena")
library("RSiena")
library("network")
library( sna )
install.packages("igraph")
library(igraph)
install.packages("tidyverse")
library(tidyverse) 
install.packages("spam")
library("spam")

#####################################
getwd()
setwd('/Users/priestleyfernandes/Downloads')

#######################################

##Read files

#Connections
g1 <- read.csv(file = "freq_com_new_2015_2.csv", sep = ",", header=TRUE) #graph modelling language
g2 <- read.csv(file = "freq_com_new_2016_2.csv", sep = ",",  header=TRUE) #graph modelling language
g3 <- read.csv(file = "freq_com_new_2017_2.csv", sep = ",",  header=TRUE) #graph modelling language

#Anxiety
a1 <- read.csv(file = "anxiety_com_new_2015_2.csv", sep = ",",  header=TRUE) #graph modelling language
a2 <- read.csv(file = "anxiety_com_new_2016_2.csv", sep = ",",  header=TRUE) #graph modelling language
a3 <- read.csv(file = "anxiety_com_new_2017_2.csv", sep = ",",  header=TRUE) #graph modelling language

#View data 
#Conversion into matrices

#Connections
#2015
g1
g1_mat <- as.matrix(g1[,2:3])
g1_mat
mediag1<-graph_from_edgelist(g1_mat,directed=T)
g1_mat <- as.matrix(mediag1)
g1_mat
class(mediag1)

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

#2017
g3
g3_mat <- as.matrix(g3[,2:3])
g3_mat
mediag3<-graph_from_edgelist(g3_mat,directed=T)
mediag3 <- add_vertices(mediag3,1,name = 'badkarma')
write_graph(mediag3, 'connection_2017.gml', format = 'gml')
g3_mat <- as.matrix(mediag3)
g3_mat


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


#2016
a2
a2_mat <- as.matrix(a2[,2:3])
a2_mat
mediaga2<-graph_from_edgelist(a2_mat,directed=T)
write_graph(mediaga2, 'anxiety_2016.gml', format = 'gml')
a2_mat <- as.matrix(mediaga2)
a2_mat

#2017
a3_mat <- as.matrix(a3[,2:3])
a3_mat
mediaga3<-graph_from_edgelist(a3_mat,directed=T)
mediaga3 <- add_vertices(mediaga3,2,name = c('badkarma','sixers'))
write_graph(mediaga3, 'anxiety_2017.gml', format = 'gml')
a3_mat <- as.matrix(mediaga3)
a3_mat

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


#Siena Model

#Creation of vafriables
#Conection
frequency_data_2015 <- g1_mat
#frequency_data_2015_net <- as.network(frequency_data_2015)
frequency_data_2016 <- g2_mat
#frequency_data_2016_net <- as.network(frequency_data_2016)
frequency_data_2017 <- g3_mat
#frequency_data_2017_net <- as.network(frequency_data_2017)

#Anxiety
anxiety_data_2015 <- a1_mat
anxiety_data_2016 <- a2_mat
anxiety_data_2017 <- a3_mat

class(frequency_data_2015)
class(frequency_data_2016)
class(frequency_data_2017)
dim(frequency_data_2015)
dim(frequency_data_2016)
dim(frequency_data_2017)

#Converting into a compatible matrix object

frequency_data_2015 <- matrix(frequency_data_2015, nrow = 372, ncol = 372)
frequency_data_2016 <- matrix(frequency_data_2016, nrow = 372, ncol = 372)
frequency_data_2017 <- matrix(frequency_data_2017, nrow = 372, ncol = 372)

anxiety_data_2015_mat <- matrix(anxiety_data_2015, nrow = 372, ncol = 372)
anxiety_data_2016_mat <- matrix(anxiety_data_2016, nrow = 372, ncol = 372)
anxiety_data_2017_mat <- matrix(anxiety_data_2017, nrow = 372, ncol = 372)

class(anxiety_data_2015)
class(frequency_data_2015)


# Dependent variable
#######################################################

#Connection Network: The dependent network
#Dimension 372 nodes
connections <- array( c( frequency_data_2015, frequency_data_2016, frequency_data_2017),
                         dim = c( 372, 372, 3 ) )
connections

#Creation of Siena dependent variable
connections <- sienaDependent(connections)

dim(connections)
str(connections)


#Network 2: The Anxiety network as Varying Dyad Covariate

anxiety <- array( c( anxiety_data_2015_mat, anxiety_data_2016_mat, anxiety_data_2017_mat),
                      dim = c( 372, 372, 2 ))

#Creayion of varDyadCovar
anxiety_var <- varDyadCovar(anxiety)

#Cross Check
anxiety_var
connections


####################################################
#Creation of Siena Data by combing the two
mydata <- sienaDataCreate(connections, anxiety_var)
mydata

print01Report( mydata, modelname="connectionVsanxiety")

###################################################
#Effects

myeff <- getEffects( mydata)

effectsDocumentation(myeff)

#Inclusion of effects 
?includeEffects
myeff <- includeEffects(myeff, transTrip)

myeff

#Crreate Algorithm
?sienaAlgorithmCreate
myalgorithm <- sienaAlgorithmCreate( projname = 'connections_vs_anxiety', seed=435123, cond = FALSE)

ans1 <- siena07( myalgorithm, data = mydata, effects = myeff)
ans1
summary(ans1)
ans1$tconv.max
ans <- siena07( myalgorithm, data = mydata, effects = myeff, prevAns=ans1)
ans
summary(ans)


#####################################################
#Experiment 1

myeff1 <- includeEffects( myeff, transTrip, cycle3)
# and some covariate effects:
myeff1 <- includeEffects(XRecip, interaction1 = "anxiety_var" )
#myeff <- includeEffects( myeff, simX, interaction1 = "smoke1" )
myeff1

#Crreate Algorithm
myalgorithm <- sienaAlgorithmCreate( projname = 'connections_vs_anxiety', useStdInits = TRUE)
ans1 <- siena07( myalgorithm, data = mydata, effects = myeff1)
ans1

summary(ans1)




#####################################################
#Experiment 2

myeff1 <- includeEffects( myeff, transTrip, cycle3)
# and some covariate effects:
myeff2 <- includeEffects(XRecip, interaction1 = "anxiety_var" )
#myeff <- includeEffects( myeff, simX, interaction1 = "smoke1" )
myeff2

#Crreate Algorithm
?sienaAlgorithmCreate
myalgorithm <- sienaAlgorithmCreate( projname = 'connections_vs_anxiety' )

ans2 <- siena07( myalgorithm, data = mydata, effects = myeff2)
ans2

summary(ans2)


#####################################################
#Experiment 3


myeff3 <- includeEffects( myeff, transTrip, cycle3)
# and some covariate effects:
myeff3 <- includeEffects(XRecip, interaction1 = "anxiety_var" )
#myeff <- includeEffects( myeff, simX, interaction1 = "smoke1" )
myeff3

#Crreate Algorithm
?sienaAlgorithmCreate
myalgorithm <- sienaAlgorithmCreate( projname = 'connections_vs_anxiety' )

ans3 <- siena07( myalgorithm, data = mydata, effects = myeff3)
ans3

summary(ans3)


#####################################################
#Experiment 4

myeff <- includeEffects( myeff, transTrip, cycle3)
# and some covariate effects:
myeff4 <- includeEffects(XRecip, interaction1 = "anxiety_var" )
#myeff <- includeEffects( myeff, simX, interaction1 = "smoke1" )
myeff4

#Crreate Algorithm
?sienaAlgorithmCreate
myalgorithm <- sienaAlgorithmCreate( projname = 'connections_vs_anxiety' )

ans4 <- siena07( myalgorithm, data = mydata, effects = myeff4)
ans4

summary(ans4)




#####################################################
#Experiment 5


myeff5 <- includeEffects( myeff, transTrip, cycle3)
# and some covariate effects:
myeff5 <- includeEffects(XRecip, interaction1 = "anxiety_var" )
#myeff <- includeEffects( myeff, simX, interaction1 = "smoke1" )
myeff5

#Crreate Algorithm
?sienaAlgorithmCreate
myalgorithm <- sienaAlgorithmCreate( projname = 'connections_vs_anxiety' )

ans5 <- siena07( myalgorithm, data = mydata, effects = myeff5)
ans5

summary(ans5)

mydata