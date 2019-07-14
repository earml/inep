#http://www.sthda.com/english/articles/25-cluster-analysis-in-r-practical-guide/111-types-of-clustering-methods-overview-and-quick-start-r-code/

rm(list=ls(all=TRUE))
library(readxl)
educaCluster <- read_excel("D:/Educacao/educaCluster.xlsx")
View(educaCluster)

nome<-as.matrix(educaCluster$X__1,75,1)
rownames(educaCluster) <- paste(nome[1:75], 1:75)
dados<-as.data.frame(educaCluster)
dados <- subset(dados, select = -c(X__1))

library("cluster")
library("factoextra")
library("magrittr")

# Load  and prepare the data
my_data <- dados %>%
  na.omit() %>%          # Remove missing values (NA)
  scale()                # Scale variables

# View the firt 3 rows
head(my_data, n = 3)

res.dist <- get_dist(dados, stand = TRUE, method = "pearson")
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


set.seed(123)
km.res <- kmeans(my_data, 7, nstart = 25)

#Sugere o número de cluster
fviz_nbclust(my_data, kmeans, method = "gap_stat")


# Visualize
library("factoextra")
fviz_cluster(km.res, data = my_data,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

# Compute hierarchical clustering
res.hc <- dados %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidian") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")     # Compute hierachical clustering
# Visualize using factoextra
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 7, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("blue3", "brown2", "dodgerblue2", "gold","gray44","chocolate1","forestgreen"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


g1<-c(24,39,47,55,59,73,53,9,40);grupoG<-dados[g1,]
g2<-c(35,42,64,12,57,44,46);grupoE<-dados[g2,]
g3<-c(20,15,16,61,3,17,66,31,71,27,10,36);grupoF<-dados[g3,]
g4<-c(50,58,8,29,34,23,33,69,32,68,4,6,13,37,43,45);grupoD<-dados[g4,]
g5<-c(70,51,63,30,56,14,21,1,54,2,19);grupoB<-dados[g5,]
g6<-c(18,67,62,26,41);grupoA<-dados[g6,]
g7<-c(11,25,5,38,48,52,72,65,75,60,7,22,49,28,74);grupoC<-dados[g7,]

print(paste(summary(grupoA[1]),summary(grupoB[1]),summary(grupoC[1]),summary(grupoD[1]),
            summary(grupoE[1]),summary(grupoF[1]),summary(grupoG[1])))

grupoA<-cbind(grupoA,rep('A',dim(grupoA)[1]))
grupoB<-cbind(grupoB,rep('B',dim(grupoB)[1]))
grupoC<-cbind(grupoC,rep('C',dim(grupoC)[1]))
grupoD<-cbind(grupoD,rep('D',dim(grupoD)[1]))
grupoE<-cbind(grupoE,rep('E',dim(grupoE)[1]))
grupoF<-cbind(grupoF,rep('F',dim(grupoF)[1]))
grupoG<-cbind(grupoG,rep('G',dim(grupoG)[1]))

library(plyr)
grupoA<-rename(grupoA, c('rep("A", dim(grupoA)[1])'="Grupo"))
grupoB<-rename(grupoB, c('rep("B", dim(grupoB)[1])'="Grupo"))
grupoC<-rename(grupoC, c('rep("C", dim(grupoC)[1])'="Grupo"))
grupoD<-rename(grupoD, c('rep("D", dim(grupoD)[1])'="Grupo"))
grupoE<-rename(grupoE, c('rep("E", dim(grupoE)[1])'="Grupo"))
grupoF<-rename(grupoF, c('rep("F", dim(grupoF)[1])'="Grupo"))
grupoG<-rename(grupoG, c('rep("G", dim(grupoG)[1])'="Grupo"))

#http://www.sthda.com/english/wiki/wiki.php?title=descriptive-statistics-and-graphics
library(gtools)

dado_analise<-smartbind(grupoA,grupoB,grupoC,grupoD,grupoE,grupoF,grupoG)
library(data.table)
dado_analise<-rbindlist(list(grupoA,grupoB,grupoC,grupoD,grupoE,grupoF,grupoG),fill = TRUE)

library("ggpubr")
# Box plot colored by groups: Species
ggboxplot(dado_analise, x = 'Grupo', y = 'TDISF',
          color = 'Grupo',
          palette = c("#00AFBB", "#E7B800", "#FC4E07","forestgreen","darkviolet","darkred","lawngreen"))


ggstripchart(dado_analise, x = "Grupo", y = "TDISF",
             color = "Grupo",
             palette = c("#00AFBB", "#E7B800", "#FC4E07","forestgreen","darkviolet","darkred","lawngreen"),
             add = "mean_sd")



groups <- cutree(fviz_dend)
res.hc$labels
table(dados[,1],groups)


library(clValid) 
library(kohonen)
# Iris data set: 
# - Remove Species column and scale 
df <- scale(iris[, -5]) 
# Compute clValid 
clmethods <- c("hierarchical", "kmeans", "diana", "sota", "pam", "clara","agnes") 
#clmethods <- c("hierarchical","agnes") 

validacao <- c("internal", "stabilit","biological")
metrica <- c("euclidean","correlation","manhattan")
metodos <- c("ward.D2","single",'complete','average')
intern <- clValid(dados, nClust = 2:10, clMethods = clmethods, validation = validacao[1],metric =metrica[1]) 
# Summary 
summary(intern)


