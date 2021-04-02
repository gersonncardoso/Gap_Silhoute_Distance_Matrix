#Libraries
library(Hmisc)
library(factoextra)
library(NbClust)
library(readxl)
library(cluster)

#Degine currente directory
setwd("C:/Users/gerso/Google Drive/Acadêmicos/Dados/Pré Crise x Pos Crise -2 Anos/Pre Crise")
Pre_Crise <- read_excel("C:/Users/gerso/Google Drive/Acadêmicos/Dados/Pré Crise x Pos Crise -2 Anos/Pre Crise/Pre_Crise.xlsx")
Pre_Crise <- subset(Pre_Crise, select = -c(...1) )

#Distance Correlation - Gower distance
correlacao=rcorr(as.matrix(Pre_Crise), type="pearson")#Correlation Matrix
dist_cor=sqrt(2*(1-correlacao$r))#Distance Correlation Matrix


#Clustering Silhoutte - Plot
fviz_nbclust(dist_cor, kmeans, method = "silhouette")+# Silhouette method
  labs(subtitle = "Silhouette Method")

#Gap statistic - Plot
set.seed(123)
fviz_nbclust(dist_cor, kmeans, nstart = 25, method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")


# Compute gap statistic for kmeans
# we used B = 500 interations get the values for max of 20 clusters. Enough to finde the first unitary standard deviation
gap_stat <- clusGap(dist_cor, FUN = kmeans, nstart = 25,
                    K.max = 20, B = 500)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)
