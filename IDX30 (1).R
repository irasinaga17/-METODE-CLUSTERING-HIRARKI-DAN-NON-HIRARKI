#####Library####
# wrangling and EDA
library(tidyverse)
library(tidyquant)
library(lubridate)
library(gfer)
library(dplyr)

# Visualization
library(ggthemes)
library(scales)

#clustering
library(factoextra)
library(FactoMineR)
library(dbscan) # DBSCAN
library(cluster) #K-Medoid

####Data 1#####
setwd("D:\\UNY\\SEMESTER 6\\STATISTIKA KEUANGAN\\Tugas")
stocks <- read.csv("IDX High Dividen 20.csv")
stocks$Date <- as.Date(stocks$Date, format="%m/%d/%Y")
head(stocks)

profile <- read.csv("profile.csv")
profile$ListingDate <- as.Date(profile$ListingDate, format="%m/%d/%Y")
head(profile)

unique(stocks$symbol)
yahoo_symbols <- stocks %>% 
  pull(symbol) %>% 
  unique()
profile %>% 
  filter(!Code %in% yahoo_symbols) %>% 
  pull(Code)

stocks_agg <- stocks %>% 
  na.omit() %>% 
  mutate(change = (Close - lag(Close))/lag(Close)) %>% 
  group_by(symbol, months(Date)) %>% 
  summarise(sdclose = StdDev(change), # volatility
            medvol = round(median(Volume)) # liquidity
  ) %>% 
  ungroup() %>% 
  left_join(select(profile, Code, Share) , by = c("symbol"="Code")) %>% 
  mutate(medvol = medvol/Share*100) %>% 
  select(-Share) %>% 
  rename(month = 2)
head(stocks_agg)

data_final <-  stocks_agg %>% 
  rename(year = 2) %>% 
  pivot_wider(names_from = year, values_from = c(3:4)) %>% 
  replace(is.na(.),0) %>% 
  column_to_rownames(var = "symbol")
data_final

####Data 2####
stocks_agg <- stocks %>% 
  na.omit() %>% 
  mutate(change = (Close - lag(Close))/lag(Close)) %>% 
  group_by(symbol, months(Date)) %>% 
  summarise(meanR = mean(change), # volatility
            meanvol = round(mean(Volume)),
            meanclose= mean(Close)# liquidity
  ) %>% 
  ungroup() %>% 
  left_join(select(profile, Code, Share) , by = c("symbol"="Code")) %>% 
  mutate(meanvol = meanvol/Share*100) %>% 
  select(-Share) %>% 
  rename(month = 2)
head(stocks_agg)

data_final <-  stocks_agg %>% 
  rename(year = 2) %>% 
  pivot_wider(names_from = year, values_from = c(3:5)) %>% 
  replace(is.na(.),0) %>% 
  column_to_rownames(var = "symbol")
data_final


####outlier Detection####
#kNNdistplot(scale(data_final), k = 8)
#abline(h = 2.5, col = "red")
# DBSCAN clustering
dbscan_clust <- dbscan(scale(data_final), eps = 2.5, minPts = 8)
dbscan_clust
# cluster yang outlier
data_anomaly <- data_final %>% 
  rownames_to_column(var = "symbol") %>% 
  mutate(db_clust = as.factor(dbscan_clust$cluster)) 

data_anomaly %>% 
  filter(db_clust==0) %>% 
  pull(symbol)
data_anomaly %>% 
  column_to_rownames("symbol") %>% 
  PCA(graph = F, quali.sup = 8) %>% 
  plot.PCA(choix = "ind", 
           select = "contrib10",
           habillage = 8,
           col.hab = c("red", "black"))
data_final_scale <- data_anomaly %>% 
  filter(db_clust != 0) %>% 
  column_to_rownames(var = "symbol") %>% 
  select(-db_clust) %>% 
  scale()
tail(data_final_scale)

####Clustering K-Means####
kmeansTunning <- function(data, maxK) {
  withinall <- NULL
  total_k <- NULL
  for (i in 2:maxK) {
    set.seed(122)
    temp <- kmeans(data,i)$tot.withinss
    withinall <- append(withinall, temp)
    total_k <- append(total_k,i)
  }
  plot(x = total_k, y = withinall, type = "o", xlab = "Number of Cluster", ylab = "Total wss")
  abline(h = 1080, col  = "firebrick3", lty = 2)
}
kmeansTunning(data_final, maxK = 10)
set.seed(2022)
clust <- kmeans(data_final,4)
clust
fviz_cluster(clust, data_final, ggtheme = theme_minimal())
kmeans_total <- clust$cluster %>% 
  table() %>% 
  as.numeric()

data.frame(cluster = c(1:4), 
           member = kmeans_total, 
           wss = clust$withinss) %>% 
  arrange(wss)

clust$betweenss / clust$totss *100

####Clustering K-Medoid####
# Elbow method
fviz_nbclust(data_final, pam, method = "wss") +
  geom_vline(xintercept = 4, linetype = 1)+
  geom_hline(yintercept = 1100)+
  labs(subtitle = "Elbow method")
# K-medoid clustering
kmedoid <- pam(x = data_final, k = 3)
kmedoid

# Cluster info
kmedoid$clusinfo %>% 
  as.data.frame() %>% 
  mutate(cluster = 1:3,
         avg_sil = kmedoid$clus.avg.widths, 
         medoid = rownames(kmedoid$medoids)) %>% 
  select(cluster, everything())
fviz_cluster(kmedoid, data_final_scale, ggtheme = theme_minimal())



####Compare Kmeans & KMedoid####
ggpubr::ggarrange(
  fviz_cluster(clust, data_final, main = "K-Means Clustering", ggtheme = theme_minimal()),
  fviz_cluster(kmedoid, data_final, main = "K-Medoid Clustering",  ggtheme = theme_minimal()), 
  ncol = 1  
)

#https://rpubs.com/David21/stocksclustering

