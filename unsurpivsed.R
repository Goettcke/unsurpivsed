setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("init.R")
source("metric_space.R")
source("gold_calc.R")





c_in_gold = 0
while(c_in_gold != 29){
  clustering.kmeans <- kmeans(plot.data, 97, iter.max = 50)
  gold.kmeans <- clustering.kmeans$cluster[which(names(clustering.kmeans$cluster) %in% gold[,1])]
  c_in_gold = length(unique(gold.kmeans))
}
gold.kmeans <- data.frame(protein = names(gold.kmeans), cluster = gold.kmeans)





source("paircounting.R")
plot(silhouette(clustering.kmeans$cluster, dist))



library(cluster)
plot(gold.sil)

unique(clustering.kmeans$cluster)

