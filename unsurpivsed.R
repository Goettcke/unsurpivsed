library(dbscan)
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

tss(gold.dist,gold.kmeans)




source("paircounting.R")


plot(silhouette(clustering.kmeans$cluster, plot.data.sim))

mean(plot.data.sim)
plot(density(plot.data.sim))

#taking the 1st quantile of the distance distribution to get a reasonable epsilon value. 
#original dbscan paper found minpts 4 to be a good value
clustering.dbscan = dbscan(plot.data, quantile(plot.data.sim, probs = c(0.01)), minPts=4)
clustering.dbscan[["proteins"]] = rownames(plot.data)
clustering.dbscan[["tss"]] = tss(dist,data.frame(he=clustering.dbscan$proteins, ha=clustering.dbscan$cluster))
clustering.dbscan[["silhouette"]] = silhouette(clustering.dbscan$cluster, plot.data.sim)
plot(clustering.dbscan$silhouette)
















