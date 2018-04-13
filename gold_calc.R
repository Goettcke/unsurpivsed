library(cluster)
gold <- read.csv(file="partial_renamed.txt",sep="\t",header=F, stringsAsFactors = F)
gold <- gold[order(gold[,1]),]
gold.dist = dist[which(rownames(dist) %in% gold[,1]),which(colnames(dist) %in% gold[,1])]
gold[,1] == rownames(gold.dist)
gold$V2 = as.numeric(as.factor(gold$V2))
gold.sil = silhouette(gold[,2],gold.dist)



#plot(gold.sil)


