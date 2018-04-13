library(cluster)
gold <- read.csv(file="partial_renamed.txt",sep="\t",header=F, stringsAsFactors = F)
gold <- gold[order(gold[,1]),]
gold.dist = dist[which(rownames(dist) %in% gold[,1]),which(colnames(dist) %in% gold[,1])]
gold[,1] == rownames(gold.dist)
gold$V2 = as.numeric(as.factor(gold$V2))
gold.sil = silhouette(gold[,2],gold.dist)




tss <- function(dist,clustering) {
  output = {}
  total_sums = c()
  clusters = unique(clustering[,2][order(clustering[,2])])
  for (clust in clusters) {
    cluster <- clustering[which(clustering[,2]==clust),]
    all_sums = c()
    length(cluster)
    for (i in 1:nrow(cluster)) {
      
      test = c()
      for (j in 1:nrow(cluster)) {
        test = c(test,dist[i,j])
      }
      all_sums = c(all_sums,sum(test))
    }
    #output[[paste0("cluster-",cluster)]] = {}
    #output[[paste0("cluster-",cluster)]][["all_sums"]] = all_sums
    total_sums = c(total_sums, sum(all_sums))
  }
  
  output[["objects"]] = gold[,1]
  output[["clusters"]] = clusters
  output[["cluster_tss"]] = total_sums
  output[["exp_ss"]] = sum(total_sums)/length(gold[,1])
  output$total_tss = sum(total_sums)
  
  return(output) 
  
  
  
}

tss_test <- tss(gold.dist,gold)

#plot(gold.sil)


