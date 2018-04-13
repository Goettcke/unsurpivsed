options(stringsAsFactors = FALSE)



ppp = function(x){
  p = c()
  for(clust in unique(x[,2])){
    s <- x[which(x[,2] == clust),]
    n <- as.character(s[,1])
    if(nrow(s)>1){
      for(i in 1:(nrow(s)-1)){
        for(j in (i+1):nrow(s)){
          toadd = paste0(n[i],"|",n[j])
          p = c(p,toadd)
        }
      }
    }
  }
  return(p)
}

gold.kmeans <- gold.kmeans[order(gold.kmeans[,1]),]


gold.kmeans.pairs = ppp(gold.kmeans)
gold.pairs = ppp(gold)
test <- gold
test$V2=1
gold.all.pairs = ppp(test)

a = intersect(gold.kmeans.pairs,gold.pairs)
b = setdiff(gold.kmeans.pairs,gold.pairs)
c = setdiff(gold.pairs,gold.kmeans.pairs)
d = setdiff(gold.all.pairs, union(a,union(b,c)))
M = union(a,union(b,union(c,d)))

la=length(a)
lb=length(b)
lc=length(c)
ld=length(d)
lM=length(M)




gold.kmeans.rand = (length(a)+length(d))/(length(a)+length(b)+length(c)+length(d))
gold.kmeans.jaccard = (length(a))/(length(a)+length(b)+length(c))
