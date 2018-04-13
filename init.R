library("reshape2")

dat <- read.csv("all-vs-all.tsv",sep="\t", header=F)
symdat <- as.data.frame(matrix(nrow=0,ncol=ncol(dat)))

dat.r1.names <- unique(dat$V1)

p1 <- dat[which(dat$V1 == "gi28212141"),]  
p3 <- p1[,2] == "gi729971"
max(p3) #Equivalent to OR applied on all rows
i = "gi28212141"

dist <- dcast(dat[,c(1,2,11)], V1 ~ V2, max)
dist = dist[,2:ncol(dist)]
rownames(dist) = colnames(dist)
dist <- data.matrix(dist)
dist[which(dist==-Inf)]=100

orddist <- order(dist)
smallest_e <- dist[orddist][which(dist[orddist] > 0)][1]

dist[dist== 0] = smallest_e

for(i in 1:ncol(dist)){
  for(j in i:ncol(dist)){
    dist[i,j] <- dist[j,i] <- min(c(-log(dist[i,j]),-log(dist[j,i])))
  }
}

min(dist)
dist <-  dist - min(dist) + 0.01
min(dist)

dist = 1/dist
