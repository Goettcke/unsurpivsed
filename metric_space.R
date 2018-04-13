
X = dist
# convert the distances matrix into B as described in the slides (p. 57)
# A = (a_ij) = -1/2* d_ij ^2
# ~ 1 line of code
A <- -0.5* X*X

# Calculate the average of the row in A, the average of the columns in A and the average of the matrix
# You can use the function mean in combination with apply () - see ?apply for rowwise and column wise application 
# Or use rowMeans or colMeans
# ~ 3 lines of code
# row.mean = apply(A, 1, mean)
row.mean = rowMeans(A)
# col.mean = apply(A, 2, mean)
col.mean = colMeans(A)
A.mean = mean(A)

# define B = (b_ij) as b_ij = a_ij - a_i[row] - a_j[column] + mean(A)
# where a_i[row] is the average of row i, a_j[column] is the average of column j and mean(A) the average of A
# First copy dimensions of A to B, then overwrite all values
B <- data.matrix(A)
# your for loop here
for(row in 1:nrow(B))
{
  for(col in 1:ncol(B))
  {
    B[row, col] = A[row, col] - row.mean[row] - col.mean[col] + A.mean
  }
}

# (b) Calculate the Eigenvalues and Eigenvectors of B

# as before 
# ~ 2 lines of code
eigenValues <- eigen(B)$values
eigenVectors <- eigen(B)$vectors


# (c) Use the first two Eigenvectors of B to calculate the tranformed coordinates of the cities
# As before use cbind to assign and multiply with B
# ~ 2 lines of code
E2 = cbind(eigenVectors[,1],eigenVectors[,2])
TD = t(E2) %*% B

# plot the original data
plot.data = t(TD)

plot.data.sim = matrix(ncol = 866, nrow=866)
for(i in 1:866){
  for(j in 1:866){
    v1 = plot.data[i,]
    v2 = plot.data[j,]
    plot.data.sim[i,j] = sqrt((v1[1]-v2[1])^2+(v1[2]-v2[2])^2)
    
  }
}
rownames(plot.data.sim) <- colnames(plot.data.sim) <- rownames(plot.data)
