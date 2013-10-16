## Get my favorite number
n = c(3,12,458,-449826.67)
n.new = n*6 + 21
n.new = n.new/3 - 1
n.new = n.new/2 - n
n.new

## Data structure exercice.
mat = matrix(runif(100*4),100,4)
colnames(mat) = c("sampleA","sampleB","sampleC","sampleD")
head(mat)
mean(mat[,1])
mean(mat[,2])
mean(mat[,3])
mean(mat[,4])
colnames(mat)[3] ## If the highest mean was found for mat[,3]
max(mat[,1])
max(mat[,2])
max(mat[,3])
max(mat[,4])
colnames(mat)[1] ## If the highest max was found for mat[,1]

## One-liner quizz
mean(apply(mat,2,mean)>0)
colnames(mat)[which.max(apply(mat,2,max))]
mat[which(apply(mat,1,function(r)!any(r<0))),]
