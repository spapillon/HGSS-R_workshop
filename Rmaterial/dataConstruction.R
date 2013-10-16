## Dummy objects
mat = matrix(round(runif(400,-100,100)),100,4)
rownames(mat) = paste("row",1:nrow(mat),sep="")
colnames(mat) = paste("col",1:ncol(mat),sep="")

save(mat,file="dataToPlayWith.Rdata")