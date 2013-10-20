## Dummy objects
mat = matrix(round(runif(400,-100,100)),100,4)
rownames(mat) = paste("row",1:nrow(mat),sep="")
colnames(mat) = paste("col",1:ncol(mat),sep="")

save(mat,file="dataToPlayWith.Rdata")

## Data to plot
nb.samp=100
nb.genes = 20000
ge.mean = rpois(nb.genes,1)
ge.mean[333] = 2
mat.ge = matrix(rnorm(nb.samp*nb.genes,ge.mean,sqrt(ge.mean)),nb.genes,nb.samp)
rownames(mat.ge) = paste("gene",1:nb.genes,sep="")
colnames(mat.ge) = paste("sample",1:nb.samp,sep="")
mat.ge[333,] = mat.ge[666,] + rnorm(nb.samp,0,sqrt(ge.mean[333])/5)

