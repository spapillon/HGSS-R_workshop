## Dummy objects
luckyNumbers = c(4,8,15,16,23,42)
names(luckyNumbers) = c("frank","henry","philip","steve","tom","francis")
oneToTen = 1:10
tenOnes = rep(1,10)
samples = c("sampA","sampB")
neo = matrix(1:12,3,4)
colnames(neo) = c("gene1","gene2","gene3","gene4")
rownames(neo) = c("sample1","sample2","sample3")
save(luckyNumbers,oneToTen,tenOnes,samples,neo,file="sorryImLate.RData")

## Data to plot
nb.samp=100
nb.genes = 20000
ge.mean = rpois(nb.genes,1)
ge.mean[333] = 2
mat.ge = matrix(rnorm(nb.samp*nb.genes,ge.mean,sqrt(ge.mean)),nb.genes,nb.samp)
rownames(mat.ge) = paste("gene",1:nb.genes,sep="")
colnames(mat.ge) = paste("sample",1:nb.samp,sep="")
mat.ge[333,] = mat.ge[666,] + rnorm(nb.samp,0,sqrt(ge.mean[333])/5)
mat.ge[667,] = rnorm(nb.samp,ge.mean[333],sqrt(ge.mean[333]))
save(mat.ge,file="dataForBasicPlots.RData")
