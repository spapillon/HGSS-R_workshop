## Vector construction
luckyNumbers = c(4,8,15,16,23,42)
luckyNumbers
oneToTen = 1:10
tenOnes = rep(1,10)
samples = c("sampA","sampB","sampC")
samples

## Vector manipulation
## Using indexes
luckyNumbers[3]
luckyNumbers[3] = 79
luckyNumbers[2:4]
luckyNumbers[2:4] = c(14,3,9)
luckyNumbers[c(1,6)]
luckyNumbers[-2]
## Using names
names(luckyNumbers)
names(luckyNumbers) = c("frank","henry","philip","steve","tom","francis")
names(luckyNumbers) = NULL
names(luckyNumbers) = c("frank","henry","philip","steve","tom","francis")
luckyNumbers["philip"]
luckyNumbers["philip"] = 79
luckyNumbers[c("henry","philip","steve")]
luckyNumbers[c("henry","philip","steve")] = c(14,3,9)
luckyNumbers[c("frank","francis")]
names(luckyNumbers)[5]
names(luckyNumbers)[5] = "tomtom"
names(luckyNumbers)[1:3]
names(luckyNumbers)[1:3] = c("newName1","newName2","newName3")
luckyNumbers = c(luckyNumbers, 65)
names(luckyNumbers)[7] = "simon"
## Size of the vector
length(luckyNumbers)
luckyNumbers[length(luckyNumbers)]

## More manipulation
sort(luckyNumbers)
luckyNumbers[order(luckyNumbers)]
sort(luckyNumbers,decreasing=TRUE)
c(luckyNumbers,1:10,tenOnes)
sort(c(luckyNumbers,1:10,tenOnes))
rev(1:10)
sample(1:10)
sample(1:10,2)

## Exploration
head(luckyNumbers)
tail(luckyNumbers)
summary(luckyNumbers)
min(luckyNumbers)
max(luckyNumbers)
mean(luckyNumbers)
mean(luckyNumbers[1:3])
mean(luckyNumbers[c(1,6)])

## Operations
luckyNumbers * 4
luckyNumbers - luckyNumbers
luckyNumbers * 1:length(luckyNumbers)

## Get my favorite number
n = c(3,12,458,-449826.67)
((n*6+21)/3-1)/2 -n

## Matrix
neo = matrix(1:12,3,4)
neo
neo[2,4]
neo[2,4] = 0
neo[,1]
neo[,1] = rep(21,3)
neo[3,]
sample(neo[3,])
neo[3,] = sample(neo[3,])
neo[1:2,1:3]
neo[1:2,1:3] = matrix(rev(1:6),2,3)
neo[c(1,3),c(2,4)]

## Manipulating more
onesMat = matrix(rep(1,6),2,3)
dim(onesMat)
onesMat
2 * onesMat
rbind(onesMat, 2*onesMat)
cbind(onesMat, 2*onesMat)
cbind(neo, rep(100,3))
neo = rbind(neo, rep(100,4))
cbind(neo[,1:2],rep(1,4),neo[,3:4])
neo = cbind(neo[,1:2],rep(1,4),neo[,3:4])
neo = neo[1:3,c(1,2,4,5)]
## Names
colnames(neo) = c("gene1","gene2","gene3","gene4")
rownames(neo) = c("sample1","sample2","sample3")
neo
neo["sample2","gene3"]
neo["sample2","gene3"] = 33
neo[c("sample1","sample3"),c("gene2","gene4")]
neo[c("sample1","sample3"),c("gene2","gene4")] = matrix(82:85,2,2)
neo = rbind(neo,c(3,54,2,12))
rownames(neo)[4] = "sample4"

## Some functions
head(neo)
mean(neo)
sum(neo) / length(neo)
neo
neo - 1
neo - neo
neo + neo

## Data structure exercice.
matExample = matrix(runif(100*4),100,4)
colnames(matExample) = c("sampleA","sampleB","sampleC","sampleD")
head(matExample)
mean(matExample[,1])
mean(matExample[,2])
mean(matExample[,3])
mean(matExample[,4])
colnames(matExample)[3] ## If the highest mean was found for matExample[,3]
max(matExample[,1])
max(matExample[,2])
max(matExample[,3])
max(matExample[,4])
colnames(matExample)[1] ## If the highest max was found for matExample[,1]

## Apply
apply(matExample,2,mean)
apply(matExample,1,min)
summary(apply(matExample,1,min))

## Functions
almostMean = function(x){
x.mean = mean(x)
return(x.mean+1)
}
almostMean(0:10)
x.mean  ## This variable doesn't exist outside of the function -> Error
## Exercice
minMaxAve <- function(input.vector){
    return(mean(c(min(input.vector),max(input.vector))))
}
minMaxAve(0:10)
minMaxAve(c(1,20:30))
power <- function(base,exponent){
    return(exp(exponent*log(base)))
}
power(2,2)
power(2,10)
2^10

## Import/Export data
## In R format
save(neo, matExample, file="someStuff.RData")
save.image(file="allTheStuff.RData")
load("someStuff.RData")
print(load("someStuff.RData"))  ## Load and print what was loaded !
load("allTheStuff.RData")
ls()
## In text format
write.table(matrixToWrite,file="textFile.txt", col.names=TRUE,row.names=FALSE, quote=FALSE, sep="\t")
input.data = read.table(file="textFile.txt", as.is=TRUE, header=TRUE, sep="\t")

## Plots
print(load(file="dataForBasicPlots.RData")) ## Load the data...
hist(mat.ge[,1])
plot(mat.ge[,1],mat.ge[,2])

par(mfrow=c(1,2)) ## To print different plots in the same windows
hist(mat.ge[,1])
plot(mat.ge[,1],mat.ge[,2])
dev.off() ## close the window

hist(mat.ge[,1])
x11() ## To open a new window for a second plot
plot(mat.ge[,1],mat.ge[,2])

## Plot exercise
hist(apply(mat.ge,1,mean),xlab="gene expression",ylab="number of genes",main="Average gene expression across the samples")
abline(v=mean(ge.mean),lty=2)

plot(mat.ge["gene333",],mat.ge["gene666",],xlab="gene333",ylab="gene666",main="Correlated genes")
lines(mat.ge["gene333",],mat.ge["gene667",],type="p",col=2,pch=2)

boxplot(mat.ge[,1:10],xlab="sample",ylab="expression",main="Box plot example")
