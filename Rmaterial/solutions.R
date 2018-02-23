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

## Some functions
head(neo)
mean(neo)
sum(neo) / length(neo)
neo
neo - 1
neo - neo
neo + neo

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

## Functions
power <- function(a,b){
    return(exp(b*log(a)))
}
power(2,2)
power(2,6)

minMaxAve <- function(v){
    return(mean(c(min(v),max(v))))
}
minMaxAve(0:10)
minMaxAve(c(1,20:30))

## Function with condition
rem3 <- function(v){
    return(v[which(v>3)])
}
rem3(1:10)
rem3(4:10)
rem3(1:3)

remTh <- function(v,th=3){
    return(v[which(v>th)])
}
remTh(1:10,3)
remTh(1:10,9)

## Function with condition and if/else
classify <- function(v,low.th=3,high.th=7){
    v.med = median(v)
    if(v.med < low.th){
        return("small")
    } else if(v.med > high.th){
        return("high")
    } else {
        return("medium")
    }
}
classify(1:3)
classify(1:10)
classify(7:10)
classify(1:20)

## Function: apply vs loop
meanCols.apply <- function(mat){
    return(apply(mat,2,mean))
}
meanCols.for <- function(mat){
    res = rep(NA,ncol(mat))
    for(i in 1:ncol(mat)){
        res[i] = mean(mat[,i])
    }
    return(res)
}
meanCols.while <- function(mat){
    res = rep(NA,ncol(mat))
    i = 1
    while(i <= ncol(mat)){
        res[i] = mean(mat[,i])
    }
    return(res)
}
meanCols.apply(matrix(1:12,3,4))
meanCols.for(matrix(1:12,3,4))
meanCols.while(matrix(1:12,3,4))

## Plots
hist(apply(mat.ge,1,mean),xlab="gene expression",ylab="number of genes",main="Average gene expression across the samples")
abline(v=mean(ge.mean),lty=2)

plot(mat.ge["gene333",],mat.ge["gene666",],xlab="gene333",ylab="gene666",main="Correlated genes")
lines(mat.ge["gene333",],mat.ge["gene667",],type="p",col=2,pch=2)

boxplot(mat.ge[,1:10],xlab="sample",ylab="expression",main="Box plot example")

## One-liner quizz
mean(apply(mat,2,mean)>0)
colnames(mat)[which.max(apply(mat,2,max))]
mat[which(apply(mat,1,function(r)!any(r<0))),]
