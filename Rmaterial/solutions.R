## Get my favorite number
n = c(3,12,458,-449826.67)
((n*6+21)/3-1)/2 -n

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
meanCols.loop <- function(mat){
    res = rep(NA,ncol(mat))
    for(i in 1:ncol(mat)){
        res[i] = mean(mat[,i])
    }
    return(res)
}
meanCols.apply(matrix(1:12,3,4))
meanCols.loop(matrix(1:12,3,4))

## Plots
hist(apply(mat.ge,1,mean),xlab="gene expression",ylab="number of genes",main="Average gene expression across the samples")
abline(v=mean(ge.mean),lty=2)

plot(mat.ge["gene333",],mat.ge["gene666",],xlab="gene333",ylab="gene666",main="Correlated genes")
lines(mat.ge["gene333",],mat.ge["gene667",],type="p",col=2,pch=2)

cor.ge = cor(t(mat.ge))

## One-liner quizz
mean(apply(mat,2,mean)>0)
colnames(mat)[which.max(apply(mat,2,max))]
mat[which(apply(mat,1,function(r)!any(r<0))),]
