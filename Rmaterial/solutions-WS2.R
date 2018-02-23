## Conditions examples
test = 2 + 2 == 4
test
!test
2>3 & 5<10
2>3 | 5<10
5:10 > 6
which(5:10 > 6)
5:10 >= 6 & 5:10<=7
which(5:10 >= 6 & 5:10<=7)

## Condition exercise
rem3 <- function(input.vector){
  return(input.vector[which(input.vector>=3)])
}
remTh <- function(input.vector,threshold){
  return(input.vector[which(input.vector>=threshold)])
}
rem3(1:10)
rem3(c(2,14,3,6,1,4,5,2,4,5,6,2,7))
remTh(1:10,3)
remTh(1:10,5)
remTh(c(2,14,3,6,1,4,5,2,4,5,6,2,7),3)
remTh(c(2,14,3,6,1,4,5,2,4,5,6,2,7),5)

## Other useful functions
logical.vector = c(4,14,23,7,44) %in% 1:10
logical.vector
any(logical.vector)
sum(logical.vector)
mean(logical.vector)
table(logical.vector)

## Might be confusing but it work without the which also
vecExample = sample(1:10)
vecExample
vecExample[which(vecExample>5)]
vecExample[vecExample>5]

## Test and loops - Exercises
classify.mean <- function(input.vec){
  if(mean(input.vec)<3){
    return("low")
  } else if(mean(input.vec)>7){
    return("high")
  } else {
    return("medium")
  }
}
classify.mean(rep(1,10))
classify.mean(1:20)
classify.mean(1:10)
classify.mean(3)

## apply vs loops
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
        i = i + 1
    }
    return(res)
}
meanCols.apply(matrix(1:12,3,4))
meanCols.for(matrix(1:12,3,4))
meanCols.while(matrix(1:12,3,4))





#######
## Methylation analysis
#######

#### Loading and discovering the data
load("session2_reduced_meth_data.RData")
ls()
dim(beta_value)
str(beta_value)
dim(pheno_data)
head(pheno_data)
dim(probe_data)
head(probe_data)

#### Density(distribution) of the methylation across all probes of a sample
## Compute and plot the density of one sample(column)
plot(density(beta_value[,13]))
hist(beta_value[,13])  ## Density is just a 'smooth' histogram
## Compute and plot the density of each sample in a pdf document
pdf("density_plots.pdf")
lapply(1:ncol(beta_value), function(i) {
  plot(density(beta_value[,i]),main=i)
})
dev.off()

#### Principal Component analysis.
pca = prcomp(t(beta_value))  ## We want to summarize the probes(rows) but the prcomp summarizes columns by default: 't()' function transpose(rows<->columns) a matrix.
dim(pca$x) ## The new coordinates of each samples is found in pca$x. Each column represent a principal component
color_vector = rep("red",ncol(beta_value))
color_vector[which(pheno_data$Sample_Group=="control")] = "blue"
plot(pca$x,col=color_vector)  ## Here 'plot' uses only the first two columns of 'pca$x', i.e. the first two PCs to represent the samples(rows)

## Same plot but with sample names instead of points
plot(pca$x,type="n") ## Empty plot initialization
text(pca$x,labels=colnames(beta_value),col=color_vector) ## the text labels are overlayed using the correct coordinates.

## Same plot but with column index instead of sample names
plot(pca$x,type="n") ## Empty plot initialization
text(pca$x,labels=1:ncol(beta_value),col=color_vector) ## the text labels are overlayed using the correct coordinates.

#### Cluster
## Using the difference in methylation to assess sample similarity
diff.dist = dist(t(beta_value))
clust.beta = hclust(diff.dist,method="ave")  ## Different method(or linkage criteria) are available.
plot(clust.beta)  ## Tree
plot(clust.beta,labels=1:ncol(beta_value)) ## Tree with column index

#### Heatmap
heatmap(beta_value[1:1000,],ColSideColors=color_vector)
## Now using the probes that varies the most across samples
# Get the top 1,000 most variant probes
probe_var <- apply(beta_value,1,var)
probe_var <- order(probe_var, decreasing=T)
probe_var <- probe_var[1:1000]
# Plot the heatmap using those sites
heatmap(beta_value[probe_var, ],ColSideColors=color_vector)


#### We decide to remove sample 1, 23, 25
beta_value = beta_value[,c(2:22,24,26,27)]
pheno_data = pheno_data[c(2:22,24,26,27),]  ## !! In pheno_data samples are rows !!
color_vector = rep("red",ncol(beta_value))
color_vector[which(pheno_data$Sample_Group=="control")] = "blue"

#### Re PCA
pca.noOutliers = prcomp(t(beta_value))
plot(pca.noOutliers$x,col=color_vector)

## 1st PC and age
plot(pca.noOutliers$x[,1], pheno_data$age,col=color_vector,xlab="PC1",ylab="age")
## 2nd PC and age more correlated in controls
plot(pca.noOutliers$x[,2], pheno_data$age,col=color_vector,xlab="PC1",ylab="age")

## PCA on the X chromosome and controls only
controls.index = which(pheno_data$Sample_Group=="control")
x_probes <- probe_data$chrom == "chrX"
summary(x_probes)
pca.x.controls <- prcomp(t(beta_value[x_probes, controls.index]))
mean_x_meth <- apply(beta_value[x_probes, controls.index], 2, mean)
color_x_meth<- ifelse(mean_x_meth >= 0.45, "red", "blue")
plot(pca.x.controls$x, col=color_x_meth)

### More advanced: automatic detection of metadata captured by PCs through linear regression
age.test = summary(lm(pca.noOutliers$x~pheno_data$age)) ## Linear regression between each PC(pca$x column) and age
age.test.pv = unlist(lapply(age.test,function(l)1-pf(l$fstatistic[1],l$fstatistic[2],l$fstatistic[3]))) ## Retrieval of the pvalue
age.test.pv[age.test.pv<.01] ## Which PCs are associated with age at pv 0.01 ? The most associated is PC2.

array.test = summary(lm(pca.noOutliers$x~pheno_data$Array))
array.test.pv = unlist(lapply(array.test,function(l)1-pf(l$fstatistic[1],l$fstatistic[2],l$fstatistic[3])))
array.test.pv[array.test.pv<.01] ## No association

slide.test = summary(lm(pca.noOutliers$x~pheno_data$Slide))
slide.test.pv = unlist(lapply(slide.test,function(l)1-pf(l$fstatistic[1],l$fstatistic[2],l$fstatistic[3])))
slide.test.pv[slide.test.pv<.01]  ## Also PC2
plot(pca.noOutliers$x[,2],pheno_data$Slide,col=color_vector,xlab="PC2",ylab="Slide")


#### Differential methylation expression (on the first 10000 probes)

## t.test should be used on normal data: are the methylation values normal ?
## Normality test on controls: extra, for fun !
norm.test.pv = apply(beta_value[1:10000,controls.index],1,function(r)shapiro.test(r)$p.value)
hist(norm.test.pv)  ## Many values close to 0...let's use 't.test' anyway (but 'wilcox.test' is non-parametrical and would be safer)

## T test on each probes(rows)
diff.meth.pv = apply(beta_value[1:10000,],1,function(r)t.test(r[controls.index],r[-controls.index])$p.value)

## Pvalue distribution
hist(diff.meth.pv)
summary(diff.meth.pv)

## Top 9
dm.i = order(diff.meth.pv)[1:9]
sig.probes.names = rownames(beta_value)[dm.i]

## Simple boxplot
boxplot(list(controls=beta_value[dm.i[1],controls.index],cases=beta_value[dm.i[1],-controls.index]),main=sig.probes.names[1],ylab="methylation")


#### Advanced plotting with ggplot2
library(ggplot2)

## Step 1: Creation of the data.frame
library(reshape)
beta.df = melt(beta_value[dm.i,])
colnames(beta.df) = c("probe","sample","methylation")
beta.df$group=pheno_data[as.character(beta.df$sample),"Sample_Group"]
beta.df$age=pheno_data[as.character(beta.df$sample),"age"]
head(beta.df)

## Simple boxplot
ggplot(subset(beta.df,probe==sig.probes.names[1]),aes(x=group,y=methylation)) + geom_boxplot(aes(fill=group)) + ggtitle(sig.probes.names[1])

## Simple boxplot + faceting(9 panels)
ggplot(beta.df,aes(x=group,y=methylation)) + geom_boxplot(aes(fill=group)) + facet_wrap(~probe)

## Violin plots
ggplot(beta.df, aes(x=group,y=methylation)) + geom_violin(aes(fill=group),scale="width") + theme(legend.position="bottom") + scale_fill_hue(name="") + facet_wrap(~probe) + xlab("")

## Density plots
ggplot(beta.df, aes(x=methylation)) + geom_density(aes(fill=group,colour=group),alpha=.7) + theme(legend.position="bottom") + scale_fill_hue(name="") + scale_colour_hue(name="") + facet_wrap(~probe)

## Other merged boxplot version
ggplot(beta.df, aes(x=probe,y=methylation)) + geom_boxplot(aes(fill=group)) + theme(legend.position="bottom") + scale_fill_hue(name="") + coord_flip() + xlab("")

## Other plots with ggplot2
## Density for one sample, e.g. 13th column
dens.df = data.frame(methylation=beta_value[,13])
ggplot(dens.df, aes(x=methylation)) + geom_density()
ggplot(dens.df, aes(x=methylation)) + geom_histogram()


## Annotation
library(GenomicRanges)
source("http://bioconductor.org/biocLite.R")
biocLite("TxDb.Hsapiens.UCSC.hg19.knownGene")
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
transcripts.gr = transcripts(TxDb.Hsapiens.UCSC.hg19.knownGene)

meth.gr = GRanges(probe_data[1:10000,1],IRanges(start=probe_data[1:10000,2],end=probe_data[1:10000,2]))
mcols(meth.gr)$probe = rownames(beta_value)[1:10000]
mcols(meth.gr)$pv = diff.meth.pv
meth.gr

gene.overlap = findOverlaps(meth.gr, transcripts.gr)
gene.overlap
length(unique(queryHits(gene.overlap))) ## How many probes overlap a known transcript

dist.gene = distanceToNearest(meth.gr, transcripts.gr)
mcols(meth.gr)$distanceToGene = as.data.frame(dist.gene)$distance
meth.gr

## Plotting the distance to gene
meth.gr.df = as.data.frame(mcols(meth.gr))
ggplot(meth.gr.df,aes(x=distanceToGene)) + geom_histogram()
ggplot(meth.gr.df,aes(x=distanceToGene)) + geom_histogram() + xlim(0,1000)
ggplot(meth.gr.df,aes(x=distanceToGene)) + geom_histogram() + xlim(1,1000)

ggplot(meth.gr.df,aes(x=distanceToGene)) + stat_ecdf(aes(colour=pv<.01)) + xlim(1,1000) + ylab("proportion of probes")
ggplot(meth.gr.df,aes(x=distanceToGene,y=-log10(pv))) + geom_bin2d() + xlim(1,1000)
