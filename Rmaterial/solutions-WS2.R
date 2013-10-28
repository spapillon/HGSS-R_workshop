load("session2_meth_data.RData")
ls()
str(beta_value)
str(pheno_data)
head(pheno_data)
str(probe_data)
head(probe_data)

pca = prcomp(t(beta_value))
plot(pca)
plot(pca$x,col=factor(pheno_data$Sample_Group))

covar.test = summary(lm(pca$x~pheno_data$age))
covar.test.pv = unlist(lapply(covar.test,function(l)1-pf(l$fstatistic[1],l$fstatistic[2],l$fstatistic[3])))
covar.test.pv[covar.test.pv<.01]

plot(pca$x[,1],pheno_data$age)
plot(pca$x[,2],pheno_data$age)

covar.test = summary(lm(pca$x~pheno_data$Array))
covar.test.pv = unlist(lapply(covar.test,function(l)1-pf(l$fstatistic[1],l$fstatistic[2],l$fstatistic[3])))
covar.test.pv[covar.test.pv<.01]

covar.test = summary(lm(pca$x~pheno_data$Slide))
covar.test.pv = unlist(lapply(covar.test,function(l)1-pf(l$fstatistic[1],l$fstatistic[2],l$fstatistic[3])))
covar.test.pv[covar.test.pv<.01]

plot(pca$x[,1],pheno_data$Slide,col=factor(pheno_data$Sample_Group))
plot(pca$x[,2],pheno_data$Slide,col=factor(pheno_data$Sample_Group))

diff.meth = apply(beta_value[1:10000,],1,function(r)t.test(r[which(pheno_data$Sample_Group=="control")],r[which(pheno_data$Sample_Group=="ETMR")])$p.value)
diffMean.meth = rep(NA,length(diff.meth))
diffMean.meth[diff.meth<.01] = apply(beta_value[diff.meth<.01,],1,function(r)abs(mean(r[which(pheno_data$Sample_Group=="control")])-mean(r[which(pheno_data$Sample_Group=="ETMR")])))
dm.i = order(diff.meth)[1:9]
sig.probes = rownames(beta_value)[dm.i]
library(reshape)
beta.df = melt(beta_value[dm.i,])
colnames(beta.df) = c("probe","sample","methylation")
beta.df$group=pheno_data[as.character(beta.df$sample),"Sample_Group"]
beta.df$age=pheno_data[as.character(beta.df$sample),"age"]

library(ggplot2)
pdf("../imgs/example-ggplot2.pdf",6,6)
ggplot(beta.df, aes(x=group,y=methylation)) + geom_violin(aes(fill=group),scale="width") + theme(legend.position="bottom") + scale_fill_hue(name="") + facet_wrap(~probe) + xlab("")

ggplot(beta.df, aes(x=methylation)) + geom_density(aes(fill=group,colour=group),alpha=.7) + theme(legend.position="bottom") + scale_fill_hue(name="") + scale_colour_hue(name="") + facet_wrap(~probe)

ggplot(beta.df, aes(x=probe,y=methylation)) + geom_boxplot(aes(fill=group)) + theme(legend.position="bottom") + scale_fill_hue(name="") + coord_flip() + xlab("")

boxplot(list(control=beta_value[dm.i[1],pheno_data$Sample_Group=="control"],ETMR=beta_value[dm.i[1],pheno_data$Sample_Group=="ETMR"]),ylab="methylation")
dev.off()

## Annotation
source("http://bioconductor.org/biocLite.R")
biocLite("TxDb.Hsapiens.UCSC.hg19.knownGene")
library(TxDb.Hsapiens.UCSC.hg19.knownGene)

sig.meth.i = sample(1:nrow(beta_value),1000)
library(GenomicRanges)
meth.gr = GRanges(probe_data[,1],IRanges(start=probe_data[,2],end=probe_data[,3]))
mcols(meth.gr)$probe = rownames(beta_value)[sig.meth.i]

gene.overlap = findOverlap(meth.gr, TxDb.Hsapiens.UCSC.hg19.knownGene)

dist.gene = distanceToNearest(meth.gr, TxDb.Hsapiens.UCSC.hg19.knownGene)
mcols(meth.gr)$distanceToGene = dist.gene$distance
mcols(meth.gr)$nearestGene = ...
