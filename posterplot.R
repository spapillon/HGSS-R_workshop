library(ggplot2)


nbp=100000
df=data.frame(x=1:nbp,y=rnorm(nbp),col=rep(1:10,each=nbp/10))

ggplot(df,aes(x=x,y=abs(y))) + geom_point(aes(colour=factor(col)),alpha=.5) +
    ylab("") + xlab("") + guides(colour=FALSE) + 
    theme(plot.title = element_text(size=20,face="bold"),
          legend.title = element_blank(),
          legend.text = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          ##panel.background = element_rect(fill = "white",colour = "grey70"),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          ##plot.background = element_rect(fill = "white",colour = "grey70"),
          plot.background = element_blank())      


nbgp = 9
nbp=nbgp*11
df=data.frame(x=1:nbp,y=rnorm(nbp,10),col=rep(1:nbgp,each=nbp/nbgp))
ggp.bar = ggplot(df,aes(x=x,y=abs(y))) + geom_bar(aes(fill=factor(col)),alpha=.5,stat="identity", colour="black",size=.2) +
    ylab("") + xlab("") + guides(fill=FALSE) + 
    theme(plot.title = element_text(size=20,face="bold"),
          legend.title = element_blank(),
          legend.text = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          ##panel.background = element_rect(fill = "white",colour = "grey70"),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          ##plot.background = element_rect(fill = "white",colour = "grey70"),
          plot.background = element_blank())      


pdf("posterPlots.pdf",15,2)
for(pal in c("Spectral","YlOrRd","RdYlBu","PiYG")){
    print(ggp.bar +     scale_fill_brewer(palette=pal))
}
print(ggp.bar +     scale_fill_hue())
dev.off()
