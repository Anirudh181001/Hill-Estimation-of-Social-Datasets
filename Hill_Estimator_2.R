social<-read.table(file = 'file:///C:/Users/shriv/Downloads/Social.txt', sep = '\t', header = TRUE)
social
Hillalpha<-function(x)
{
  ordered <- rev(sort(x))
  ordered <- ordered[ordered[] > 0.]
  n <- length(x)
  loggs <- log(ordered)
  hill <- cumsum(loggs[1.:(n - 1.)])/(1.:(n - 1.)) - loggs[2.:n]
  hill <- 1./hill
  plot(1.:length(hill), hill, 
       type = "l", xlab = "number of order statistics", 
       ylab = "Hill estimate of alpha", main="Hill plot", ylim=c(0,10)) 
}
s_2<-social[,2]
Hillalpha(s_2)
install.packages("igraph")
library('igraph')
lapad=list()
p1=1
c1=2
j1=1
while(j1<28981){
  lapad[p1]=social[j1,1]
  lapad[c1]=social[j1,2]
  p1=p1+2
  c1=c1+2
  j1=j1+1
}
l556=as.integer(lapad)
papadapoopi<- graph(edges=l556,directed=F)
plot(papadapoopi)
