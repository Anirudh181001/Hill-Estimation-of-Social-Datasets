facebook<-read.table(file = 'file:///C:/Users/shriv/Downloads/FACEBOOK.txt', sep = '\t', header = TRUE)
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
       ylab = "Hill estimate of alpha", main="Hill plot",ylim=c(0,10)) 
}
f_2<-facebook[,2]
Hillalpha(f_2)
install.packages("igraph")
library('igraph')
l1=list()
p=1
c=2
j=1
while(j<88235){
  l1[p]=facebook[j,1]
  l1[c]=facebook[j,2]
  p=p+2
  c=c+2
  j=j+1
}
l555=as.integer(l1)
l51=c(1,2,2,3,3,1,4,2)
l55=as.integer(l51)
g<- graph(edges=l555,directed=F)
plot(g)

