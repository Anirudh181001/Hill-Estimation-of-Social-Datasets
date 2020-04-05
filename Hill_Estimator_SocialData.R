install.packages("igraph")
library('igraph')
bar<-sample_pa(40,power=10,m=3,directed=FALSE)
barmat<-as_adjacency_matrix(bar)
gr<-graph_from_adjacency_matrix(barmat,mode="undirected")
barmatfull<-as.matrix(barmat)
barmatsq<-barmatfull%*%barmatfull
barlist<-c()
barbara<-c()
c=1
for(i in barmatsq){
  for(j in lower.tri(barmatsq)){
    if(j==TRUE){
      barlist[c]=i
      c=c+1
    }
  }
}
m<-1
for(i in as.matrix(barmat)){
  for(j in lower.tri(as.matrix(barmat))){
    if(j==1){
      barbara[m]=i
      m=m+1
    }
  }
}
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
       ylab = "Hill estimate of alpha", main="Hill plot") 
}
Hillalpha(barlist)
