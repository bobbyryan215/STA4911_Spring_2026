library(TDA)
library(ripserr)
library(igraph)
library(phutil)

set.seed(139)
#create random group
g <- sample_gnp(25, 2/10)
n <- ecount(g)
plot(g)


#ripserr vietoris_rips 
dist <- as.dist(distances(g))
x1 <- vietoris_rips(dist) |> as_persistence()


#TDA rispDiag same results as above
x2 <- ripsDiag(dist, maxdimension = 1 ,maxscale = 1000,dist = "arbitrary") |> as_persistence()


#same graph as above but now with weights. 
E(g)$weight <- rnorm(n)^2
plot(g)


#ripserr vietors_rips
dist_w <- as.dist(distances(g))
x3 <- vietoris_rips(dist_w)|> as_persistence()
as.matrix(x3)

#TDA rispDiag same results as above
x4 <- ripsDiag(dist_w, maxdimension = 1 ,maxscale = 1000,dist = "arbitrary") |> as_persistence()
as.matrix(x4)
