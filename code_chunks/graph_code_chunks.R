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
dist_matrix <- as.dist(distances(g))
x1 <- vietoris_rips(dist_matrix) |> as_persistence()
as.matrix(x1)


#TDA rispDiag same results as above
#FIXME: the maxscale parameter may be improperty defined
x2 <- ripsDiag(dist_matrix, maxdimension = 1 ,maxscale = 1.1*max(dist_matrix),dist = "arbitrary", library = "GUDHI") |> as_persistence()
as.matrix(x2)


#check x1 and x2
#compare birth of zero dimension
sort(as.matrix(x1)[as.matrix(x1)[,1] ==0,2]) - sort(as.matrix(x2)[as.matrix(x2)[,1] ==0,2])
#compare death of zero dimension. Here is a small difference
sort(as.matrix(x1)[as.matrix(x1)[,1] ==0,3]) - sort(as.matrix(x2)[as.matrix(x2)[,1] ==0,3])

#compare birth of 1 dimension
sort(as.matrix(x1)[as.matrix(x1)[,1] ==1,2]) - sort(as.matrix(x2)[as.matrix(x2)[,1] ==1,2])
#compare death of 1 dimension. 
sort(as.matrix(x1)[as.matrix(x1)[,1] ==1,3]) - sort(as.matrix(x2)[as.matrix(x2)[,1] ==1,3])



#same graph as above but now with weights. 
E(g)$weight <- rnorm(n)^2
plot(g)


#ripserr vietors_rips
dist_matrix_w <- as.dist(distances(g))
x3 <- vietoris_rips(dist_matrix_w)|> as_persistence()
as.matrix(x3)

#TDA rispDiag same results as above
#FIXME: the maxscale parameter may be improperty defined
x4 <- ripsDiag(dist_matrix_w, maxdimension = 1 ,maxscale = 1.1*max(dist_matrix),dist = "arbitrary") |> as_persistence()
as.matrix(x4)



#check x1 and x2
#compare birth of zero dimension
sort(as.matrix(x3)[as.matrix(x3)[,1] ==0,2]) - sort(as.matrix(x4)[as.matrix(x4)[,1] ==0,2])
#compare death of zero dimension. Here is a small difference
sort(as.matrix(x3)[as.matrix(x3)[,1] ==0,3]) - sort(as.matrix(x4)[as.matrix(x4)[,1] ==0,3])

#compare birth of 1 dimension
sort(as.matrix(x3)[as.matrix(x3)[,1] ==1,2]) - sort(as.matrix(x4)[as.matrix(x4)[,1] ==1,2])
#compare death of 1 dimension. 
sort(as.matrix(x3)[as.matrix(x3)[,1] ==1,3]) - sort(as.matrix(x4)[as.matrix(x4)[,1] ==1,3])
