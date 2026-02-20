library("TDA")
library("ripserr")

set.seed(123)

# Data: point cloud (matrix)
X <- matrix(rnorm(20), ncol = 2)

# Data: distance matrix from point cloud
D <- as.matrix(dist(X))

# point cloud (matrix) — AlphaComplexDiag
alphaComplexDiag(X, maxdimension = NCOL(X) - 1, library = "GUDHI", location = FALSE, printProgress = FALSE) 


# point cloud (matrix) — RipsDiag
ripsDiag(X, maxdimension = 1, maxscale = 2, dist = "euclidean", library = "GUDHI", location = FALSE, printProgress = FALSE)


# distance matrix — RipsDiag 
ripsDiag(D, maxdimension = 1, maxscale = 2, dist = "arbitrary", library = "GUDHI", location = FALSE, printProgress = FALSE) 


# distance matrix — VietorisRips
vietoris_rips(D, max_dim = 1L, threshold = -1, p = 2L, dim = NULL)



