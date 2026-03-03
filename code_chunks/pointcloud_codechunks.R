library(TDA)
library(ripserr)

set.seed(123)

### Data: point cloud (coordinate matrix)
X <- matrix(rnorm(30), ncol = 3)


## AlphaShapeDiag
# NOTE: for n by d matrix, currently d should be 3
# TODO: check if it is valid for d >3 (it runs but I'm not sure if it is correct), for d<3 it breaks
# TODO: should i add one with printprogress = TRUE? not very important

# Default, returns dimension, birth, and death values
alphaShapeDiag(
  X,                            # user must provide
  maxdimension  = NCOL(X) - 1,  # default
  library       = "GUDHI",      # default, must always include GUDHI, string or vector length 2
  location      = FALSE,        # default
  printProgress = FALSE         # default
)

alphaShapeDiag(
  X, 
  maxdimension  = NCOL(X) - 1, 
  library       = c("GUDHI", "PHAT"), 
  location      = TRUE, # also returns birth and death point locations  
  printProgress = FALSE    
)

alphaShapeDiag(
  X, 
  maxdimension  = NCOL(X) - 1, 
  library       = c("GUDHI", "Dionysus"), # must include GUDHI first
  location      = TRUE,                   # also returns birth and death point locations AND representative cycles for each homological feature
  printProgress = FALSE   
)



## AlphaComplexDiag (persistence diagram should be the same as AlphaShapeDiag if dimension = 3)

# Default
# TODO: check valid dimensions, no restrictions explicitly stated but it breaks for larger dimensions
alphaComplexDiag(
  X,                          # user must provide
  maxdimension = NCOL(X) - 1, # default 
  library = "GUDHI",          # default, must always include GUDHI, string or vector length 2
  location = FALSE,           # default   
  printProgress = FALSE       # default
)     

alphaComplexDiag(
  X, 
  maxdimension = NCOL(X) - 1, 
  library = c("GUDHI", "Dionysus"), # must include GUDHI first
  location = TRUE,                  # also returns birth and death point locations          
  printProgress = FALSE
) 

alphaComplexDiag(
  X, 
  maxdimension = NCOL(X) - 1, 
  library = c("GUDHI", "Dionysus"), # must include GUDHI first
  location = TRUE,                  # also returns birth and death point locations AND representative cycles for each homological feature               
  printProgress = FALSE
) 


## RipsDiag

ripsDiag(
  X,                    # user must provide
  maxdimension = 1,     # user must provide
  maxscale = 2,         # user must provide
  dist = "euclidean",   # default, X is a coordinate matrix
  library = "GUDHI",    # default if dist = "euclidean", must be "GUDHI" or "Dionysus"
  location = FALSE,     #default
  printProgress = FALSE #default 
)

# TODO: add location = true options




### Data: distance matrix from point cloud
D <- as.matrix(dist(X))

# RipsDiag (dist)
ripsDiag(
  D,                    # user must provide
  maxdimension = 1,     # user must provide
  maxscale = 2,         # user must provide
  dist = "arbitrary",   # default, D is a dist object
  library = "Dionysus", # default if dist = "arbitrary", must be "GUDHI" or "Dionysus"
  location = FALSE,     #default
  printProgress = FALSE #default 
)

# TODO: add location = true options


# VietorisRips (dist)
# FIXME: fix format, add settings
vietoris_rips(D, max_dim = 1L, threshold = -1, p = 2L, dim = NULL)

