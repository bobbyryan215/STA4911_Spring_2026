library(TDA)
library(ripserr)
library(phutil)

# Data
data <- volcano


# TDA

# gridDiag sublevel gudhi
gridDiag(
  FUNvalues     = data,                     # must use FUNvalues for raster, user must provide array (or matrix)
  maxdimension  = length(dim(data)) - 1,    # default for FUNvalues
  sublevel      = TRUE,                     # default                   
  library       = "GUDHI",                  # default
  location      = FALSE,                    # default
  printProgress = FALSE                     # default
)

# gridDiag sublevel dyonysus 
gridDiag(
  FUNvalues    = data,
  maxdimension = length(dim(data)) - 1,
  sublevel     = TRUE,
  library      = "Dionysus",
  location      = FALSE,
  printProgress = FALSE 
)

# gridDiag sublevel dyonysus 
gridDiag(
  FUNvalues    = data,
  maxdimension = length(dim(data)) - 1,
  sublevel     = TRUE,
  library      = "PHAT",
  location      = FALSE,
  printProgress = FALSE 
)

# gridDiag superlevel gudhi
gridDiag(
  FUNvalues     = data,
  maxdimension  = length(dim(data)) - 1,
  sublevel      = FALSE, 
  library       = "GUDHI",
  location      = FALSE,                   
  printProgress = FALSE 
)

# gridDiag superlevel dyonysus
gridDiag(
  FUNvalues    = data,
  maxdimension = length(dim(data)) - 1,
  sublevel     = FALSE,
  library      = "Dionysus",
  location      = FALSE,                   
  printProgress = FALSE 
)

# gridDiag superlevel dyonysus
gridDiag(
  FUNvalues    = data,
  maxdimension = length(dim(data)) - 1,
  sublevel     = FALSE,
  library      = "PHAT",
  location      = FALSE,
  printProgress = FALSE 
)

# TODO: add location = true, diagLimit?


# ripserr

# cubical sublevel default *missing one point compared to TDA
cub_sub <- cubical(
  data,             # user must provide
  treshold = 9999,  # default
  method = "lj"     # default
  )
as.data.frame(cub_sub) # to see the persistence diagram

# cubical sublevel method = cp
cub_sub_cp <- cubical(
  data,             # user must provide
  treshold = 9999,  # default
  method = "cp"    
)
as.data.frame(cub_sub_cp) # to see the persistence diagram


# for superlevel negate data 
# FIXME: check what is happening
cub_sup <- cubical(-data)
as.data.frame(cub_sup) # compared to tda, points are negative and birth and death swapped 
transform(as.data.frame(cub_sup), birth = -birth, death = -death) # compared to tda, birth and death are swapped 
transform(as.data.frame(cub_sup), birth = -death, death = -birth) # this is almost the same as TDA (only first one is different)

