library(TDA)
library(ripserr)
library(phutil)

# Data
data <- volcano


# TDA

# gridDiag sublevel gudhi
gridDiag(
  FUNvalues     = data, 
  lim           = c(min(data), max(data)),
  maxdimension  = length(dim(data)) - 1,
  sublevel      = TRUE,
  library       = "GUDHI"
)

# gridDiag sublevel dyonysus 
gridDiag(
  FUNvalues    = data,
  lim          = c(min(data), max(data)),
  maxdimension = length(dim(data)) - 1,
  sublevel     = TRUE,
  library      = "Dionysus"
)

# gridDiag sublevel dyonysus 
gridDiag(
  FUNvalues    = data,
  lim          = c(min(data), max(data)),
  maxdimension = length(dim(data)) - 1,
  sublevel     = TRUE,
  library      = "PHAT"
)

# gridDiag superlevel gudhi
gridDiag(
  FUNvalues     = data, 
  lim           = c(min(data), max(data)),
  maxdimension  = length(dim(data)) - 1,
  sublevel      = FALSE, 
  library       = "GUDHI"
)

# gridDiag superlevel dyonysus
gridDiag(
  FUNvalues    = data,
  lim          = c(min(data), max(data)),
  maxdimension = length(dim(data)) - 1,
  sublevel     = FALSE,
  library      = "Dionysus"
)

# gridDiag superlevel dyonysus
gridDiag(
  FUNvalues    = data,
  lim          = c(min(data), max(data)),
  maxdimension = length(dim(data)) - 1,
  sublevel     = FALSE,
  library      = "PHAT"
)

# TODO: add location = true, diagLimit?


# ripserr

# ripserr cubical (sublevel) *missing one point compared to 
cub_sub <- cubical(data)
as.data.frame(cub_sub)


# for superlevel negate data 
# FIXME: check why its different
cub_sup <- cubical(-data)
as.data.frame(cub_sup)

