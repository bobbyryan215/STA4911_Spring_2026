library(TDA)
library(ripserr)
library(phutil)

# Data
data <- volcano


# TDA

# gridDiag sublevel GUDHI
gridDiag(
  FUNvalues     = data,                     # must use FUNvalues for raster, user must provide array (or matrix)
  maxdimension  = length(dim(data)) - 1,    # default for FUNvalues
  sublevel      = TRUE,                     # default                   
  library       = "GUDHI",                  # default
  location      = FALSE,                    # default
  printProgress = FALSE                     # default
)

# gridDiag sublevel Dionysus 
gridDiag(
  FUNvalues    = data,
  maxdimension = length(dim(data)) - 1,
  sublevel     = TRUE,
  library      = "Dionysus",
  location      = FALSE,
  printProgress = FALSE
)

# gridDiag sublevel PHAT 
gridDiag(
  FUNvalues    = data,
  maxdimension = length(dim(data)) - 1,
  sublevel     = TRUE,
  library      = "PHAT",
  location      = FALSE,
  printProgress = FALSE
)

# gridDiag superlevel GUDHI
gridDiag(
  FUNvalues     = data,
  maxdimension  = length(dim(data)) - 1,
  sublevel      = FALSE, 
  library       = "GUDHI",
  location      = FALSE,                   
  printProgress = FALSE
)

# gridDiag superlevel Dionysus
gridDiag(
  FUNvalues     = data,
  maxdimension  = length(dim(data)) - 1,
  sublevel      = FALSE,
  library       = "Dionysus",
  location      = FALSE,                   
  printProgress = FALSE
)

# gridDiag superlevel PHAT
gridDiag(
  FUNvalues    = data,
  maxdimension = length(dim(data)) - 1,
  sublevel     = FALSE,
  library      = "PHAT",
  location      = FALSE,
  printProgress = FALSE
)

# TODO: add location = true, diagLimit?

# gridDiag Dionysus, location = TRUE 
gridDiag(
  FUNvalues     = data,
  lim           = rbind(c(1, 1), c(nrow(data), ncol(data))), # if location = true, user must provide 
  by            = c(1, 1),                                   # if location = true, user must provide
  maxdimension  = length(dim(data)) - 1,
  sublevel      = TRUE,
  library       = "Dionysus",                                # if location = true, library must be "Dionysus" or "PHAT"
  location      = TRUE,                                     
  printProgress = FALSE
)


# gridDiag PHAT, location = TRUE 
gridDiag(
  FUNvalues     = data,
  lim           = rbind(c(1, 1), c(nrow(data), ncol(data))), 
  by            = c(1, 1),                                   
  maxdimension  = length(dim(data)) - 1,
  sublevel      = TRUE,
  library       = "PHAT",                                   
  location      = TRUE,                                     
  printProgress = FALSE
)

# gridDiag sublevel, diagLimit
gridDiag(
  FUNvalues     = data,
  maxdimension  = length(dim(data)) - 1,
  sublevel      = TRUE,
  library       = "GUDHI",
  location      = FALSE,
  printProgress = FALSE,
  diagLimit     = max(data) + 10   # replaces Inf if sublevel = true
)

# gridDiag superlevel GUDHI, diagLimit
gridDiag(
  FUNvalues     = data,
  maxdimension  = length(dim(data)) - 1,
  sublevel      = FALSE,
  library       = "GUDHI",
  location      = FALSE,
  printProgress = FALSE,
  diagLimit     = min(data) - 10   # replaces -Inf if sublevel = false
)


# ripserr

# cubical sublevel default 
cub_sub <- cubical(
  data,                # user must provide matrix or array
  threshold = 9999,    # default
  method = "lj"        # default
  )
as.data.frame(cub_sub) # persistence diagram

# cubical sublevel method = cp
cub_sub_cp <- cubical(
  data,             
  threshold = 9999,  
  method = "cp"    
)
as.data.frame(cub_sub_cp) 


# cubical superlevel 
cub_sup <- cubical(
  -data,            # negate data
  threshold = 9999,
  method = "lj"
  )
transform(as.data.frame(cub_sup), birth = -birth, death = -death) # negate birth and death

