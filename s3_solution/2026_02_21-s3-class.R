library(TDA)
library(ripserr)

# first create the class object
# need to add some way to store extra parameters for functions calls
# need to add better way of verifying integers instead of having to put L after the number
PH_ts_constructor <- function(data, data_dim, dim_lag, sample_lag, filtration = "vietoris_rips", 
                              engine = "ripserr", library = "", params) {
  if (!is.ts(data)) {
    stop("data must be a time series object")
  }
  if (!(is.integer(data_dim) && is.integer(dim_lag) && is.integer(sample_lag))) {
    stop("data_dim, dim_lag, and sample_lag paramaters must all be integers")
  }
  if (!(filtration %in% c("vietoris_rips","ripsDiag"))) {
    print("filtration must be either 'vietoris_rips' or 'ripsDiag' for a time series object")
    return(0)
  }
  if (!(engine %in% c("TDA","ripserr"))) {
    stop("engine must be either 'ripserr' or 'TDA'")
  }
  if (!(library %in% c("GUDHI", "PHAT", "Dionysus",""))) {
    stop("library must be either 'GUDHI', 'PHAT', 'Dionysus', or blank")
  }
  if (engine == "ripserr" && library != "") {
    stop("library is only defined in 'TDA' engine")
  }
  
  object <- list(data = data, data_dim = data_dim, dim_lag = dim_lag,
                 sample_lag = sample_lag, filtration = filtration,
                 engine = engine, library = library)
  class(object) <- "PH_ts"
  return(object)
}

ts_to_point_cloud <- function(ts, data_dim, dim_lag, sample_lag) {
  window_width <- (data_dim - 1) * dim_lag
  start <- seq(from = 1, to = length(ts) - window_width, by = sample_lag) # starting indeces for each window
  window_count <- length(start)
  mat <- matrix(data = NA, ncol = data_dim, nrow = window_count)
  for (i in 1:data_dim) { # fill by column
    offset <- (i - 1) * dim_lag # shift time series for each column
    mat[, i] <- ts[start+offset] # fill column with sequence values shifted by offset
  }
  return(mat)
}

print.PH_ts <- function(obj) {
  cat("Persistent Homology for Time Series\n")
  cat("-----------------------------------\n")
  cat("Filtration used: ", obj$filtration, "\n")
  cat("Engine used: ", obj$engine, "\n")
  lib <- ifelse(obj$library == "", "none", obj$library)
  cat("Library used: ", lib, "\n")
}


