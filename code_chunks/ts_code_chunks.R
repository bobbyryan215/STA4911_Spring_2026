library(TDA)
library(ripserr)


#time series helper function
ts_to_point_cloud <- function(ts, data_dim, dim_lag = 1, sample_lag = 1) {
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

#time series into point cloud with vietoris_rips and ripsDiag
pc <- ts_to_point_cloud(ldeaths, 3, 1, 1)
rips <- vietoris_rips(pc, maxdimension = 1, maxscale = 1000)
rdiag <- ripsDiag(pc, maxdimension = 1, maxscale = 1000, library = "GUDHI")
as.data.frame(rips)
rdiag
