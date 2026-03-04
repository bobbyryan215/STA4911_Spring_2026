library(TDA)
library(ripserr)
library(igraph)



compute_persistence <- function(data, s7_obj) {
  UseMethod("compute_persistence", data)
}

compute_persistence.matrix <- function(data, s7_obj) {
  if (s7_obj@math_object == "raster") {
    if (s7_obj@engine == "ripserr") {
      cubical(data) |> as_persistence()
    }
    if (s7_obj@engine == "TDA") {
      gridDiag(FUNvalues = data, library = s7_obj@library) |> as_persistence()
    }
  }
  if (s7_obj@math_object == "point cloud") {
    if (s7_obj@engine == "ripserr") {
      vietoris_rips(data) |> as_persistence()
    }
    if (s7_obj@engine == "TDA") {
      if (s7_obj@filtration == "vietoris_rips") {
        ripsDiag(data, library = s7_obj@library) |> as_persistence()
      }
      if (s7_obj@filtration == "alpha_complex") {
        alphaComplexDiag(data, library = s7_obj@library) |> as_persistence()
      }
      if (s7_obj@filtration == "alpha_shape") {
        alphaShapeDiag(data, library = s7_obj@library) |> as_persistence() 
      }
    }
  }
}

compute_persistence.ts <- function(data, s7_obj) {
  pc <- ts_to_point_cloud(data, s7_obj@data_dim, s7_obj@dim_lag, s7_obj@sample_lag)
  if (s7_obj@engine == "ripserr") {
    vietoris_rips(data) |> as_persistence()
  }
  if (s7_obj@engine == "TDA") {
    if (s7_obj@filtration == "vietoris_rips") {
      ripsDiag(data, library = s7_obj@library) |> as_persistence()
    }
  }
}

compute_persistence.dist <- function(data, s7_obj) {
  if (s7_obj@engine == "ripserr") {
    vietoris_rips(data) |> as_persistence()
  }
  if (s7_obj@engine == "TDA") {
    ripsDiag(data, library = s7_obj@library) |> as_persistence()
  }
}

compute_persistence.igraph <- function(data, s7_obj) {
  if (s7_obj@engine == "ripserr") {
    vietoris_rips(data) |> as_persistence()
  }
  if (s7_obj@engine == "TDA") {
    ripsDiag(data, library = s7_obj@library) |> as_persistence()
  }
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

s7_obj_param_extractor(s7_obj) {
  
}