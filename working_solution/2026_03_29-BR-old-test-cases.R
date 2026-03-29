library(S7)
library(TDA)
library(ripserr)
library(phutil)

filtration_type <- new_property(
  class = class_character,
  validator = function(value) {
    if (  !(value %in% c("vietoris_rips", "alpha_shape" ,"alpha_complex", "cubical")) )
      "must be vietoris_rips, cubical, alpha_shape, or alpha_complex"
  },
  default = "vietoris_rips"
)

maxdimension_type <- new_property(
  class = class_double,
  validator = function(value) {
    if (!is.na(value) & value < 0)
      "must be a non-negative integer"
  },
  default = 1
)


engine_type <- new_property(
  class = class_character,
  validator = function(value) {
    if (  !(value %in% c("TDA","ripserr")) )
      "must be TDA or ripserr"
  },
  default ="TDA"
)


library_type <- new_property(
  class = class_character,
  validator = function(value) {
    if (  !(value %in% c("GUDHI", "PHAT", "Dionysus", NA_character_)) )
      "must be GUDHI, PHAT, or Dionysus or NA_character_"
  },
  default = NA_character_
)



#FIXME: Do we want a larger parent class. It may be useful to that all subclasses are related. This can easily be removed
PH <- new_class("PH",
                properties = list(
                  filtration = filtration_type,
                  engine = engine_type,
                  library = library_type,
                  max_dimension = maxdimension_type),
                validator = function(self) {
                  if (self@engine == "ripserr" & !is.na(self@library) ){
                    sprintf("Library is only defined when engine is TDA. Please leave library blank or NA_character_ when using the ripserr engine.")
                  }
                  
                }
                
)



#read only property
#best solution I could find
maxdiameter_type = new_property(
  class = class_double,
  default = NA_real_)


maxradius_type = new_property(
  class = class_double,
  getter = function(self) {
    self@max_diameter/2
  }
)



PH_pointcloud <-  new_class("PH_pointcloud", parent = PH,
                            properties = list(max_radius= maxradius_type,
                                              max_diameter= maxdiameter_type),
                            validator = function(self) {
                              # FIXME: check if additional filtration check is needed
                              if (self@engine == "ripserr" & (self@filtration == "alpha_complex" || self@filtration=="alpha_shape")  ){
                                sprintf("Alpha complexes are only defined for the engine TDA using point clouds. Please use library TDA for any alpha filtration")
                              }
                              
                            }
)

sublevel_type <- new_property(
  class = class_logical,
  validator = function(value) {
    if (length(value) != 1 || is.na(value))
      "sublevel must be either TRUE or FALSE."
  },
  default = TRUE
)

# raster subclass (includes object specific parameters)
# TODO: should sublevel be included here or in method
PH_raster <- new_class(
  "PH_raster", 
  parent = PH,
  properties = list(
    max_diameter = maxdiameter_type,
    sublevel = sublevel_type
  )
)

matrix_class <- new_S3_class("matrix")
dist_class <- new_S3_class("dist")
array_class <- new_S3_class("array")

#TODO: add helper function to handle extra parameters so we can pass them into the persistence functions
compute_persistence <- new_generic("compute_persistence", c("obj","data"))
method(compute_persistence, list(PH_pointcloud, matrix_class)) <- function(obj, data) {
  if (obj@engine == "ripserr") {
    vietoris_rips(
      data,
      max_dim = obj@max_dimension,
      threshold = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  else if (obj@engine == "TDA") {
    if (obj@filtration == "vietoris_rips") {
      ripsDiag(
        data,
        library = obj@library,
        maxdimension = obj@max_dimension,
        maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
      ) |> as_persistence()
    }
    if (obj@filtration == "alpha_complex") {
      alphaComplexDiag(
        data,
        library = obj@library,
        maxdimension = obj@max_dimension,
        maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
      ) |> as_persistence()
    }
    if (obj@filtration == "alpha_shape") {
      alphaShapeDiag(
        data,
        library = obj@library,
        maxdimension = obj@max_dimension,
        maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
      ) |> as_persistence() 
    }
  }
}

method(compute_persistence, list(PH_pointcloud, dist_class)) <- function(obj, data) {
  if (obj@engine == "ripserr") {
    vietoris_rips(
      data,
      max_dim = obj@max_dimension,
      threshold = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  else if (obj@engine == "TDA") {
    if (obj@filtration == "vietoris_rips") {
      ripsDiag(
        data,
        library = obj@library,
        maxdimension = obj@max_dimension,
        dist = "arbitrary",
        maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
      ) |> as_persistence()
    }
    if (obj@filtration == "alpha_complex") {
      alphaComplexDiag(
        data,
        library = obj@library,
        maxdimension = obj@max_dimension,
        maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
      ) |> as_persistence()
    }
    if (obj@filtration == "alpha_shape") {
      alphaShapeDiag(
        data,
        library = obj@library,
        maxdimension = obj@max_dimension,
        #FIXME: Bobby- we should remove to fix this
        #maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
      ) |> as_persistence() 
    }
  }
}
method(compute_persistence, list(PH_raster, matrix_class)) <- function(obj, data) {
  data <- as.array(data)
  compute_persistence(obj, data)
}
method(compute_persistence, list(PH_raster, array_class)) <- function(obj, data) {
  if (obj@engine == "ripserr") {
    cubical(
      data,
      threshold = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  else if (obj@engine == "TDA") {
    gridDiag(
      FUNvalues = data,
      library = obj@library,
      sublevel = obj@sublevel,
      maxdimension = obj@max_dimension
    ) |> as_persistence()
  }
}





#vietoris_rips- tda - dist
#Dispatch- does not work- returns NULL
#Internal code- does not work- returns NULL
#Function call- works
#Internal code assigned within- works



x <- PH_pointcloud(
  engine = "TDA", library = "GUDHI", filtration = "vietoris_rips",
  max_diameter = 1000
)
# example using double-dispatch S7 method
# `method(compute_persistence, list(PH_pointcloud, dist_class))`
# returns `NULL`
res <- compute_persistence(x, eurodist)
res
# same example using internal code without dispatch
# returns `NULL`
obj <- x
data <- eurodist
res <- if (obj@engine == "ripserr") {
  vietoris_rips(
    data,
    max_dim = obj@max_dimension,
    threshold = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
  ) |> as_persistence()
} else if (obj@engine == "TDA") {
  if (obj@filtration == "vietoris_rips") {
    ripsDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      dist = "arbitrary",
      maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_complex") {
    alphaComplexDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_shape") {
    alphaShapeDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence() 
  }
}
res
# same example using only code executed through conditionals
# returns meaningful answer
res <- ripsDiag(
  data,
  library = obj@library,
  maxdimension = obj@max_dimension,
  dist = "arbitrary",
  maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
) |> as_persistence()
res
res |> as.data.frame()
# TEST: assign conditional results to a named object and return the object
# returns meaningful answer
if (obj@engine == "ripserr") {
  res <- vietoris_rips(
    data,
    max_dim = obj@max_dimension,
    threshold = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
  ) |> as_persistence()
} else if (obj@engine == "TDA") {
  if (obj@filtration == "vietoris_rips") {
    res <- ripsDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      dist = "arbitrary",
      maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_complex") {
    res <- alphaComplexDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_shape") {
    res <- alphaShapeDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence() 
  }
}
res










#vietoris_rips- risperr - dist
#Dispatch- works
#Internal code- works
#Function call- works
#Internal code assigned within- works

x <- PH_pointcloud(
  engine = "ripserr", filtration = "vietoris_rips",
  max_diameter = 1000
)
# example using double-dispatch S7 method
# `method(compute_persistence, list(PH_pointcloud, matrix))`
# returns menaingful output
data <- eurodist
res <- compute_persistence(x, data)
res


# same example using internal code without dispatch
# returns meaningful answer
obj <- x
res <- if (obj@engine == "ripserr") {
  vietoris_rips(
    data,
    max_dim = obj@max_dimension,
    threshold = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
  ) |> as_persistence()
} else if (obj@engine == "TDA") {
  if (obj@filtration == "vietoris_rips") {
    ripsDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      dist = "arbitrary",
      maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_complex") {
    alphaComplexDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_shape") {
    alphaShapeDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      #FIXME: Bobby- we should remove to fix this
      #maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence() 
  }
}
res
# same example using only code executed through conditionals
# returns meaningful answer
res <- vietoris_rips(
  data,
  max_dim = obj@max_dimension,
  threshold = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
) |> as_persistence()
res
res |> as.data.frame()

# TEST: assign conditional results to a named object and return the object
# returns meaningful answer
if (obj@engine == "ripserr") {
  res <- vietoris_rips(
    data,
    max_dim = obj@max_dimension,
    threshold = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
  ) |> as_persistence()
} else if (obj@engine == "TDA") {
  if (obj@filtration == "vietoris_rips") {
    res <- ripsDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      dist = "arbitrary",
      maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_complex") {
    res <- alphaComplexDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      #maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_shape") {
    res <- alphaShapeDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      #maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence() 
  }
}
res














#alpha_shape- TDA
#Dispatch- does not work
#Internal code- works but does not respect max dimension argument
#Function call- works but does not respect max dimension argument
#Internal code assigned within- works but does not respect max dimension argument 

x <- PH_pointcloud(
  engine = "TDA", library = "GUDHI", filtration = "alpha_shape",
  max_diameter = 1000
)
# example using double-dispatch S7 method
# `method(compute_persistence, list(PH_pointcloud, matrix))`
# returns Error: Can't find method for generic `compute_persistence(obj, data)`:
data <- matrix(rnorm(100), ncol = 4)
res <- compute_persistence(x, data)
res


# same example using internal code without dispatch
# returns meaningful answer
obj <- x
res <- if (obj@engine == "ripserr") {
  vietoris_rips(
    data,
    max_dim = obj@max_dimension,
    threshold = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
  ) |> as_persistence()
} else if (obj@engine == "TDA") {
  if (obj@filtration == "vietoris_rips") {
    ripsDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      dist = "arbitrary",
      maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_complex") {
    alphaComplexDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_shape") {
    alphaShapeDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      #FIXME: Bobby- we should remove to fix this
      #maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence() 
  }
}
res
# same example using only code executed through conditionals
# returns meaningful answer
res <- alphaShapeDiag(
  data,
  library = obj@library,
  maxdimension = obj@max_dimension,
) |> as_persistence()
res
res |> as.data.frame()

# TEST: assign conditional results to a named object and return the object
# returns meaningful answer
if (obj@engine == "ripserr") {
  res <- vietoris_rips(
    data,
    max_dim = obj@max_dimension,
    threshold = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
  ) |> as_persistence()
} else if (obj@engine == "TDA") {
  if (obj@filtration == "vietoris_rips") {
    res <- ripsDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      dist = "arbitrary",
      maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_complex") {
    res <- alphaComplexDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      #maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_shape") {
    res <- alphaShapeDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      #maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence() 
  }
}
res






#alpha_complex TDA
#Dispatch- does not work- returns Error: Can't find method for generic `compute_persistence(obj, data)`
#Internal Code- does not work-returns NULL
#Function Call- works but does not respect max dimension argument- Sometimes crashes R!??
#Internal code assigned within- works but does not respect max dimension argument 

x <- PH_pointcloud(
  engine = "TDA", library = "GUDHI", filtration = "alpha_complex",
  max_diameter = 1000
)
# example using double-dispatch S7 method
# `method(compute_persistence, list(PH_pointcloud, matrix))`
# returns Error: Can't find method for generic `compute_persistence(obj, data)`:

data <- matrix(rnorm(30), ncol = 3)
res <- compute_persistence(x, data)
res


# same example using internal code without dispatch
# returns `NULL`
#VERY ODD THAT THIS DOES NOT WORK AND THE ONE ABOVE DOES
obj <- x
res <- if (obj@engine == "ripserr") {
  vietoris_rips(
    data,
    max_dim = obj@max_dimension,
    threshold = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
  ) |> as_persistence()
} else if (obj@engine == "TDA") {
  if (obj@filtration == "vietoris_rips") {
    ripsDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      dist = "arbitrary",
      maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_complex") {
    alphaComplexDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      #FIXME: Bobby- we should remove to fix this
      #maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_shape") {
    alphaShapeDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      #FIXME: Bobby- we should remove to fix this
      #maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence() 
  }
}
res
# same example using only code executed through conditionals
# returns meaningful answer
res <- alphaComplexDiag(
  data,
  library = obj@library,
  maxdimension = obj@max_dimension,
) |> as_persistence()
res
res |> as.data.frame()

# TEST: assign conditional results to a named object and return the object
# returns meaningful answer
if (obj@engine == "ripserr") {
  res <- vietoris_rips(
    data,
    max_dim = obj@max_dimension,
    threshold = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
  ) |> as_persistence()
} else if (obj@engine == "TDA") {
  if (obj@filtration == "vietoris_rips") {
    res <- ripsDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      dist = "arbitrary",
      maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_complex") {
    res <- alphaComplexDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      #maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_shape") {
    res <- alphaShapeDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      #maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence() 
  }
}
res








#vietoris_rips tda coordinate matrices
#Dispatch- does not work- returns Error: Can't find method for generic `compute_persistence(obj, data)`
#Internal Code- does not work- returns NULL
#Function Call- works
#Internal code assigned within- works

x <- PH_pointcloud(
  engine = "TDA", filtration = "vietoris_rips", library= "GUDHI",
  max_diameter = 1000, max_dimension = 2
)
# example using double-dispatch S7 method
# `method(compute_persistence, list(PH_pointcloud, matrix))`
# returns Error: Can't find method for generic `compute_persistence(obj, data)`:
data <- matrix(rnorm(100), ncol = 4)
res <- compute_persistence(x, data)

res


# same example using internal code without dispatch
# returns NULL
obj <- x
res <- if (obj@engine == "ripserr") {
  vietoris_rips(
    data,
    max_dim = obj@max_dimension,
    threshold = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
  ) |> as_persistence()
} else if (obj@engine == "TDA") {
  if (obj@filtration == "vietoris_rips") {
    ripsDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_complex") {
    alphaComplexDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      #FIXME: Bobby- we should remove to fix this
      #maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_shape") {
    alphaShapeDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      #FIXME: Bobby- we should remove to fix this
      #maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence() 
  }
}
res
# same example using only code executed through conditionals
# returns meaningful answer
res <- ripsDiag(
  data,
  library = obj@library,
  maxdimension = obj@max_dimension,
  maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
) |> as_persistence()

res
res |> as.data.frame()

# TEST: assign conditional results to a named object and return the object
# returns meaningful answer
if (obj@engine == "ripserr") {
  res <- vietoris_rips(
    data,
    max_dim = obj@max_dimension,
    threshold = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
  ) |> as_persistence()
} else if (obj@engine == "TDA") {
  if (obj@filtration == "vietoris_rips") {
    res <- ripsDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_complex") {
    res <- alphaComplexDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      #maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_shape") {
    res <- alphaShapeDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      #maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence() 
  }
}
res








#vietoris_rips ripserr
#Dispatch- does not work- returns Error: Can't find method for generic `compute_persistence(obj, data)`
#Internal Code- meaningful result
#Function Call- meaningful result
#Internal code assigned within- works

x <- PH_pointcloud(
  engine = "ripserr", filtration = "vietoris_rips",
  max_diameter = 1000, max_dimension = 2
)
# example using double-dispatch S7 method
# `method(compute_persistence, list(PH_pointcloud, matrix))`
# returns Error: Can't find method for generic `compute_persistence(obj, data)`:
data <- matrix(rnorm(100), ncol = 4)
res <- compute_persistence(x, data)

res


# same example using internal code without dispatch
# returns meaningful answer
obj <- x
res <- if (obj@engine == "ripserr") {
  vietoris_rips(
    data,
    max_dim = obj@max_dimension,
    threshold = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
  ) |> as_persistence()
} else if (obj@engine == "TDA") {
  if (obj@filtration == "vietoris_rips") {
    ripsDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      dist = "arbitrary",
      maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_complex") {
    alphaComplexDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      #FIXME: Bobby- we should remove to fix this
      #maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_shape") {
    alphaShapeDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      #FIXME: Bobby- we should remove to fix this
      #maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence() 
  }
}
res
# same example using only code executed through conditionals
# returns meaningful answer
res <- vietoris_rips(
  data,
  max_dim = obj@max_dimension,
  threshold = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
) |> as_persistence()


res
res |> as.data.frame()

# TEST: assign conditional results to a named object and return the object
# returns meaningful answer
if (obj@engine == "ripserr") {
  res <- vietoris_rips(
    data,
    max_dim = obj@max_dimension,
    threshold = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
  ) |> as_persistence()
} else if (obj@engine == "TDA") {
  if (obj@filtration == "vietoris_rips") {
    res <- ripsDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      dist = "arbitrary",
      maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_complex") {
    res <- alphaComplexDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      #maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence()
  }
  if (obj@filtration == "alpha_shape") {
    res <- alphaShapeDiag(
      data,
      library = obj@library,
      maxdimension = obj@max_dimension,
      #maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
    ) |> as_persistence() 
  }
}
res

