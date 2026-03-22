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
        maxscale = ifelse(is.na(obj@max_diameter), 0, obj@max_diameter)
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

