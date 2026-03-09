library(TDA)
library(ripserr)
library(S7)


compute_persistence <- function(data, object, filtration, engine) {
  if (engine == "ripserr") {
    if ((object == "point cloud" || object == "metric space") && filtration == "vietoris_rips") {
      as.data.frame(vietoris_rips(data, maxdimension = 1, maxscale = 2))
    }
    else if (object == "raster" && filtration == "cubical") {
      as.data.frame(cubical(data))
    }
  }
  else if (engine == "TDA") {
    if ((object == "point cloud" || object == "metric space") && filtration == "ripsDiag") {
      ripsDiag(data, maxdimension = 1, maxscale = 2, library = "GUDHI")
    }
    if (object == "raster" && filtration == "gridDiag") {
      gridDiag(FUNvalues = data)
    }
  }
}


compute_persistence(volcano, "raster", "gridDiag", "TDA")


library(S7)

#first draft
PH <- new_class("PH",
                   properties = list(
                     data = class_double, #this may not be correct
                     object = new_property(class_character, default = "point cloud"),
                     filtration = new_property(class_character, default = "vietoris_rips"),
                       engine = new_property(class_character, default = "TDA")
                   ),
                
                
                validator = function(self) {
                  if ( !(self@object %in% c("raster","point cloud", "distance matrix", "time series")) ) {
                    sprintf(
                      "Object must be raster, point cloud, distance matrix, or time series"
                    )
                  }
                  else if ( !(self@engine %in% c("TDA","ripserr")) ) {
                    sprintf(
                      "Engine must be TDA or ripserr"
                    )
                  }
                  
                  
                 else if ( !(self@filtration %in% c("vietoris_rips","ripsDiag")) ) {
                    sprintf(
                      "Filtration must be vietoris_rips or ripsDiag"
                    )
                  }
                  
            
                  
                }
                
                )
                

x<- PH(volcano,"point cloud", "vietoris_rips", "TDA")

speak <- new_generic("speak", "x")

method(speak, PH) <- function(x) {
  "I am a persistent homology"
}

  
speak(x)



describe <- new_generic("describe", "x")
method(describe, PH) <- function(x) {
  paste0("This object is for persistent homology of data of type ", x@object, " with the ", x@filtration, " using the engine ", x@engine)
}


describe(x)

x<- PH(volcano,"point cloud", "vietoris_rips", "TDA")



rm(list = ls())























#second draft
#NEED TO CHANGE filtration to vietoris_rips, alpha, cubical??
#Change filtration to work properly
data_type = new_property(
  class_double,
  default = quote(stop("non-empty data is required")))


object_type <- new_property(
  class = class_character,
  validator = function(value) {
    if ( !(value %in% c("raster","point_cloud", "distance_matrix", "time_series") ) )
      "must be raster, point_cloud, distance_matrix, or time_series"
  },
  default = "point_cloud"
)


engine_type <- new_property(
  class = class_character,
  validator = function(value) {
    if (  !(value %in% c("TDA","ripserr")) )
      "must be TDA or ripserr"
  },
  default ="TDA"
)


filtration_type <- new_property(
  class = class_character,
  validator = function(value) {
    if (  !(value %in% c("vietoris_rips","ripsDiag", "gridDiag")) )
      "must be vietoris_rips, ripsDiag, or gridDiag"
  },
  default = "vietoris_rips"
)




library_type <- new_property(
  class = class_character,
  validator = function(value) {
    if (  !(value %in% c("GUDHI", "PHAT", "Dionysus","")) )
      "must be GUDHI, PHAT, or Dionysus"
  },
  default = ""
)





PH <- new_class("PH",
                properties = list(
                  data = data_type, 
                  object = object_type,
                  filtration = filtration_type,
                  engine = engine_type,
                  library = library_type),
                  validator = function(self) {
                    
                    if (self@engine == "ripserr" & self@library != "" ){
                      sprintf("Library is only defined when engine is TDA. Please leave library blank when using the ripserr engine.")
                    }
                    
                  }
                )


x <- PH(volcano, "raster","gridDiag","TDA", "PHAT")



# FIXME: consider adding engine variable for only the instance where library is TDA?
# FIXME: THIS IS NOT CORRECT> FIRST IF ELSE IS WRONG
compute_persistence <- new_generic("compute_persistence", "x")
method(compute_persistence, PH) <- function(x) {
  if (x@engine == "ripserr") {
    if ((x@object == "point_cloud" || x@object == "metric_space") && x@filtration == "vietoris_rips") {
      as.data.frame(vietoris_rips(x@data, maxdimension = 1, maxscale = 2))
    }
    else if (x@object == "time_series" && x@filtration == "vietoris_rips") {
      embedded_point_cloud <- embed(as.numeric(x@data), 3)
      as.data.frame(vietoris_rips(embedded_point_cloud, maxdimension = 1, maxscale = 2))
    }
    else if (x@object == "raster" && x@filtration == "cubical") {
      as.data.frame(cubical(x@data))
    }
    else {
        return("This combination of engine, object and filtration does not compute. Please check and try again.")
      }
    }
  else if (x@engine == "TDA") {
    if ((x@object == "point_cloud" || x@object == "metric_space") && x@filtration == "ripsDiag") {
      ripsDiag(x@data, maxdimension = 1, maxscale = 2, library = "GUDHI")
    }
    else if (x@object == "distance_matrix" && x@filtration == "ripsDiag") {
      ripsDiag(x@data, maxdimension = 1, maxscale = 2, dist = "arbitrary", library = "GUDHI")
    }
    else if (x@object == "raster" && x@filtration == "gridDiag") {
      gridDiag(FUNvalues = x@data)
    }
    else {
      return("This combination of engine, object, and filtration does not compute. Please check and try again.")
    }
  }
}



compute_persistence(x)


x <- PH(volcano, "raster","ripsDiag","TDA", "PHAT")
























#third draft



data_type = new_property(
  class_double,
  default = quote(stop("non-empty data is required")))


object_type <- new_property(
  class = class_character,
  validator = function(value) {
    if ( !(value %in% c("raster","point_cloud", "distance_matrix", "time_series", "graph") ) )
      "must be raster, point_cloud, distance_matrix, time_series, or graph"
  },
  default = "point_cloud"
)


engine_type <- new_property(
  class = class_character,
  validator = function(value) {
    if (  !(value %in% c("TDA","ripserr")) )
      "must be TDA or ripserr"
  },
  default ="TDA"
)


filtration_type <- new_property(
  class = class_character,
  validator = function(value) {
    if (  !(value %in% c("vietoris_rips","cubical", "alpha_shape" ,"alpha_complex")) )
      "must be vietoris_rips, cubical, alpha_shape, or alpha_complex"
  },
  default = "vietoris_rips"
)




library_type <- new_property(
  class = class_character,
  validator = function(value) {
    if (  !(value %in% c("GUDHI", "PHAT", "Dionysus","")) )
      "must be GUDHI, PHAT, or Dionysus"
  },
  default = ""
)



# FIXME: ?  Do we want the object to raise give the issues, or only when we try to compute persistence??
PH <- new_class("PH",
                properties = list(
                  data = data_type, 
                  object = object_type,
                  filtration = filtration_type,
                  engine = engine_type,
                  library = library_type),
                validator = function(self) {
                  
                  if (self@engine == "ripserr" & self@library != "" ){
                    sprintf("Library is only defined when engine is TDA. Please leave library blank when using the ripserr engine.")
                  }
                  
                  else if (self@engine == "ripserr" & (self@filtration == "alpha_complex" || self@filtration=="alpha_shape")  ){
                    sprintf("Alpha complexes are only defined for the engine TDA. Please use library TDA for any alpha filtration")
                  }
                  
                }
)


x <- PH(volcano, "raster","vietoris_rips","TDA", "PHAT")

PH(volcano, "raster","alpha_shape","ripserr", "PHAT")

PH(volcano, "raster","alpha_shape","ripserr")

PH(volcano, "raster","alpha_shape","ripserr")



# FIXME: add all of the combinations
compute_persistence <- new_generic("compute_persistence", "x")
method(compute_persistence, PH) <- function(x) {
  if (x@engine == "ripserr") {
    if ((x@object == "point_cloud" || x@object == "metric_space") && x@filtration == "vietoris_rips") {
      as.data.frame(vietoris_rips(x@data, maxdimension = 1, maxscale = 2))
    }
    else if (x@object == "time_series" && x@filtration == "vietoris_rips") {
      embedded_point_cloud <- embed(as.numeric(x@data), 3)
      as.data.frame(vietoris_rips(embedded_point_cloud, maxdimension = 1, maxscale = 2))
    }
    else if (x@object == "raster" && x@filtration == "cubical") {
      as.data.frame(cubical(x@data))
    }
    else {
      return("This combination of engine, object and filtration does not compute. Please check and try again.")
    }
  }
  else if (x@engine == "TDA") {
    if ((x@object == "point_cloud" || x@object == "metric_space") && x@filtration == "vietoris_rips") {
      ripsDiag(x@data, maxdimension = 1, maxscale = 2, library = "GUDHI")
    }
    else if (x@object == "distance_matrix" && x@filtration == "vietoris_rips") {
      ripsDiag(x@data, maxdimension = 1, maxscale = 2, dist = "arbitrary", library = "GUDHI")
    }
    else if (x@object == "raster" && x@filtration == "cubical") {
      gridDiag(FUNvalues = x@data)
    }
    else {
      return("This combination of engine, object, and filtration does not compute. Please check and try again.")
    }
  }
}



compute_persistence(x)


x <- PH(volcano, "raster","cubical","TDA", "PHAT")


compute_persistence(x)


# FIXME: add preprocess function to avoid below issues
x <- PH(iris[1:4])

x <- PH(as.matrix(iris[1:4]))

compute_persistence(x)


# add parent classes for each object type


