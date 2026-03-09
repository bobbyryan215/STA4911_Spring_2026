library(S7)
library(TDA)
library(ripserr)
library(phutil)



#the following definitions of types are global in that are not filtration specific. 
engine_type <- new_property(
  class = class_character,
  validator = function(value) {
    if (  !(value %in% c("TDA","ripserr")) )
      "must be TDA or ripserr"
  },
  default ="TDA"
)

#TODO: see if not having cubical is correct
#TODO: see how to handle other objects
filtration_type <- new_property(
  class = class_character,
  validator = function(value) {
    if (  !(value %in% c("vietoris_rips", "alpha_shape" ,"alpha_complex")) )
      "must be vietoris_rips, alpha_shape, or alpha_complex"
  },
  default = "vietoris_rips"
)

#FIXME: should we change the default value?
library_type <- new_property(
  class = class_character,
  validator = function(value) {
    if (  !(value %in% c("GUDHI", "PHAT", "Dionysus", NA_character_)) )
      "must be GUDHI, PHAT, Dionysus, or NA"
  },
  default = NA_character_
)


maxdimension_type <- new_property(
  class = class_double,
  validator = function(value) {
    if (!is.na(value) && value < 0)
      "must be a non-negative integer"
  },
  default = NA_real_
)

maxscale_type <- new_property(
  class = class_double,
  validator = function(value) {
    if (  (!is.na(value) && value <= 0 ))
      "must be a positive real number"
  },
  default = NA_real_
)

params_type <- new_property(
  class = class_list,
  validator = function(value) {
    
    if (  !all(names(value) %in% c("dist","location","printProgress","data_dim","dim_lag","sample_lag","method")  ))
      "only acceptable paramaters are dist, location, printProgress, data_dim, dim_lag, sample_lag, and method"
  },
  default = list()
)

#TODO: decide if we should have one for all objects or one for each
params_raster_type <- new_property(
  class = class_list,
  validator = function(value) {
    if ( !all(names(value) %in% c("sublevel","threshold","method","location", "lim", "by", "printProgress")) )
      "only acceptable parameters are sublevel, threshold, method, location, lim, by, and printProgress"
  },
  default = list()
)


PH <- new_class("PH",
  properties = list(
    engine = engine_type,
    library = library_type,
    maxdimension = maxdimension_type
  ),
  validator = function(self) {
    if (self@engine == "ripserr" && !is.na(self@library)) {
      "Library is only defined when engine is TDA."
    }
  }
)


PH_pointcloud <-  new_class("PH_pointcloud", parent = PH,
                            properties = list(
                              filtration = filtration_type,
                              maxscale = maxscale_type,
                              params = params_type),
                            validator = function(self) {
                              
                            
                              # FIXME: check if additional filtration check is needed
                              if (self@engine == "ripserr" && (self@filtration == "alpha_complex" || self@filtration=="alpha_shape")  ){
                                sprintf("Alpha complexes are only defined for the engine TDA. Please use library TDA for any alpha filtration")
                              }
                              
                            }
)

#TODO: see how to hande sublevel = false in ripserr
#TODO: validate params value types?
PH_raster_class <- new_class("PH_raster", parent = PH,
                       properties = list(
                         params = params_raster_type),
                       validator = function(self) {
                         if (self@engine == "TDA" && ("threshold" %in% names(self@params))) {
                           sprintf("threshold is only used when engine is ripserr. Please remove threshold from params or set engine = ripserr.")
                         }
                         if (self@engine == "TDA" && ("method" %in% names(self@params))) {
                           sprintf("method is only used when engine is ripserr. Please remove method from params or set engine = ripserr.")
                         }
                         if (self@engine == "ripserr" && ("location" %in% names(self@params))) {
                           sprintf("location is only used when engine is TDA. Please remove location from params or set engine = TDA.")
                         }
                         if (self@engine == "ripserr" && ("printProgress" %in% names(self@params))) {
                           sprintf("printProgress is only used when engine is TDA. Please remove printProgress from params or set engine = TDA.")
                         }
                         if (!is.null(self@params$location) && self@params$location == TRUE && !(all(c("lim","by") %in% names(self@params)))) {
                           sprintf("When location = TRUE, both lim and by must be provided in params.")
                         }
                       }
)


PH_graph <-  new_class("PH_graph", parent = PH,
                       properties = list(
                         filtration = filtration_type,
                         params = class_any),
                       validator = function(self) {
                         
                         #the warnings below are for graph object specific errors
                         if (self@filtration != "vietoris_rips" ){
                           sprintf("The only filtration avaliable for graph objects is vietoris_rips")
                         }
                         else if (self@engine == "ripserr" && !is.na(self@library)){
                           sprintf("Library is only defined when engine is TDA. Please leave library blank when using the ripserr engine.")
                         }
                         # FIXME: check if additional filtration check is needed
                         else if (self@engine == "ripserr" && (self@filtration == "alpha_complex" || self@filtration=="alpha_shape")  ){
                           sprintf("Alpha complexes are only defined for the engine TDA. Please use library TDA for any alpha filtration")
                         }
                       }
)


#TODO: decide if this is a good way to handle params
PH_raster <- function(engine = "TDA", library = NA_character_, maxdimension = NA_real_, ...) {
  PH_raster_class(
    engine = engine,
    library = library,
    maxdimension = maxdimension,
    params = list(...)
  )
}



PH_pointcloud(engine = "ripserr", filtration = "vietoris_rips", maxdimension = 5, params = list( printProgress = TRUE))
PH_raster(engine = "ripserr", library = "GUDHI", sublevel = FALSE, location = TRUE, lim = c(0, 1), by = 0.1)
