library(S7)
library(TDA)
library(ripserr)
library(phutil)


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
  default = NA_real_
)

maxscale_type <- new_property(
  class = class_double,
  validator = function(value) {
    if (  (!is.na(value) & value <= 0 ))
      "must be a positive real number"
  },
  default = NA_real_
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
                  maxdimension = maxdimension_type,
                  maxscale = maxscale_type),
                  validator = function(self) {
                  if (self@engine == "ripserr" & !is.na(self@library) ){
                    sprintf("Library is only defined when engine is TDA. Please leave library blank or NA_character_ when using the ripserr engine.")
                  }
                  
                }
                
)


#the following definitions of types are global in that are not filtration specific. 



params_point_cloud_type <- new_property(
  class = class_list,
  validator = function(value) {
      #TODO: Should sample_lab and dim_lag be in here and dist
    if (  !all(names(value) %in% c("dist","location","printProgress","method","p")  ))
      "only acceptable paramaters are dist, location, printProgress, p, and method"
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


PH_pointcloud <-  new_class("PH_pointcloud", parent = PH,
                       properties = list(
                         params = params_point_cloud_type),
                          validator = function(self) {
                         # FIXME: check if additional filtration check is needed
                          if (self@engine == "ripserr" & (self@filtration == "alpha_complex" || self@filtration=="alpha_shape")  ){
                           sprintf("Alpha complexes are only defined for the engine TDA using point clouds. Please use library TDA for any alpha filtration")
                          }
                          else if (self@engine == "ripserr" &  !all(names(self@params) %in% c("method","p"))){
                                   sprintf("Only acceptable params when engine is ripserr are method, and p")
                          }
                      
                       else if (self@engine == "TDA" &  !all(names(self@params) %in% c("dist","location","printProgress"))){
                                sprintf("Only acceptable params when engine is TDA are dist, location, and printProgress")
                          }
                       }
)

#TODO: I did not modify PH_raster from last time.
#TODO: see how to hande sublevel = false in ripserr
#TODO: validate params value types?
PH_raster <- new_class("PH_raster", parent = PH,
                       properties = list(
                         params = params_raster_type),
                       validator = function(self) {
                         if (self@engine == "ripserr" & !is.na(self@library)) {
                           sprintf("Library is only defined when engine is TDA. Please leave library blank when using the ripserr engine.")
                         }
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



PH_pointcloud(engine = "ripserr", filtration = "vietoris_rips", maxdimension = 5, params = list(printProgress = TRUE, p = TRUE))
PH_raster(engine = "TDA", library = "GUDHI", params = list(sublevel = FALSE))





#TODO: decide if this is a good way to handle params
#TODO: add methods for preprocessing strings. 
PH_raster <- function(engine = "TDA", library = NA_character_, maxdimension = NA_real_, ...) {
  PH_raster_class(
    engine = engine,
    library = library,
    maxdimension = maxdimension,
    params = list(...)
  )
}
