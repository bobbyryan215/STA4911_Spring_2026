library(S7)
library(TDA)
library(ripserr)
library(phutil)


#FIXME: Do we want a larger parent class. It may be useful to that all subclasses are related. This can easily be removed
PH <- new_class("PH")


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
      "must be GUDHI, PHAT, or Dionysus"
  },
  default = NA_character_
)


maxdimension_type <- new_property(
  class = class_double,
  validator = function(value) {
    if (!is.na(value) & value <= 0)
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




params_type <- new_property(
  class = class_list,
  validator = function(value) {
      
    if (  !all(names(value) %in% c("dist","location","printProgress","data_dim","dim_lag","sample_lag","method")  ))
      "only acceptable paramaters are dist, location, printProgress, data_dim, dim_lag, sample_lag, and method"
  },
  default = list()
)



PH_pointcloud <-  new_class("PH_pointcloud", parent = PH,
                       properties = list(
                         filtration = filtration_type,
                         engine = engine_type,
                         library = library_type,
                         maxdimension = maxdimension_type,
                         maxscale = maxscale_type,
                         params = params_type),
                          validator = function(self) {
                        
                      
                          if (self@engine == "ripserr" & !is.na(self@library) ){
                           sprintf("Library is only defined when engine is TDA. Please leave library blank when using the ripserr engine.")
                         }
                         # FIXME: check if additional filtration check is needed
                         else if (self@engine == "ripserr" & (self@filtration == "alpha_complex" || self@filtration=="alpha_shape")  ){
                           sprintf("Alpha complexes are only defined for the engine TDA. Please use library TDA for any alpha filtration")
                         }
                      
                       }
)



PH_pointcloud(engine = "ripserr", filtration = "vietoris_rips", maxdimension = 5, params = list( printProgress = TRUE))
