library(S7)
library(TDA)
library(ripserr)
library(phutil)


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


#works
self <- PH_pointcloud(engine = "ripserr", filtration = "vietoris_rips", max_diameter=7)

self@max_radius <- 10

#kind of works: User can only interact with max diameter
PH_pointcloud(engine = "ripserr", filtration = "vietoris_rips", max_radius=7)

#In my notes, I had writeed down that we wanted the user to specify max_dimension if they were using TDA.
#Do we need to do this if the max_dimension default is set to 1?

