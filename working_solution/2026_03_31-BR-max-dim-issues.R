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






#First non-solution
#setter and getter for max radius and max diameter. You would think this would work.
maxdiameter_type <- new_property(
  class = class_double,
  default = NA_real_,
  getter = function(self) {
    self@max_radius * 2  
  },
  setter = function(self, value) {
    self@max_diameter <- value
    self@max_radius <- value / 2
    self
  }
)

maxradius_type <- new_property(
  class = class_double,
  default = NA_real_,
  getter = function(self) {
    self@max_diameter / 2  
  },
  setter = function(self, value) {
    self@max_radius <- value
    self@max_diameter <- value * 2
    self
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
PH_pointcloud(max_diameter=9)
#does not work
PH_pointcloud(max_radius=9)

#works
x <- PH_pointcloud(max_diameter=9)
x@max_radius <- 1
x

#works
x <- PH_pointcloud(max_diameter=9)
x@max_diameter <- 1
x








#second non-solution
#max radius has both setter and getter
maxdiameter_type <- new_property(
  class = class_double,
  default = NA_real_
)

maxradius_type <- new_property(
  class = class_double,
  default = NA_real_,
  getter = function(self) {
    self@max_diameter / 2  
  },
  setter = function(self, value) {
    self@max_radius <- value
    self@max_diameter <- value * 2
    self
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


#does not work
PH_pointcloud(max_diameter=9)
#works
PH_pointcloud(max_radius=9)

#works
x <- PH_pointcloud(max_diameter=9)
x@max_radius <- 1
x

#works
x <- PH_pointcloud(max_diameter=9)
x@max_diameter <- 1
x












#setter in both
#does not work
maxdiameter_type <- new_property(
  class = class_double,
  default = NA_real_,
  setter = function(self, value) {
    self@max_diameter <- value
    self@max_radius <- value / 2
    self
  }
)

maxradius_type <- new_property(
  class = class_double,
  default = NA_real_,
  setter = function(self, value) {
    self@max_radius <- value
    self@max_diameter <- value * 2
    self
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
PH_pointcloud(max_diameter=9)
#does not work
PH_pointcloud(max_radius=9)

#works
x <- PH_pointcloud(max_radius=9)
x@max_radius <- 1
x

#works
x <- PH_pointcloud(max_diameter=9)
x@max_diameter <- 1
x





#other combinations I tried that did not work
#getter and setter in opposite does not work
#setter and getter in both does not work
#setter and getter in one but not the other does not work
#getter in both does not work

#there is no other place for setter and getters in the S7 package










#best solution I could find
#setter in one
#will not let the user do things that will not work
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
PH_pointcloud(max_diameter=9)
#will not let you
PH_pointcloud(max_radius=9)

#will not let you
x <- PH_pointcloud(max_diameter=9)
x@max_radius <- 1
x

#works
x <- PH_pointcloud(max_diameter=9)
x@max_diameter <- 1
x