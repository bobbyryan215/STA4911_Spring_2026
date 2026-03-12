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

#FIXME: should we change the default value? (causes error in griddiag)
library_type <- new_property(
  class = class_character,
  validator = function(value) {
    if (  !(value %in% c("GUDHI", "PHAT", "Dionysus", NA_character_)) )
      "must be GUDHI, PHAT, Dionysus, or NA"
  },
  default = NA_character_
)

# FIXME: error in griddiag 
maxdimension_type <- new_property(
  class = class_double,
  validator = function(value) {
    if (!is.na(value) & value < 0)
      "must be a non-negative integer"
  },
  default = NA_real_
)

# parent class (includes global definitions)
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
    sublevel = sublevel_type
    )
)

compute_persistence <- new_generic("compute_persistence", "x", function(x, data, ...) {
  S7_dispatch()
})
method(compute_persistence, PH_raster) <- function(x, data,
                                                   threshold = 9999,
                                                   method = "lj",
                                                   location = FALSE,
                                                   lim = NULL,
                                                   by = NULL,
                                                   printProgress = FALSE,
                                                   diagLimit = NULL
                                                   ) {
  if (x@engine == "ripserr") {
    if (x@sublevel) {
      cubical(
        data,
        threshold = threshold,
        method = method
      ) |> as_persistence()
    } 
    # TODO: change when ripserr handles sublevel = FALSE
    else {
      cubical(
        -data,
        threshold = threshold,
        method = method
      ) |>
        as.data.frame() |>
        transform(
          birth = -birth,
          death = -death
        ) |> as_persistence()
    }

    # TODO: when location = TRUE, both lim and by must be provided    
  } else if (x@engine == "TDA") {
    gridDiag(
      FUNvalues = data,
      maxdimension = x@maxdimension,
      sublevel = x@sublevel,
      library = x@library,
      location = location,
      lim = lim,
      by = by,
      printProgress = printProgress,
      diagLimit = diagLimit
    ) |> as_persistence()
  }
}

PH_raster(engine ="ripserr", sublevel = TRUE) |> compute_persistence(volcano, method = "lj") |> as.data.frame()
cubical(volcano) |> as_persistence() |> as.data.frame()

#FIXME: wont work without maxdimension and library defined
PH_raster(engine = "TDA", sublevel = FALSE, maxdimension = 1, library = "GUDHI") |> compute_persistence(volcano) |> as.data.frame()
gridDiag(FUNvalues = volcano, sublevel = FALSE, maxdimension = 1, library = "GUDHI") 


