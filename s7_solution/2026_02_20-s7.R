#the following definitions of types are global in that are not filtration specific. 
data_type = new_property(
  class_any,
  default = quote(stop("non-empty data is required")))

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

#FIXME: should we change the default value?
library_type <- new_property(
  class = class_character,
  validator = function(value) {
    if (  !(value %in% c("GUDHI", "PHAT", "Dionysus","")) )
      "must be GUDHI, PHAT, or Dionysus"
  },
  default = ""
)

#TODO: decide if we want to use a global maxdim argument
#FIXME: should we change the default value?
# max_dimension <- new_property(
#   class = class_numeric,
#   validator = function(value) {
#     if (length(value) != 1) 
#       return("must be a single value")
#     if (is.na(value))
#       return("cannot be NA")
#     if (value %% 1 !=0 || value < 0)
#       return("must be an integer greater or equal to 0")
#   },
#   default = 1
# )


PH_graph <-  new_class("PH_graph",
                       properties = list(
                         data = data_type,
                         filtration = filtration_type,
                         engine = engine_type,
                         library = library_type,
                         params = class_any),
                       validator = function(self) {
                         
                         #the warnings below are for graph object specific errors
                         if (self@filtration != "vietoris_rips" ){
                           sprintf("The only filtration avaliable for graph objects is vietoris_rips")
                         }
                         else if (self@engine == "ripserr" & self@library != "" ){
                           sprintf("Library is only defined when engine is TDA. Please leave library blank when using the ripserr engine.")
                         }
                         # FIXME: check if additional filtration check is needed
                         else if (self@engine == "ripserr" & (self@filtration == "alpha_complex" || self@filtration=="alpha_shape")  ){
                           sprintf("Alpha complexes are only defined for the engine TDA. Please use library TDA for any alpha filtration")
                         }
                         #FIXME: change this to allow more types of objects
                         else if ( !is_igraph(self@data) ){
                           sprintf("Objects must be inputted as igraph objects.")
                         }
                       }
)










#FIXME: Need to add an argument for when a user wants to get cycles as well
#FIXME: add global maxdimesnio argument??
PH  <- function(data, math_object, filtration, engine, library = "", ...){
  
  if (math_object == "graph" ){
    if (engine == "ripserr"){
      
      #get additional function specific arguments passed by user and check if they are acceptable based on documentation
      args_list <- list(...)
      allowed <- c("max_dim","threshold","p","dim","data_dim","dim_lag","sample_lag","method")
      bad <- setdiff(names(args_list), allowed)
      if (length(bad) > 0) {
        stop("Unknown arguments for ripserr::vietoris_rips: ", paste(bad, collapse = ", "))
      }
      
      #convert to S7 object now that we know additional arguments are correct. The class call could raise additional errors.
      PH_storage <- PH_graph(data, filtration, engine, library, args_list)
      
      #make data into dist object
      dist_w <- as.dist(distances(PH_storage@data))
      #call function that will make vietoris_rips
      do.call(vietoris_rips, c(list(dataset = dist_w), PH_storage@params)) |> as_persistence()
      
    }
    else if (engine == "TDA"){
      #get additional function specific arguments passed by user and check if they are acceptable based on documentation
      args_list <- list(...)
      allowed <- c("maxdimension","maxscale","location","printProgress", "dist")
      bad <- setdiff(names(args_list), allowed)
      
      #raise error if unknown arguments are presented
      if (length(bad) > 0) {
        stop("Unknown arguments for TDA::ripsDiag: ", paste(bad, collapse = ", "))
      }
      
      #we must set dist=arbitrary so raise error if user tries to set this not equal to dist
      if ("dist" %in% names(args_list) & args_list[["dist"]] != "arbitrary"){
        stop("For TDA::ripsDiag: you must use dist=arbitrary")
      }
      # we must remove dist= arbitrary if user adds it in because we need to ensure dist= arbitary is used even if the user does not use it
      if ("dist" %in% names(args_list) & args_list[["dist"]] == "arbitrary"){
        args_list <- args_list[- which(names(args_list)=="dist")  ]
      }
      # we must add maxdimension argument if the user does not add it
      #FIXME: How will we decide what the default maxdimension is
      if (!("maxdimension" %in% names(args_list))){
        args_list <- c(args_list, list(maxdimension = 1))
      }
      
      # we must add library argument if not included by user since it is optional
      if ("library" == "" ){
        args_list <- c(args_list, list(library = "GUDHI"))
      }
      
      
      
      
      
      #convert to S7 object now that we know additional arguments are correct. The class call could raise additional errors.
      PH_storage <- PH_graph(data, filtration, engine, library, args_list)
      
      #make data into dist object
      dist_w <- as.dist(distances(PH_storage@data))
      
      # we must add maxscale argument if the user does not add it
      #FIXME: How will we decide what the default maxscale is
      if (!("maxscale" %in% names(args_list))){
        PH_storage@params <- c(args_list, list(maxscale = 1.1*max(dist_w)))
      }
      
      #call function that will make vietoris_rips
      #FIXME: figure out a way to convert this to PHUTIL object
      do.call(ripsDiag, 
        c(list(X = dist_w, dist = "arbitrary"), PH_storage@params)) 
    }
  }
}

#this works
PH(make_ring(10), "graph","vietoris_rips","ripserr", max_dim = 1)

#this works but it wont let you convert it to a phutil object for some bizarre reason
PH(make_ring(10), "graph","vietoris_rips","TDA", "GUDHI", dist = "arbitrary")
PH(make_ring(10), "graph","vietoris_rips","TDA", "GUDHI", dist = "arbitrary") |> as_persistence()

