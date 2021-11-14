#' placeholder
#'
#'@param func_dat: list that contains the observations
#'each observation is a list, that conatins two vectors of identical length: args and vals
#'
#'@return placeholder
#'@export
zero_observations <- function(func_dat){
  zeroed_func_dat <- map(.x = func_dat,
                         .f = function(fnc){
                           args = fnc$args - fnc$args[1]
                           return(args = args, vals = fnc$vals)
                         })
  return(zeroed_func_dat)
}