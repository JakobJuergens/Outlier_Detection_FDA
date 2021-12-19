#' This function is a convenience wrapper around largeList::saveList
#'
#'@param func_dat: list that contains the observations
#' each observation is a list, that contains two vectors of identical length: args and vals
#'@param path: path to where the file shall be saved, including a filename and the 
#' ending .llo
#'
#'@return no return value
#'@export
largeListify <- function(func_dat, path){
  tryCatch(
    {
      saveList(object = func_dat, file = path, append = FALSE, compress = FALSE)
    },
    error = function(cond){
      message('There was an Error saving the list.')
      message('Here is the original error message:')
      message(cond)
    },
    warning = function(cond){
      message('A warning was issued while saving.')
      message('Here is the original warning message:')
      message(cond)
    }
  )
  
}