#' placeholder
#'
#' @param path: path to the parquet object containing the Endanzug-Data
#' @param min_length: minimum length of the endanzug
#' @param max_length: maximum permitted length of the endanzug
#'
#' @return placeholder
#' @export
data_prep <- function(path,
                      min_length = 0, max_length = 200) {
  data <- read_parquet(file = path, as_tibble = TRUE) %>%
    select(!time)

  subdata <- data %>%
    select(c(prodNo, mArr, aArr, mStart, mEnd)) %>%
    filter(!is.na(mStart) & !is.na(mEnd))

  subdata$mArr <- map(
    .x = subdata$mArr,
    .f = unlist
  )

  subdata$aArr <- map(
    .x = subdata$aArr,
    .f = unlist
  )

  subdata$index_left <- unlist(
    map(
      .x = 1:nrow(subdata),
      .f = function(i) which.min(abs(subdata$mArr[[i]] - subdata$mStart[i]))
    )
  )

  subdata$index_right <- unlist(
    map(
      .x = 1:nrow(subdata),
      .f = function(i) which.min(abs(subdata$mArr[[i]] - subdata$mEnd[i]))
    )
  )

  subdata <- subdata %>%
    filter(index_left < index_right)
  
  subdata$mArr_endanzug <- map(.x = 1:nrow(subdata),
                               .f = function(i) 
                                 subdata$mArr[[i]][subdata$index_left[i]:subdata$index_right[i]])
  
  subdata$aArr_endanzug <- map(.x = 1:nrow(subdata),
                               .f = function(i) 
                                 subdata$aArr[[i]][subdata$index_left[i]:subdata$index_right[i]])
  
  subdata <- subdata %>% 
    filter(index_right - index_left > min_length &
           index_right - index_left < max_length)
  
  return(subdata)
}
