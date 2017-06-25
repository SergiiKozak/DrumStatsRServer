testjson <- function(){

library(jsonlite)

data <- c(1,2,3,4)

res <- toJSON(data)

print(res)

}
