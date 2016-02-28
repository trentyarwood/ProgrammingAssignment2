## R Programming Assignment - Week 3
## Trent Yarwood
##
## mat <- matrix(c(2,0,0,2), 2,2)
## m1 <- makeCacheMatrix(mat)
## cacheSolve(m1)
##
##        [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5
##
##


makeCacheMatrix <- function(x = matrix()) {
     i <- NULL ##initialise the inverse matrix
     
     set <- function (y = matrix()) { ##set the matrix
          x <<- y
          i  <<- NULL
     }
     
     get <- function() {  #return the matrix
          x
     }
     
     setInv <- function(inv) {  ##set the inverse of the matrix
          i <<- inv
     }
     
     getInv <- function() {  ##return the inverse of the matrix
          i
     }
     list(set = set, get = get,  ##store the subfunctions as a list for retreival
          setInv = setInv,
          getInv = getInv)
}

cacheSolve <- function(x, ...) {
     m <- x$getInv() ##return the inverse of x
     if(!is.null(m)) {  ##check the cache
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...) ##calculate the inverse of the matrix
     x$setInv(m)
     m
}