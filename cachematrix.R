## --------------------------------------------------------------------------------
## COURSERA - R PROGRAMMING (rprog15)
## Assignment 2 - June 16th 2015
## --------------------------------------------------------------------------------

## Make the matrix class structure
## This function will build a 'class' which can calculate the inverse of
## a matrix. After the calculation the result will be cached in the object / list
## This object type is input to the method 'cacheSolve'
makeCacheMatrix <- function(x = matrix()) {

  sm <- NULL
  set <- function(y) {
    x <<- y
    sm <<- NULL
  }
  get <- function() x
  setsolved <- function(solve) sm <<- solve
  getsolved <- function() sm
  list(set = set, get = get,
       setsolved = setsolved,
       getsolved = getsolved)
  
}

## This methods calculate the inverse matrix
## The x-matrix should be based on the class "makeCacheMatrix" which supports
## caching of the calculation. If the result is already present in cache return
## that. If not calculate it using the solce-method
cacheSolve <- function(x, ...) {
        
  sm <- x$getsolved()
  if(!is.null(sm)) {
    message("getting cached data")
    return(sm) ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  sm <- solve(data)
  x$setsolved(sm)
  sm
  
}

## Testing
## ma <- matrix(c(1,2,3,4), nrow=2, ncol=2)