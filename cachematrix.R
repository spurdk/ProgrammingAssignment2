## This function will build a 'class' which can calculate the inverse of
## a matrix. After the calculation the result will be cached in the object / list
## Afterwards, the object can be called an the first time the inverse will be
## calculated. Second call, the result is retrived from cache.

## Make the matrix class structure
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


## Calculate the inverse matrix - if not cached
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