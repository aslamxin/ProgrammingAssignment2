## used makeVector example as a reference
## makeCacheMatrix creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinvrs <- function(inv) invrs <<- inv
  getinvrs <- function() invrs
  list(set = set, get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)
}


## used cachemean example as a reference
## cacheSolve computes the inverse of the"matrix"
## will retrieve the inverse from the cache, if already calculated before

cacheSolve <- function(x, ...) {
  invrs <- x$getinvrs()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  mat.data <- x$get()
  invrs <- solve(mat.data, ...)
  x$setinvrs(invrs)
  invrs
}
