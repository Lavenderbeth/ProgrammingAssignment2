## A pair of functions that cache the inverse of a matrix

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  zinv <- NULL
  set <- function(y) {
    x <<- y
    zinv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) zinv <<- solve
  getinv <- function() zinv
  list(set = set, get = get, setinv = setinv, getinv = getinv) 
}


##This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  kinv<- x$getinv()
  if(!is.null(kinv)) {
    message("getting cached data")
    return(kinv)
  }
  data <- x$get()
  kinv<- solve(data, ...)
  x$setinv(kinv)
  kinv
  
}

