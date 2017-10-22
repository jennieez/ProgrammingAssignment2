## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinversem <- function(inversem) im <<- inversem
  getinversem <- function() im
  list(set = set, get = get,
       setinversem = setinversem,
       getinversem = getinversem)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves
## the inverse from the cache

cacheSolve <- function(x, ...) {
  im <- x$getinversem()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  m <- x$get()
  im <- solve(m, ...)
  x$setinversem(im)
  im
}