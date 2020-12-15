# two functions for efficient matrix inversion by result caching

# makeCacheMatrix object that is capable of caching the result of
# matrix inversion for further use

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # set value, reset inv
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # return value
  get <- function() x
  
  # set inv
  setinv <- function(temp) inv <<- temp
  # return inv
  getinv <- function() inv
  
  # return methods as list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# Returns a matrix that is the inverse of 'x'
# uses cached data if available in the makeCacheMatrix object
cacheSolve <- function(x, ...) {
  
  # fetch inv from cache
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # compute inv if no cached data
  data <- x$get()
  inv <- solve(data, ...)
  # save inv to cache
  x$setinv(inv)
  # return inv
  inv
}
