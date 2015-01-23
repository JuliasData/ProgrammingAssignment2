## pair of functions to cache the inverse of a matrix consisting of makeCacheMatrix and cacheSolve

# the function makeCacheMatrix creates a special matrix object, i.e. for a matrix x 
# a list of functions  are associated in the global environment 
# so that these can and will be used by CacheSolve as well
makeCacheMatrix<- function (x  = matrix()) {
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL 
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
# creating a list with the four functions defined above 
  list(set = set, get = get,
  setinverse = setinverse,
  getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
#checking if an inverted matrix (i.e. m) is assigned
  m <- x$getinverse()
# if yes then return this object
  if(!is.null(m)) {
    message("getting cached matrix data")
    return(m)
  }
# if not invert the matrix with build-in function solve and assign 
# i.e. put in cache and return this result
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}