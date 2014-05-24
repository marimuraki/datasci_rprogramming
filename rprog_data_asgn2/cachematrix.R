# JHU DS: R Programming 
# https://class.coursera.org/rprog-003

# Example:
# z <- makeCacheMatrix(x)   // create the special matrix
# z$get()                   // return matrix
# cacheSolve(z)             // return inverse
# cacheSolve(z)             // return inverse (cached)

# makeCacheMatrix creates a special “matrix” object that can cache its inverse; 
# returns a list of functions that:
# 1. set value of the matrix
# 2. get value of the matrix
#	3. set value of the inverse
# 4. get value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve computes the inverse of the special “matrix” returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # if inverse already calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if inverse not yet calculated
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
