## Put comments here that give an overall description of what your
## functions do
# These functions are written to: declare a special matrix that can cache 
# its inverse, and calculate te inverse of a matrix by verifying the presence
# of a cached inverse first, thus avoiding to perform always the full computation.
# The code is similar to the one shown in the readme file in this directory.


## Write a short comment describing this function
# The first function, `makeCacheMatrix` creates a special "matrix", which is
# really a list containing a function to
# 
# 1.  set the value of the vector
# 2.  get the value of the vector
# 3.  set the value of the mean
# 4.  get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { # y is supposed to be a matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse # no security statements written yet
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# The following function calculates the inverse of the special "marix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the mean of
# the data and sets the value of the mean in the cache via the `setinverse`
# function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
