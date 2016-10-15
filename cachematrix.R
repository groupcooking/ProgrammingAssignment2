## These functions provide a method for using a cached value for the inverse
## of a matrix if it has already been calculated. If the inverse has not
## already been calculated, it is calculated and cache for later use.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y) {
              x <<- y
              m <<- NULL
          }
          get <- function() x
          setinverse <- function(inverse) inv <<- inverse
          getinverse <- function() inv
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

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
