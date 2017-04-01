## This pair of functions calculate and cache the inverse of a matrix. 
## The cached data is returned when the inverse of the matrix was already calculated.
## The Solve base function of R is used to calculate the inverse of the matrix.

## Function makeCacheMatrix:
## This function creates a special MATRIX object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
      
}

## Function cacheSolve:
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}