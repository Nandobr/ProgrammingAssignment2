 ## This pair of functions calculate and cache the inverse of a matrix. 
 ## The cached data is returned when the inverse of the matrix was already calculated.
 ## The Solve base function of R is used to calculate the inverse of the matrix.
 
 ## Function makeCacheMatrix:
 ## This function creates a special MATRIX object that can cache its inverse.
 makeCacheMatrix <- function(x = matrix()){
       inv <- NULL                   #initialize variables x(as argument) and inv
       set <- function(y) {          #define set method and give value to parent environment
             x <<- y
             inv <<- NULL
       }
       get <- function() x           #define get method and return matrix
       setinv <- function(inverse) inv <<- inverse #define method to set inverse value
       getinv <- function() inv      #define method to get inverse value
       list(set = set, get = get,  
            setinv = setinv,
            getinv = getinv)         #return the list of methods
       
 }
 
 ## Function cacheSolve:
 ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
 ## If the inverse has already been calculated (and the matrix has not changed), then 
 ## the cachesolve should retrieve the inverse from the cache.
 cacheSolve <- function(x, ...) {
       inv <- x$getinv()             # get the value of inverse of matrix(in case it exists)
       if(!is.null(inv)) {           # check if the inverse was already calculated for this matrix
             message("getting cached data") # print a message and return cached value
             return(inv)
       }
       data <- x$get()               # get matrix 
       inv <- solve(data, ...)       # calculate the inverse of matrix using solve function
       x$setinv(inv)                 # set inverse value
       inv                     #return calculated inverse value
 }