# Create special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x       <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inv) inverse <<- inv
    
    getinverse <- function() inverse
    
    list(set = set, 
         get = get,
         setinverse = setinverse, 
         getinverse = getinverse)
  
}


# Compute the inverse of the special "matrix" object created by
# makeCacheMatrix. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should 
# retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    
    # here's hoping that <data> is an invertible matrix
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
