## makeCacheMatrix and cacheSolve work together to cache the inverse of an input matrix. 
 
## makeCacheMatrix creates a special "matrix" object that can cache its inverse. We assume that the input matrix x is square and invertible. Of note, if the input matrix is reset by directly calling set, the value for the inverse is also reset to NULL.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
     set <- function(y = matrix()) {
             x <<- y
             i <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) 
             i <<- inverse
     getinverse <- function() i
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" x returned by makeCacheMatrix. If the inverse has already been calculated for x, cacheSolve gets the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("geting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        message("computed inverse")
        i
}
