## The functions are used to cache the inverse of a square invertible matrix

## The "makeCacheMatrix" function creates a square invertible matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x # to get the value of the matrix
    setinverse <- function(inverse) m <<- inverse # to cache the matrix
    getinverse <- function() m # to get the inverse of the matrix from the cache
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The "cacheSolve" function computes the inverse of a square invertible matrix.
## If the inverse is already calculated, then this function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
    # check whether the cache for the inverse exists
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
    # calculate the inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
