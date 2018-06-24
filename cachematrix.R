## Matrix inversion is usually a costly computation
## It is efficient to cache the inverse matrix
## if source matrix does not change.

## Function makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
### set the value of the matrix
### get the value of the matrix
### set the value of the inverse matrix
### get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        message("recalculate inverse")
        inv <<- NULL
    }
    get <- function()
        x
    setinverse <- function(inverse)
        inv <<- inverse
    getinverse <- function()
        inv
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## Get cached inverse matrix.
## This function check inverse matrix first
## If cache is null, recalculate inverse and set it in cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
