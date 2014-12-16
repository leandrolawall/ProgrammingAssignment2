##  These functions are able to save time when it is needed calculating the 
##  inverse of a matrix repeatedly, e.g. in a loop.

##  makeCacheMatrix() creates a special "matrix" object that can cache its inverse.
##  This function also establishes a set of methods(internal functions) that 
##  allow access to the internal features (functions) of this special "matrix" object.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setinverse <- function(solve) {inv <<- solve}
        getinverse <- function() {inv}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##  cacheSolve() computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix() above. If the inverse has already been calculated
##  (and the matrix has not changed), then the cachesolve will retrieve the inverse
##  from the cache.

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