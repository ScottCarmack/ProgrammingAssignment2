
# makeCacheMatrix is a function that stores a matrix and
# cached value of the inverse of the matrix
# It returns a list of functions

makeCacheMatrix <- function(x = matrix()) {
        
        # holds the cached value or returns NULL if nothing
        # is cached
        cache <- NULL
        
        # store a matrix
        setMatrix <- function(y) {
                x <<- y
                # flushes the chache
                cache <<- NULL
        }
        
        # returns the matrix
        getMatrix <- function() x
        
        # cache the argument
        setInverse <- function(solve) cache <<- solve
        
        # return the cached value
        getInverse <- function() cache
        
        # returns a list of the functions
        list(setMatrix = setMatrix ,
             getMatrix = getMatrix , 
             setInverse = setInverse , 
             getInverse = getInverse)

}


# cacheSolve calculates the inverse of the matrix created with 
# makeCacheMatrix

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        
        # get the cached value
        inverse <- x$getInverse()
        
        # if a cached value exists return it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        # otherwise get the matrix, caclulate the inverse and store it in
        # the cache
        data <- x$getMatrix()
        inverse <- solve(data)
        x$setInverse(inverse)
        
        # return the inverse
        inverse
}
