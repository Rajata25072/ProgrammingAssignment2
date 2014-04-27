## Assignment 3
## Cache for inverse matrix
# Basically, everything is 80% the same
#, just replace function from mean to find inverse matrix instead


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        # Replace function from mean to find inverse matrix
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        #If there exists result in the cache, 
        #just return the cache (no computation)
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #If there doesn't the result in the cache
        #compute it and store as m
        data <- x$get()
        m <- mean(data, ...)
        x$setInverse(m)
        m
}