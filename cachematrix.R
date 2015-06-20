# This function creates a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inverse) {
        i <<- inverse
    }
    getinverse <- function() {
        i
    }
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# The following function calculates the inverse of the matrix created with the 
# above function. However, it first checks to see if the inverse has already 
# been calculated. If so, it gets the inverse from the cache and skips the 
# computation. Otherwise, it calculates the inverse of the matrix and sets the 
# value of the inverse in the cache.
# Note: The matrix supplied must always be invertible
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
