## This function builds an object with 2 attributes, a matrix and it's inverse, 
## and 4 methods for accesing and modifying those attributes.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function receives an special object (makeCacheMatrix) and modifies its 
## inverse and returns it.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv) ## Return a matrix that is the inverse of 'x'
    } else {
        message("calculating inverse")
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        return(inv) ## Return a matrix that is the inverse of 'x'
    }
}
