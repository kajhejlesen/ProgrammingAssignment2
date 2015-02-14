## Functions to cache a matrix and its inverse

## Caches a matrix x, as well as its inverse. Outputs a list(set(), get(), setInverse(), getInverse())

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL # removing old value
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Calculates the inverse of a matrix created with makeCacheMatrix, and sets the inverse value if not
## already set. Returns the inverse of the matrix

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setInverse(inv)
    inv
}