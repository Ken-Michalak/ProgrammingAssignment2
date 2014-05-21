## Pair of functions for caching the inverse of a matrix

## Creates a matrix object that can be used to cache the inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInv <- function(inv) i <<- inv
    getInv <- function() i
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Gets the inverse of a matrix and caches the result
## x: a list created by makeCacheMatrix()
## ...: other parameters for solve()
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInv()
    if(!is.null(i)) {
        message("using cached inverse")
    } else {
        # calculate inverse
        m <- x$get()
        i <- solve(m, ...)
        x$setInv(i)
    }
    i
}
