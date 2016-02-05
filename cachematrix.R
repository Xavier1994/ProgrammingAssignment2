## This file is about caching the caculating result of invertex a matrix

## This function is to make a specal matrix to replace normal matrix to
## support cache operation

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setIn <- function(i) {
        inv <<- i
    }
    getIn <- function() inv
    list(set = set, get = get, setIn = setIn, getIn = getIn)
}


## This function will return the invert matrix of the given matrix

cacheSolve <- function(x, ...) {
    inv <- x$getIn()
    if(!is.null(inv)){
        message("get caching data")
        inv
    }
    m <- x$get()
    inv <- solve(m,...)
    x$setIn(inv)
    inv
}
