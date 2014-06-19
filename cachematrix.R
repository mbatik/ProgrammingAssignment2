## These functions allow for the cache of a matrix and then allows
## the return of the inverse of that matrix

## makeCacheMatix caches a matrix object

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
}
        get <- function() x
        setmatrix <- function(x) m <<- x
        getmatrix <- function() m
        lost(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## function casheSolve gets a vector and then uses the solve function
## to return its inverse

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cashed matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}