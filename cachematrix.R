## Define functions that store matrix values and
## allow the inverse of matricies to be cached

## Creates a matrix wrapped in a function.
## Setting the matrix is done by calling this function
## again to create a new matrix.
## Setting the inverse is done by calling the setsolve
## function

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        get <- function() x
        getsolve <- function() s
        setsolve <- function(S) s <<- S
        list(get = get,
             getsolve = getsolve,
             setsolve = setsolve)
}


## Returns the inverse of a matrix, if applicable.
## This function would have been better named "getSolve"

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        if(is.null(s)) {
                warning("matrix was not solvable")
                return(s)
        }
        x$setsolve(s)
        s
}
