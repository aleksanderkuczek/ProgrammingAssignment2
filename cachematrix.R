## This is solution to Programming Assignment 2: Lexical Scoping
## by Aleksander Kuczek

## This function creates cachable matrix and introduce getters and setters

makeCacheMatrix <- function(x = matrix()){
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function checks if there is a soultion cached before solving and either calculates and returns it or just returns a matrix that is the inverse of 'x' from cache

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
