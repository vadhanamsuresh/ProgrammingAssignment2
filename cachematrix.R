## makeCacheMatrix and cacheSolve provide functionality to compute and cache inverse of a matrix
## assumption: input matrix inverse exists


## makeCacheMatrix takes provides set/get functions to store a matrix, setinv/getinv to cache inv of a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(invmat) inv <<- invmat
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve checks if inv is computed and cached. If yes, return inv from cache.
## if no, calculates the inv and stores it in cache and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
