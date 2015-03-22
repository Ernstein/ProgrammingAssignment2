## This set of functions together first creates a special matrix object, then
## computes the inverse matrix of the original object, and stores the result. 
## If the inverse of the matrix has already been previously computed, then the 
## previously calculated result it returned, savind computation time.


## The function, makeCacheMatrix, creates a special matrix object that can 
## cache it's inverse. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
}


## The function cacheSolve computes the inverse of a special 'matrix' object
## created by the makeCacheMatrix function, and stores the solution. If the 
## inverse has previously been calculated, then the previously computed inverse 
## matrix is returned instead of being re-computed.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m 
}
