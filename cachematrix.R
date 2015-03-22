## This set of functions together first creates a special matrix object, then
## computes the inverse of the matrix object, and stores the result. 
## If the inverse of the matrix has already been previously computed, then the 
## previously calculated result it returned, saving computation time. This set 
## of functions assumes the x matrix is inversible.


## The function, makeCacheMatrix, creates a special matrix object that can 
## cache it's inverse 

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

## Computes the inverse of a special 'matrix' object created by the 
## makeCacheMatrix function, stores the solution and prints to the console. If 
## the inverse has has previously been calculated, the stored result is 
## returned instead

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
