## The two functions enable to calculate the inverses of matrices fed into the cacheSolve
## functions without the need of resolving an inverse matrix if it has already been done

## This function set up the list to be present in the global environment when a matrix is
## provided to it.  Will calculate the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setMX <- function(y) {
        x <<- y
        m <<- NULL
    }
    getMX <- function() x
    setinverseMX <- function() m <<- solve(x)
    getinverseMX <- function() m
    list(setMX=setMX,getMX=getMX,setinverseMX=setinverseMX,getinverseMX=getinverseMX)
}


## This function works with the functions produced by makeCacheMatrix
## to return the inverse of a matrix x, will also return a message if 
## existing cached data is used instead of a direct calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverseMX()
    if(!is.null(m)) {
        message("getting cached data")
        return(m) 
    }
    dataMX <- x$getMX()
    m <- solve(dataMX,...)
    x$setinverseMX()
    m
}
