## The first function makeCacheMatrix creates a special "matrix" that can cache its inverse. 
## The second function cacheSolve returns the inverse of a matrix created by makeCacheMatrix, using the cached value
## if available, otherwise by calculating the inverse and caching it.

## The makeCacheMatrix function provides a list of functions to: 
##      Set the value of the matrix, 
##      Get the value of the matrix, 
##      Set the value of the inverse matrix, and 
##      Get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function returns the value of the inverse of a matrix defined via the makeCacheMatrix function.
## It first checks whether the inverse matrix has already been calculated and cached. If so it returns the cached value.
## If not it calculates the value of the inverse matrix and saves it to cache via the setinverse function on makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
