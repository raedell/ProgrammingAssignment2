## These functions written in partial fulfillment of Coursera Data Science in R Programming 
## Week 3 Peer Review Assignment; week beginning April 19, 2021; GitHub user: raedell

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## This function makes a unique "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## denote the argument with default mode of "matrix"
    inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
    set <- function(y) {                    ## describes the set function to assign new 
        x <<- y                             ## value of matrix in parent environment
        inv <<- NULL                        ## reset inv to NULL if ever there is a new matrix
    }
    get <- function() x                     ## define the get function - returns value of the matrix argument

    setinverse <- function(inverse) inv <<- inverse  ## it designates the value of inv in parent environment
    getinverse <- function() inv                     ## achieves the value of inv where it is called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## this is needed to refer 
                                                                                  ## to the functions with the $ operator
}


## This function calculates the inverse of the special "matrix" remitted by the above makeCacheMatrix.
## If the inverse has been computed and the matrix had no changes,
## then the inverse will be retrieved from the cache by cacheSolve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
