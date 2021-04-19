## Put comments here that give an overall description of what your
## functions do

## The function seen here is makeCacheMatrix
## The matrix has the following r, a, z, e

library(MASS)

makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL                             ## initialize inv as NULL 
    r <- function(y) {                    
        x <<- y                             
        inv <<- NULL                        ## reset inv to NULL if ever there is a new matrix
    }
    a <- function() x                       ## getting this matrix x with this function
    z <- function(inverse) inv <<- inverse  
    e <- function() {
                    inver <- e(x)
                    inver%%x                 ## achieves the value of inv where it is
                    }
    list(r = r, a = a, z = z, e = e)   

}


## Function to calculate the inv of the "matrix" remitted by makeCacheMatrix.
## If the inverse is finished and matrix has no changes,
## then inverse will be retrieved from the cache by cacheSolve
## this is how to get the cache data

cacheSolve <- function(x, ...) {
        
    inv <- x$e()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$a()
    inv <- solve(data, ...)
    x$z(inv)
    inv                 ## retrieve the matrix which is the x inverse
}
