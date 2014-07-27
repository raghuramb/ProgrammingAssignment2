## Two functions : 
##   1. makecacheMatrix : Returns a list of functions to 
##          a. Set the value of the Matrix 
##          b. Get the value of the Matrix 
##          c. Set the value of the inverse 
##          d. Get the value of the inverse 
##  2. cachesolve : Computes the inverse of the matrix. If the inverse is 
##                  already calculated before, it returns the cached inverse.  

## Define and prepare the list of functions 
makeCacheMatrix <- function(x = matrix()) {
    # inv will store the cached inverse matrix 
    inv <- NULL 
    #setter for the matrix 
    set <- function (y) {
        x <<- y 
        inv <<- NULL 
    }
    #Getter for the matrix 
    get <- function () x
    #Setter for the inverse 
    setInverse <<- function (inverse) inv <<- inverse 
    #getter for the inverse
    getInverse <<- function () inv
    #the list of functions is returned
    list (set=set, get=get, setInverse=setInverse,getInverse=getInverse)
}


## Compute the inverse of a matrix, if the matrix is already calculated before 
## returns the inverse from cache.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)){
        message ("Getting Cached Data")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    # Return the inverse matrix or cached inverse 
    inv
}
