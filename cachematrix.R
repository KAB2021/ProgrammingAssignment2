## Put comments here that give an overall description of what your
## functions do

## Our goal is to develop two functions, "makeCacheMatrix" and "cacheSolve," that efficiently cache the 
## inverse of a matrix. Caching the inverse reduces computation time by storing the previously calculated result.
## This approach improves performance when matrix inversion is a costly operation.


## Write a short comment describing this function
## `makeCacheMatrix` is a function that caches its inverse for an invertible square matrix input.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x<<- y
                inv<<- NULL
        }
        get <- function() x
        setinv <- function(solveinv) inv <<- solveinv
        getinv <- function() inv
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
## The "cacheSolve" function calculates the inverse of the "matrix" object from "makeCacheMatrix" when the matrix 
## remains unchanged, enhancing computational efficiency.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("loading cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
