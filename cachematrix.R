#___________________________________________________________________________________________________ 
# Programming Assignment 2

#___________________________________________________________________________________________________
# makeCacheMatrix
# makeCacheMatrix is a function that creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inv_x <<-inverse
    getinverse <- function() inv_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#___________________________________________________________________________________________________
# cacheSolve
# cacheSolve returns the inverse of the special matrix created by makeCacheMatrix.
# If the cached inverse matrix is available, cacheSolve retrieves it, if not, it computes, caches, 
# and returns it.

cacheSolve <- function(x, ...) {
    inv_x <- x$getinverse()
    if (!is.null(inv_x)) {
        message("getting cached inverse matrix")
        return(inv_x)
    } else {
        inv_x <- solve(x$get())
        x$setinverse(inv_x)
        return(inv_x)
    }
}

#___________________________________________________________________________________________________
# Example of use

a <- makeCacheMatrix(matrix(1:4,2))
cacheSolve(a)
